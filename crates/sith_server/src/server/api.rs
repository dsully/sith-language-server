use crate::{server::schedule::Task, session::Session};
use lsp_server as server;

mod diagnostics;
mod notifications;
mod requests;
#[cfg(test)]
mod tests;
mod traits;

use notifications as notification;
use requests as request;

use self::traits::{NotificationHandler, RequestHandler};

use super::{client::Responder, schedule::BackgroundSchedule, Result};

/// Defines the `document_url` method for implementers of [`traits::Notification`] and [`traits::Request`],
/// given the parameter type used by the implementer.
macro_rules! define_document_url {
    ($params:ident: &$p:ty) => {
        fn document_url($params: &$p) -> std::borrow::Cow<lsp_types::Url> {
            std::borrow::Cow::Borrowed(&$params.text_document.uri)
        }
    };
}

use define_document_url;

pub(super) fn request<'a>(req: server::Request) -> Task<'a> {
    let id = req.id.clone();

    match req.method.as_str() {
        request::DocumentDiagnostic::METHOD => {
            background_request_task::<request::DocumentDiagnostic>(
                req,
                BackgroundSchedule::LatencySensitive,
            )
        }
        request::GotoDefinition::METHOD => {
            background_request_task::<request::GotoDefinition>(req, BackgroundSchedule::Worker)
        }
        request::References::METHOD => {
            background_request_task::<request::References>(req, BackgroundSchedule::Worker)
        }
        request::Completion::METHOD => background_request_task::<request::Completion>(
            req,
            BackgroundSchedule::LatencySensitive,
        ),
        request::ResolveCompletionItem::METHOD => background_request_task::<
            request::ResolveCompletionItem,
        >(
            req, BackgroundSchedule::LatencySensitive
        ),
        request::Format::METHOD => {
            background_request_task::<request::Format>(req, BackgroundSchedule::Fmt)
        }
        request::DocumentHighlight::METHOD => {
            background_request_task::<request::DocumentHighlight>(
                req,
                BackgroundSchedule::LatencySensitive,
            )
        }
        request::Hover::METHOD => {
            background_request_task::<request::Hover>(req, BackgroundSchedule::Worker)
        }
        request::Rename::METHOD => {
            background_request_task::<request::Rename>(req, BackgroundSchedule::Worker)
        }
        request::PrepareRename::METHOD => {
            background_request_task::<request::PrepareRename>(req, BackgroundSchedule::Worker)
        }
        request::CodeActions::METHOD => {
            background_request_task::<request::CodeActions>(req, BackgroundSchedule::Worker)
        }
        request::CodeActionResolve::METHOD => {
            background_request_task::<request::CodeActionResolve>(req, BackgroundSchedule::Worker)
        }
        request::ExecuteCommand::METHOD => local_request_task::<request::ExecuteCommand>(req),
        request::DocumentSymbol::METHOD => {
            background_request_task::<request::DocumentSymbol>(req, BackgroundSchedule::Worker)
        }
        request::SignatureHelp::METHOD => {
            background_request_task::<request::SignatureHelp>(req, BackgroundSchedule::Worker)
        }
        method => {
            tracing::warn!("Received request {method} which does not have a handler");
            return Task::nothing();
        }
    }
    .unwrap_or_else(|err| {
        tracing::error!("Encountered error when routing request with ID {id}: {err}");
        let result: Result<()> = Err(err);
        Task::immediate(id, result)
    })
}

pub(super) fn notification<'a>(notif: server::Notification) -> Task<'a> {
    match notif.method.as_str() {
        notification::Cancel::METHOD => local_notification_task::<notification::Cancel>(notif),
        notification::DidChange::METHOD => {
            local_notification_task::<notification::DidChange>(notif)
        }
        notification::DidChangeConfiguration::METHOD => {
            local_notification_task::<notification::DidChangeConfiguration>(notif)
        }
        notification::DidChangeWorkspace::METHOD => {
            local_notification_task::<notification::DidChangeWorkspace>(notif)
        }
        notification::DidClose::METHOD => local_notification_task::<notification::DidClose>(notif),
        notification::DidOpen::METHOD => local_notification_task::<notification::DidOpen>(notif),
        notification::SetTrace::METHOD => local_notification_task::<notification::SetTrace>(notif),
        method => {
            tracing::warn!("Received notification {method} which does not have a handler.");
            return Task::nothing();
        }
    }
    .unwrap_or_else(|err| {
        tracing::error!("Encountered error when routing notification: {err}");
        Task::nothing()
    })
}

#[allow(dead_code)]
fn local_request_task<'a, R: traits::SyncRequestHandler>(
    req: server::Request,
) -> super::Result<Task<'a>> {
    let (id, params) = cast_request::<R>(req)?;
    Ok(Task::local(|session, notifier, requester, responder| {
        let result = R::run(session, notifier, requester, params);
        respond::<R>(id, result, &responder);
    }))
}

fn background_request_task<'a, R: traits::BackgroundDocumentRequestHandler>(
    req: server::Request,
    schedule: BackgroundSchedule,
) -> super::Result<Task<'a>> {
    let (id, params) = cast_request::<R>(req)?;
    Ok(Task::background(schedule, move |session: &Session| {
        // TODO: we should log an error if we can't take a snapshot.
        let Some(snapshot) = session.take_snapshot(&R::document_url(&params)) else {
            return Box::new(|_, _| {});
        };
        Box::new(move |notifier, responder| {
            let result = R::run_with_snapshot(snapshot, notifier, params);
            respond::<R>(id, result, &responder);
        })
    }))
}

fn local_notification_task<'a, N: traits::SyncNotificationHandler>(
    notif: server::Notification,
) -> super::Result<Task<'a>> {
    let (id, params) = cast_notification::<N>(notif)?;
    Ok(Task::local(move |session, notifier, _, _| {
        if let Err(err) = N::run(session, notifier, params) {
            tracing::error!("An error occurred while running {id}: {err}");
        }
    }))
}

#[allow(dead_code)]
fn background_notification_thread<'a, N: traits::BackgroundDocumentNotificationHandler>(
    req: server::Notification,
    schedule: BackgroundSchedule,
) -> super::Result<Task<'a>> {
    let (id, params) = cast_notification::<N>(req)?;
    Ok(Task::background(schedule, move |session: &Session| {
        // TODO: we should log an error if we can't take a snapshot.
        let Some(snapshot) = session.take_snapshot(&N::document_url(&params)) else {
            return Box::new(|_, _| {});
        };
        Box::new(move |notifier, _| {
            if let Err(err) = N::run_with_snapshot(snapshot, notifier, params) {
                tracing::error!("An error occurred while running {id}: {err}");
            }
        })
    }))
}

/// Tries to cast a serialized request from the server into
/// a parameter type for a specific request handler.
/// It is *highly* recommended to not override this function in your
/// implementation.
fn cast_request<Req>(
    request: server::Request,
) -> super::Result<(
    server::RequestId,
    <<Req as RequestHandler>::RequestType as lsp_types::request::Request>::Params,
)>
where
    Req: traits::RequestHandler,
{
    request
        .extract(Req::METHOD)
        .map_err(|err| match err {
            json_err @ server::ExtractError::JsonError { .. } => {
                anyhow::anyhow!("JSON parsing failure:\n{json_err}")
            }
            server::ExtractError::MethodMismatch(_) => {
                unreachable!("A method mismatch should not be possible here unless you've used a different handler (`Req`) \
                    than the one whose method name was matched against earlier.")
            }
        })
        .with_failure_code(server::ErrorCode::InternalError)
}

/// Sends back a response to the server using a [`Responder`].
fn respond<Req>(
    id: server::RequestId,
    result: crate::server::Result<
        <<Req as traits::RequestHandler>::RequestType as lsp_types::request::Request>::Result,
    >,
    responder: &Responder,
) where
    Req: traits::RequestHandler,
{
    if let Err(err) = responder.respond(id, result) {
        tracing::error!("Failed to send response: {err}");
    }
}

/// Tries to cast a serialized request from the server into
/// a parameter type for a specific request handler.
fn cast_notification<N>(
    notification: server::Notification,
) -> super::Result<
    (
        &'static str,
        <<N as traits::NotificationHandler>::NotificationType as lsp_types::notification::Notification>::Params,
)> where N: traits::NotificationHandler{
    Ok((
        N::METHOD,
        notification
            .extract(N::METHOD)
            .map_err(|err| match err {
                json_err @ server::ExtractError::JsonError { .. } => {
                    anyhow::anyhow!("JSON parsing failure:\n{json_err}")
                }
                server::ExtractError::MethodMismatch(_) => {
                    unreachable!("A method mismatch should not be possible here unless you've used a different handler (`N`) \
                        than the one whose method name was matched against earlier.")
                }
            })
            .with_failure_code(server::ErrorCode::InternalError)?,
    ))
}

pub(crate) struct Error {
    pub(crate) code: server::ErrorCode,
    pub(crate) error: anyhow::Error,
}

/// A trait to convert result types into the server result type, [`super::Result`].
trait LSPResult<T> {
    fn with_failure_code(self, code: server::ErrorCode) -> super::Result<T>;
}

impl<T, E: Into<anyhow::Error>> LSPResult<T> for core::result::Result<T, E> {
    fn with_failure_code(self, code: server::ErrorCode) -> super::Result<T> {
        self.map_err(|err| Error::new(err.into(), code))
    }
}

impl Error {
    pub(crate) fn new(err: anyhow::Error, code: server::ErrorCode) -> Self {
        Self { code, error: err }
    }
}

// Right now, we treat the error code as invisible data that won't
// be printed.
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(error_source) = self.error.source() {
            write!(f, "{}: {}", self.error, error_source)
        } else {
            write!(f, "{}", self.error)
        }
    }
}
