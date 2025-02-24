import { ExtensionContext, workspace } from "vscode";
import * as vscode from "vscode";

import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    RevealOutputChannelOn,
    ServerOptions,
} from "vscode-languageclient/node";
import { logger, LazyOutputChannel } from "./utils/logger";
import {
    checkIfConfigurationChanged,
    getExtensionSettings,
    getGlobalSettings,
    getWorkspaceSettings,
} from "./utils/settings";
import { findSithBinaryPath, getProjectRoot } from "./utils/fs";
import { DEV_SITH_BIN_BUILD_PATH } from "./utils/constants";
import { registerCommand } from "./utils/vscodeapi";
import { executeAutofix, executeFormat, executeOrganizeImports } from "./utils/commands";

let client: LanguageClient | undefined;
let restartInProgress = false;
let restartQueued = false;
const serverId = "sith";

export async function activate(context: ExtensionContext) {
    // Create output channels for the server and trace logs
    const outputChannel = vscode.window.createOutputChannel(`Sith Language Server`);
    const traceOutputChannel = new LazyOutputChannel(`Sith Language Server Trace`);

    // Make sure that these channels are disposed when the extension is deactivated.
    context.subscriptions.push(outputChannel);
    context.subscriptions.push(traceOutputChannel);
    context.subscriptions.push(logger.channel);

    context.subscriptions.push(
        workspace.onDidChangeConfiguration(async (e: vscode.ConfigurationChangeEvent) => {
            if (checkIfConfigurationChanged(e, serverId)) {
                await runServer();
            }
        }),
        registerCommand(`${serverId}.showServerLogs`, () => outputChannel.show()),
        registerCommand(`${serverId}.executeAutofix`, async () => {
            if (client) {
                await executeAutofix(client, serverId);
            }
        }),
        registerCommand(`${serverId}.executeFormat`, async () => {
            if (client) {
                await executeFormat(client, serverId);
            }
        }),
        registerCommand(`${serverId}.executeOrganizeImports`, async () => {
            if (client) {
                await executeOrganizeImports(client, serverId);
            }
        }),
    );

    const runServer = async () => {
        if (restartInProgress) {
            if (!restartQueued) {
                // Schedule a new restart after the current restart.
                logger.trace(
                    `Triggered SithLSP restart while restart is in progress; queuing a restart.`,
                );
                restartQueued = true;
            }
            return;
        }

        restartInProgress = true;

        try {
            if (client) {
                await stopServer();
            }

            await startServer(outputChannel, traceOutputChannel);
        } finally {
            // Ensure that we reset the flag in case of an error, early return, or success.
            restartInProgress = false;
            if (restartQueued) {
                restartQueued = false;
                await runServer();
            }
        }
    };

    await runServer();
}

export function deactivate(): Thenable<void> {
    if (client) {
        return stopServer();
    }
}

async function startServer(
    outputChannel: vscode.OutputChannel,
    traceOutputChannel: vscode.OutputChannel,
) {
    logger.info("Server started");

    const extensionSettings = getExtensionSettings(serverId);
    const globalSettings = getGlobalSettings(serverId);

    const projectRoot = await getProjectRoot();
    const workspaceSettings = getWorkspaceSettings(serverId, projectRoot);

    let command: string;
    if (process.env.DEV_MODE === "true") {
        command = DEV_SITH_BIN_BUILD_PATH;
        logger.info(`Using the SithLSP dev binary build: ${DEV_SITH_BIN_BUILD_PATH}`);
    } else {
        command = await findSithBinaryPath(workspaceSettings);
    }

    const run: Executable = {
        command,
        options: {
            env: {
                ...process.env,
                RUST_LOG: "debug",
            },
        },
    };
    const serverOptions: ServerOptions = {
        run,
        debug: run,
    };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: "file", language: "python" },
            { scheme: "untitled", language: "python" },
        ],
        outputChannel,
        traceOutputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Never,
        initializationOptions: {
            settings: extensionSettings,
            globalSettings,
        },
    };

    // Create the language client and start the client.
    client = new LanguageClient("sith-language-server", "SithLSP", serverOptions, clientOptions);
    client.start();
}

async function stopServer() {
    logger.info("Server stopped");
    await client.stop();
}
