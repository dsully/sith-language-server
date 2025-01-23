import logging
import sys
import unittest.result
from _typeshed import SupportsDunderGE, SupportsDunderGT, SupportsDunderLE, SupportsDunderLT, SupportsRSub, SupportsSub
from collections.abc import Callable, Container, Iterable, Mapping, Sequence, Set as AbstractSet
from contextlib import AbstractContextManager
from re import Pattern
from types import TracebackType
from typing import Any, AnyStr, ClassVar, Generic, NamedTuple, NoReturn, Protocol, SupportsAbs, SupportsRound, TypeVar, overload
from typing_extensions import ParamSpec, Self, TypeAlias
from warnings import WarningMessage

if sys.version_info >= (3, 9):
    from types import GenericAlias

if sys.version_info >= (3, 10):
    from types import UnionType

_T = TypeVar("_T")
_S = TypeVar("_S", bound=SupportsSub[Any, Any])
_E = TypeVar("_E", bound=BaseException)
_FT = TypeVar("_FT", bound=Callable[..., Any])
_P = ParamSpec("_P")

DIFF_OMITTED: str

class _BaseTestCaseContext:
    test_case: TestCase
    def __init__(self, test_case: TestCase) -> None: ...

class _AssertRaisesBaseContext(_BaseTestCaseContext):
    expected: type[BaseException] | tuple[type[BaseException], ...]
    expected_regex: Pattern[str] | None
    obj_name: str | None
    msg: str | None

    def __init__(
        self,
        expected: type[BaseException] | tuple[type[BaseException], ...],
        test_case: TestCase,
        expected_regex: str | Pattern[str] | None = None,
    ) -> None: ...

    # This returns Self if args is the empty list, and None otherwise.
    # but it's not possible to construct an overload which expresses that
    def handle(self, name: str, args: list[Any], kwargs: dict[str, Any]) -> Any: ...

if sys.version_info >= (3, 9):
    from unittest._log import _AssertLogsContext, _LoggingWatcher
else:
    # Unused dummy for _AssertLogsContext. Starting with Python 3.10,
    # this is generic over the logging watcher, but in lower versions
    # the watcher is hard-coded.
    _L = TypeVar("_L")

    class _LoggingWatcher(NamedTuple):
        records: list[logging.LogRecord]
        output: list[str]

    class _AssertLogsContext(_BaseTestCaseContext, Generic[_L]):
        LOGGING_FORMAT: ClassVar[str]
        logger_name: str
        level: int
        msg: None
        def __init__(self, test_case: TestCase, logger_name: str, level: int) -> None: ...
        def __enter__(self) -> _LoggingWatcher: ...
        def __exit__(
            self, exc_type: type[BaseException] | None, exc_value: BaseException | None, tb: TracebackType | None
        ) -> bool | None: ...

def addModuleCleanup(function: Callable[_P, object], /, *args: _P.args, **kwargs: _P.kwargs) -> None: ...
def doModuleCleanups() -> None: ...

if sys.version_info >= (3, 11):
    def enterModuleContext(cm: AbstractContextManager[_T]) -> _T: ...

def expectedFailure(test_item: _FT) -> _FT: ...
def skip(reason: str) -> Callable[[_FT], _FT]: ...
def skipIf(condition: object, reason: str) -> Callable[[_FT], _FT]: ...
def skipUnless(condition: object, reason: str) -> Callable[[_FT], _FT]: ...

class SkipTest(Exception):
    def __init__(self, reason: str) -> None: ...

class _SupportsAbsAndDunderGE(SupportsDunderGE[Any], SupportsAbs[Any], Protocol): ...

# Keep this alias in sync with builtins._ClassInfo
# We can't import it from builtins or pytype crashes,
# due to the fact that pytype uses a custom builtins stub rather than typeshed's builtins stub
if sys.version_info >= (3, 10):
    _ClassInfo: TypeAlias = type | UnionType | tuple[_ClassInfo, ...]
else:
    _ClassInfo: TypeAlias = type | tuple[_ClassInfo, ...]

class TestCase:
    failureException: type[BaseException]
    longMessage: bool
    maxDiff: int | None
    # undocumented
    _testMethodName: str
    # undocumented
    _testMethodDoc: str
    def __init__(self, methodName: str = "runTest") -> None: ...
    def __eq__(self, other: object) -> bool: ...
    def __hash__(self) -> int: ...
    def setUp(self) -> None: ...
    def tearDown(self) -> None: ...
    @classmethod
    def setUpClass(cls) -> None: ...
    @classmethod
    def tearDownClass(cls) -> None: ...
    def run(self, result: unittest.result.TestResult | None = None) -> unittest.result.TestResult | None: ...
    def __call__(self, result: unittest.result.TestResult | None = ...) -> unittest.result.TestResult | None: ...
    def skipTest(self, reason: Any) -> NoReturn: ...
    def subTest(self, msg: Any = ..., **params: Any) -> AbstractContextManager[None]: ...
    def debug(self) -> None: ...
    if sys.version_info < (3, 11):
        def _addSkip(self, result: unittest.result.TestResult, test_case: TestCase, reason: str) -> None: ...

    def assertEqual(self, first: Any, second: Any, msg: Any = None) -> None: ...
    def assertNotEqual(self, first: Any, second: Any, msg: Any = None) -> None: ...
    def assertTrue(self, expr: Any, msg: Any = None) -> None: ...
    def assertFalse(self, expr: Any, msg: Any = None) -> None: ...
    def assertIs(self, expr1: object, expr2: object, msg: Any = None) -> None: ...
    def assertIsNot(self, expr1: object, expr2: object, msg: Any = None) -> None: ...
    def assertIsNone(self, obj: object, msg: Any = None) -> None: ...
    def assertIsNotNone(self, obj: object, msg: Any = None) -> None: ...
    def assertIn(self, member: Any, container: Iterable[Any] | Container[Any], msg: Any = None) -> None: ...
    def assertNotIn(self, member: Any, container: Iterable[Any] | Container[Any], msg: Any = None) -> None: ...
    def assertIsInstance(self, obj: object, cls: _ClassInfo, msg: Any = None) -> None: ...
    def assertNotIsInstance(self, obj: object, cls: _ClassInfo, msg: Any = None) -> None: ...
    @overload
    def assertGreater(self, a: SupportsDunderGT[_T], b: _T, msg: Any = None) -> None: ...
    @overload
    def assertGreater(self, a: _T, b: SupportsDunderLT[_T], msg: Any = None) -> None: ...
    @overload
    def assertGreaterEqual(self, a: SupportsDunderGE[_T], b: _T, msg: Any = None) -> None: ...
    @overload
    def assertGreaterEqual(self, a: _T, b: SupportsDunderLE[_T], msg: Any = None) -> None: ...
    @overload
    def assertLess(self, a: SupportsDunderLT[_T], b: _T, msg: Any = None) -> None: ...
    @overload
    def assertLess(self, a: _T, b: SupportsDunderGT[_T], msg: Any = None) -> None: ...
    @overload
    def assertLessEqual(self, a: SupportsDunderLE[_T], b: _T, msg: Any = None) -> None: ...
    @overload
    def assertLessEqual(self, a: _T, b: SupportsDunderGE[_T], msg: Any = None) -> None: ...
    # `assertRaises`, `assertRaisesRegex`, and `assertRaisesRegexp`
    # are not using `ParamSpec` intentionally,
    # because they might be used with explicitly wrong arg types to raise some error in tests.
    @overload
    def assertRaises(
        self,
        expected_exception: type[BaseException] | tuple[type[BaseException], ...],
        callable: Callable[..., object],
        *args: Any,
        **kwargs: Any,
    ) -> None: ...
    @overload
    def assertRaises(
        self, expected_exception: type[_E] | tuple[type[_E], ...], *, msg: Any = ...
    ) -> _AssertRaisesContext[_E]: ...
    @overload
    def assertRaisesRegex(
        self,
        expected_exception: type[BaseException] | tuple[type[BaseException], ...],
        expected_regex: str | Pattern[str],
        callable: Callable[..., object],
        *args: Any,
        **kwargs: Any,
    ) -> None: ...
    @overload
    def assertRaisesRegex(
        self, expected_exception: type[_E] | tuple[type[_E], ...], expected_regex: str | Pattern[str], *, msg: Any = ...
    ) -> _AssertRaisesContext[_E]: ...
    @overload
    def assertWarns(
        self,
        expected_warning: type[Warning] | tuple[type[Warning], ...],
        callable: Callable[_P, object],
        *args: _P.args,
        **kwargs: _P.kwargs,
    ) -> None: ...
    @overload
    def assertWarns(
        self, expected_warning: type[Warning] | tuple[type[Warning], ...], *, msg: Any = ...
    ) -> _AssertWarnsContext: ...
    @overload
    def assertWarnsRegex(
        self,
        expected_warning: type[Warning] | tuple[type[Warning], ...],
        expected_regex: str | Pattern[str],
        callable: Callable[_P, object],
        *args: _P.args,
        **kwargs: _P.kwargs,
    ) -> None: ...
    @overload
    def assertWarnsRegex(
        self, expected_warning: type[Warning] | tuple[type[Warning], ...], expected_regex: str | Pattern[str], *, msg: Any = ...
    ) -> _AssertWarnsContext: ...
    def assertLogs(
        self, logger: str | logging.Logger | None = None, level: int | str | None = None
    ) -> _AssertLogsContext[_LoggingWatcher]: ...
    if sys.version_info >= (3, 10):
        def assertNoLogs(
            self, logger: str | logging.Logger | None = None, level: int | str | None = None
        ) -> _AssertLogsContext[None]: ...

    @overload
    def assertAlmostEqual(self, first: _S, second: _S, places: None, msg: Any, delta: _SupportsAbsAndDunderGE) -> None: ...
    @overload
    def assertAlmostEqual(
        self, first: _S, second: _S, places: None = None, msg: Any = None, *, delta: _SupportsAbsAndDunderGE
    ) -> None: ...
    @overload
    def assertAlmostEqual(
        self,
        first: SupportsSub[_T, SupportsAbs[SupportsRound[object]]],
        second: _T,
        places: int | None = None,
        msg: Any = None,
        delta: None = None,
    ) -> None: ...
    @overload
    def assertAlmostEqual(
        self,
        first: _T,
        second: SupportsRSub[_T, SupportsAbs[SupportsRound[object]]],
        places: int | None = None,
        msg: Any = None,
        delta: None = None,
    ) -> None: ...
    @overload
    def assertNotAlmostEqual(self, first: _S, second: _S, places: None, msg: Any, delta: _SupportsAbsAndDunderGE) -> None: ...
    @overload
    def assertNotAlmostEqual(
        self, first: _S, second: _S, places: None = None, msg: Any = None, *, delta: _SupportsAbsAndDunderGE
    ) -> None: ...
    @overload
    def assertNotAlmostEqual(
        self,
        first: SupportsSub[_T, SupportsAbs[SupportsRound[object]]],
        second: _T,
        places: int | None = None,
        msg: Any = None,
        delta: None = None,
    ) -> None: ...
    @overload
    def assertNotAlmostEqual(
        self,
        first: _T,
        second: SupportsRSub[_T, SupportsAbs[SupportsRound[object]]],
        places: int | None = None,
        msg: Any = None,
        delta: None = None,
    ) -> None: ...
    def assertRegex(self, text: AnyStr, expected_regex: AnyStr | Pattern[AnyStr], msg: Any = None) -> None: ...
    def assertNotRegex(self, text: AnyStr, unexpected_regex: AnyStr | Pattern[AnyStr], msg: Any = None) -> None: ...
    def assertCountEqual(self, first: Iterable[Any], second: Iterable[Any], msg: Any = None) -> None: ...
    def addTypeEqualityFunc(self, typeobj: type[Any], function: Callable[..., None]) -> None: ...
    def assertMultiLineEqual(self, first: str, second: str, msg: Any = None) -> None: ...
    def assertSequenceEqual(
        self, seq1: Sequence[Any], seq2: Sequence[Any], msg: Any = None, seq_type: type[Sequence[Any]] | None = None
    ) -> None: ...
    def assertListEqual(self, list1: list[Any], list2: list[Any], msg: Any = None) -> None: ...
    def assertTupleEqual(self, tuple1: tuple[Any, ...], tuple2: tuple[Any, ...], msg: Any = None) -> None: ...
    def assertSetEqual(self, set1: AbstractSet[object], set2: AbstractSet[object], msg: Any = None) -> None: ...
    # assertDictEqual accepts only true dict instances. We can't use that here, since that would make
    # assertDictEqual incompatible with TypedDict.
    def assertDictEqual(self, d1: Mapping[Any, object], d2: Mapping[Any, object], msg: Any = None) -> None: ...
    def fail(self, msg: Any = None) -> NoReturn: ...
    def countTestCases(self) -> int: ...
    def defaultTestResult(self) -> unittest.result.TestResult: ...
    def id(self) -> str: ...
    def shortDescription(self) -> str | None: ...
    def addCleanup(self, function: Callable[_P, object], /, *args: _P.args, **kwargs: _P.kwargs) -> None: ...

    if sys.version_info >= (3, 11):
        def enterContext(self, cm: AbstractContextManager[_T]) -> _T: ...

    def doCleanups(self) -> None: ...
    @classmethod
    def addClassCleanup(cls, function: Callable[_P, object], /, *args: _P.args, **kwargs: _P.kwargs) -> None: ...
    @classmethod
    def doClassCleanups(cls) -> None: ...

    if sys.version_info >= (3, 11):
        @classmethod
        def enterClassContext(cls, cm: AbstractContextManager[_T]) -> _T: ...

    def _formatMessage(self, msg: str | None, standardMsg: str) -> str: ...  # undocumented
    def _getAssertEqualityFunc(self, first: Any, second: Any) -> Callable[..., None]: ...  # undocumented
    if sys.version_info < (3, 12):
        failUnlessEqual = assertEqual
        assertEquals = assertEqual
        failIfEqual = assertNotEqual
        assertNotEquals = assertNotEqual
        failUnless = assertTrue
        assert_ = assertTrue
        failIf = assertFalse
        failUnlessRaises = assertRaises
        failUnlessAlmostEqual = assertAlmostEqual
        assertAlmostEquals = assertAlmostEqual
        failIfAlmostEqual = assertNotAlmostEqual
        assertNotAlmostEquals = assertNotAlmostEqual
        assertRegexpMatches = assertRegex
        assertNotRegexpMatches = assertNotRegex
        assertRaisesRegexp = assertRaisesRegex
        def assertDictContainsSubset(
            self, subset: Mapping[Any, Any], dictionary: Mapping[Any, Any], msg: object = None
        ) -> None: ...

class FunctionTestCase(TestCase):
    def __init__(
        self,
        testFunc: Callable[[], object],
        setUp: Callable[[], object] | None = None,
        tearDown: Callable[[], object] | None = None,
        description: str | None = None,
    ) -> None: ...
    def runTest(self) -> None: ...
    def __hash__(self) -> int: ...
    def __eq__(self, other: object) -> bool: ...

class _AssertRaisesContext(_AssertRaisesBaseContext, Generic[_E]):
    exception: _E
    def __enter__(self) -> Self: ...
    def __exit__(
        self, exc_type: type[BaseException] | None, exc_value: BaseException | None, tb: TracebackType | None
    ) -> bool: ...
    if sys.version_info >= (3, 9):
        def __class_getitem__(cls, item: Any, /) -> GenericAlias: ...

class _AssertWarnsContext(_AssertRaisesBaseContext):
    warning: WarningMessage
    filename: str
    lineno: int
    warnings: list[WarningMessage]
    def __enter__(self) -> Self: ...
    def __exit__(
        self, exc_type: type[BaseException] | None, exc_value: BaseException | None, tb: TracebackType | None
    ) -> None: ...
