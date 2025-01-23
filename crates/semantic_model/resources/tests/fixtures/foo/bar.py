from . import baz


class A:
    def __init__(self):
        self.a = 1
        self.b = 2
        self.c = 3

        def f():
            self.d = 3


class B: ...


class C: ...
