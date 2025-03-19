#!/usr/bin/env python3
##
## lip - lisp in python
##       https://github.com/minmus-9/lip
## Copyright (C) 2025  Mark Hays (github:minmus-9)
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

"lisp.py -- lcore demo"

## {{{ prologue
## pylint: disable=invalid-name, too-many-lines
## XXX pylint: disable=missing-docstring

import sys

from lcore import (
    main as lmain,
    Context,
    EL,
    SENTINEL,
    Symbol,
    T,
    car,
    cdr,
    cons,
    create_continuation,
    create_lambda,
    eq,
    error,
    ffi,
    glbl,
    is_atom,
    k_leval,
    k_stringify,
    load,
    parse,
    set_car,
    set_cdr,
    spcl,
    symcheck,
)

## }}}
## {{{ helpers


def unary(ctx, f):
    try:
        x, a = ctx.argl
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected one arg") from None
    ctx.val = f(x)
    return ctx.cont


def binary(ctx, f):
    a = ctx.argl
    try:
        x, a = a
        y, a = a
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected two args") from None
    ctx.val = f(x, y)
    return ctx.cont


## }}}
## {{{ special forms


@spcl("and")
def op_and(ctx):
    args = ctx.argl
    if args is EL:
        ctx.val = EL
        return ctx.cont
    ctx.s = [ctx.env, [ctx.cont, ctx.s]]
    return op_and_setup(ctx, args)

def op_and_setup(ctx, args):
    try:
        ctx.exp, args = args
    except TypeError:
        raise SyntaxError("expected a list") from None
    if args is EL:
        ctx.env, s = ctx.s
        ctx.cont, ctx.s = s
        return k_leval
    ctx.s = [args, ctx.s]
    ctx.cont = k_op_and_next
    return k_leval

def k_op_and_next(ctx):
    args, ctx.s = ctx.s
    ctx.env = ctx.s[0]
    if ctx.val is EL:
        ctx.env, s = ctx.s
        ctx.cont, ctx.s = s
        return ctx.cont
    return op_and_setup(ctx, args)


@spcl("begin")
def op_begin(ctx):
    args = ctx.argl
    if args is EL:
        ctx.val = EL
        return ctx.cont
    try:
        ctx.exp, args = args
    except TypeError:
        raise SyntaxError("expected list") from None
    if args is not EL:
        ctx.s = [args, [ctx.env, [ctx.cont, ctx.s]]]
        ctx.cont = op_begin_next
    ## if args was EL, we merely burned up a jump
    return k_leval


def op_begin_next(ctx):
    args, s = ctx.s
    try:
        ctx.exp, args = args
    except TypeError:
        raise SyntaxError("expected list") from None
    if args is EL:
        ## i didn't understand this until watching top(1) run as
        ## my tail-recursive code chewed up ram
        ##
        ## i *thought* begin/do wanted to be a special form because
        ## the order of arg evaluation is up to the implementation.
        ## since lcore explicitly evaluates args left to right, i
        ## figured it didn't really matter. but no, not even close.
        ##
        ## THIS is why it's important that begin/do be a special
        ## form: the stack is now unwound as we evaluate the
        ## last arg so we get a tail call opporuntity. if you
        ## do the moral equivalent of
        ##          (define (do & args) (last args))
        ## it'll work fine, but you don't get tco, just recursion.
        ##
        ## which you can see with top(1) :D
        ctx.env, s = s
        ctx.cont, ctx.s = s
    else:
        ctx.env = s[0]
        ctx.s = [args, s]
        ctx.cont = op_begin_next
    return k_leval


@spcl("cond")
def op_cond(ctx):
    ctx.s = [ctx.env, [ctx.cont, ctx.s]]
    return op_cond_setup(ctx, ctx.argl)


def op_cond_setup(ctx, args):
    if args is EL:
        ctx.env, s = ctx.s
        ctx.cont, ctx.s = s
        ctx.val = EL
        return ctx.cont

    ctx.env = ctx.s[0]

    pc, args = args
    try:
        ctx.exp, c = pc
        if c.__class__ is not list:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected list, got {pc!r}") from None
    if c[1] is EL:
        c = c[0]
    else:
        c = [ctx.symbol("begin"), c]
    ctx.s = [args, [c, ctx.s]]
    ctx.cont = op_cond_next
    return k_leval


def op_cond_next(ctx):
    args, s = ctx.s
    ctx.exp, s = s
    if ctx.val is EL:
        ctx.s = s
        return op_cond_setup(ctx, args)
    ctx.env, s = s
    ctx.cont, ctx.s = s
    return k_leval


@spcl("define")
def op_define(ctx):
    try:
        sym, body = ctx.argl
        if body is EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("define takes at least 2 args") from None

    if sym.__class__ is list:
        sym, params = sym
        if sym.__class__ is not Symbol:
            raise SyntaxError("expected symbol")
        if body[1] is EL:
            body = body[0]
        else:
            body = [ctx.symbol("begin"), body]
        ctx.env[sym] = create_lambda(params, body, ctx.env)
        ctx.val = EL
        return ctx.cont

    if body[1] is not EL:
        raise SyntaxError("body must be a single value")
    if sym.__class__ is not Symbol:
        raise SyntaxError("expected symbol")
    ctx.s = [sym, [ctx.env, [ctx.cont, ctx.s]]]
    ctx.exp = body[0]
    ctx.cont = k_op_define
    return k_leval


def k_op_define(ctx):
    sym, s = ctx.s
    ctx.env, s = s
    ctx.cont, ctx.s = s
    ctx.env[sym] = ctx.val
    ctx.val = EL
    return ctx.cont


## optimized (if)


@spcl("if")
def op_if(ctx):
    try:
        ctx.exp, rest = ctx.argl
        c, rest = rest
        a, rest = rest
        if rest is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected three args") from None
    ctx.s = [(c, a), [ctx.env, [ctx.cont, ctx.s]]]
    ctx.cont = k_op_if
    return k_leval


def k_op_if(ctx):
    ca, s = ctx.s
    ctx.env, s = s
    ctx.cont, ctx.s = s
    ctx.exp = ca[1] if ctx.val is EL else ca[0]
    return k_leval


@spcl("lambda")
def op_lambda(ctx):
    try:
        params, body = ctx.argl
        if body.__class__ is not list:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected at least 2 args") from None
    if cdr(body) is EL:
        body = car(body)
    else:
        body = cons(ctx.symbol("begin"), body)
    ctx.val = create_lambda(params, body, ctx.env)
    return ctx.cont


@spcl("or")
def op_or(ctx):
    args = ctx.argl
    if args is EL:
        ctx.val = EL
        return ctx.cont
    ctx.s = [ctx.env, [ctx.cont, ctx.s]]
    return op_or_setup(ctx, args)

def op_or_setup(ctx, args):
    try:
        ctx.exp, args = args
    except TypeError:
        raise SyntaxError("expected a list") from None
    if args is EL:
        ctx.env, s = ctx.s
        ctx.cont, ctx.s = s
        return k_leval
    ctx.s = [args, ctx.s]
    ctx.cont = k_op_or_next
    return k_leval

def k_op_or_next(ctx):
    args, ctx.s = ctx.s
    ctx.env = ctx.s[0]
    if ctx.val is EL:
        return op_or_setup(ctx, args)
    ctx.env, s = ctx.s
    ctx.cont, ctx.s = s
    return ctx.cont


@spcl("quote")
def op_quote(ctx):
    ctx.val = ctx.unpack1()
    return ctx.cont


@spcl("set!")
def op_setbang(ctx):
    try:
        sym, a = ctx.argl
        value, a = a
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected two args") from None
    if sym.__class__ is not Symbol:
        raise SyntaxError("expected symbol")
    ctx.s = [ctx.env, [ctx.cont, [sym, ctx.s]]]
    ctx.cont = k_op_setbang
    ctx.exp = value
    return k_leval


def k_op_setbang(ctx):
    ctx.env, s = ctx.s
    ctx.cont, s = s
    sym, ctx.s = s
    e = ctx.env
    while e is not SENTINEL:
        if sym in e:
            e[sym] = ctx.val
            ctx.val = EL
            return ctx.cont
        e = e[SENTINEL]
    raise NameError(str(sym))


@spcl("special")
def op_special(ctx):
    try:
        sym, body = ctx.argl
        if body is EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("define takes at leats 2 args") from None

    if sym.__class__ is list:
        sym, params = sym
        if cdr(body) is EL:
            body = car(body)
        else:
            body = cons(ctx.symbol("begin"), body)
        lam = create_lambda(params, body, ctx.env)
        lam.special = True
        ctx.env[symcheck(sym)] = lam
        ctx.val = EL
        return ctx.cont

    if cdr(body) is not EL:
        raise SyntaxError("body must be a single value")
    ctx.push_ce()
    ctx.push(symcheck(sym))
    ctx.exp = car(body)
    ctx.cont = k_op_special
    return k_leval


def k_op_special(ctx):
    sym = ctx.pop()
    ctx.pop_ce()
    proc = ctx.val
    if not callable(proc):
        raise SyntaxError("expected proc")
    proc.special = True
    ctx.env[sym] = proc
    ctx.val = EL
    return ctx.cont


@spcl("trap")
def op_trap(ctx):
    x = ctx.unpack1()
    ok = T
    ctx.push_ce()
    try:
        res = ctx.leval(x, ctx.env)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    ctx.pop_ce()
    ctx.val = cons(ok, cons(res, EL))
    return ctx.cont


## }}}
## {{{ quasiquote


@spcl("quasiquote")
def op_quasiquote(ctx):
    ctx.exp = ctx.unpack1()
    return qq_


def qq_(ctx):
    form = ctx.exp
    if form.__class__ is not list:
        ctx.val = form
        return ctx.cont
    app = form[0]
    if eq(app, ctx.symbol("quasiquote")):
        ## XXX proper nesting?
        ctx.argl = form[1]
        return op_quasiquote
    if eq(app, ctx.symbol("unquote")):
        ctx.exp = form[1][0]
        return k_leval
    if eq(app, ctx.symbol("unquote-splicing")):
        _, __ = ctx.unpack2()
        raise SyntaxError("cannot use unquote-splicing here")
    ctx.s = [SENTINEL, [ctx.env, [ctx.cont, ctx.s]]]
    return k_qq_setup(ctx, form)


def k_qq_setup(ctx, form):
    elt, form = form
    if not (form.__class__ is list or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    ctx.s = [ctx.env, [ctx.cont, [form, ctx.s]]]
    if elt.__class__ is list and elt[0] is ctx.symbol("unquote-splicing"):
        ctx.exp = elt[1][0]
        ctx.cont = k_qq_spliced
        return k_leval
    ctx.cont = k_qq_next
    ctx.exp = elt
    return qq_


def k_qq_spliced(ctx):
    ctx.env, s = ctx.s
    ctx.cont, s = s
    form, s = s
    value = ctx.val
    if value is EL:
        ctx.s = s
        if form is EL:
            return k_qq_finish
        return k_qq_setup(ctx, form)
    while value is not EL:
        if value.__class__ is not list:
            raise TypeError(f"expected list, got {value!r}")
        elt, value = value
        if value is EL:
            ctx.val = elt
            ctx.s = [ctx.env, [ctx.cont, [form, s]]]
            return k_qq_next
        s = [elt, s]
    raise RuntimeError("bugs in the taters")


def k_qq_next(ctx):
    ctx.env, s = ctx.s
    ctx.cont, s = s
    form, s = s
    ctx.s = [ctx.val, s]
    if form is EL:
        return k_qq_finish
    return k_qq_setup(ctx, form)


def k_qq_finish(ctx):
    ret = EL
    s = ctx.s
    while True:
        x, s = s
        if x is SENTINEL:
            break
        ret = [x, ret]
    ctx.env, s = s
    ctx.cont, ctx.s = s
    ctx.val = ret
    return ctx.cont


## }}}
## {{{ primitives


@glbl("apply")
def op_apply(ctx):
    try:
        proc, a = ctx.argl
        ctx.argl, a = a
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected two args") from None
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError(f"expected proc, got {proc!r}") from None
    return proc


@glbl("atom?")
def op_atom(ctx):
    ## you could change op_atom_f to a lambda and save a global
    ## lookup. i like being able to look at a profile and tell
    ## what's going on without having to reference the code, even
    ## with a small performance penalty. the double function call
    ## with the unary() helper is what hurts!
    return unary(ctx, op_atom_f)


def op_atom_f(x):
    return T if is_atom(x) else EL


@glbl("call/cc")
@glbl("call-with-current-continuation")
def op_callcc(ctx):
    ## add a little sugar for speed: if called without arguments,
    ## just return a continuation; i.e.,
    ##      (define c (call/cc))
    ## is equivalent to the idiom
    ##      (define c (call/cc (lambda (cc) cc)))
    ## but 20% faster
    args = ctx.argl
    if args is EL:
        ctx.val = create_continuation(ctx)
        return ctx.cont
    ## ok, do it the "hard way"
    try:
        proc, a = args
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected one arg") from None
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError(f"expected callable, got {proc!r}") from None
    ctx.argl = cons(create_continuation(ctx), EL)
    return proc


@glbl("car")
def op_car(ctx):
    return unary(ctx, car)


@glbl("cdr")
def op_cdr(ctx):
    return unary(ctx, cdr)


@glbl("cons")
def op_cons(ctx):
    return binary(ctx, cons)


@glbl("/")
def op_div(ctx):
    return binary(ctx, op_div_f)


def op_div_f(x, y):
    if isinstance(x, int) and isinstance(y, int):
        return x // y
    return x / y


@glbl("eq?")
def op_eq(ctx):
    return binary(ctx, op_eq_f)


def op_eq_f(x, y):
    return T if eq(x, y) else EL


@glbl("equal?")
def op_equal(ctx):
    return binary(ctx, op_equal_f)


def op_equal_f(x, y):
    return T if x == y else EL


@glbl("error")
def op_error(ctx):
    raise error(ctx.unpack1())


@glbl("eval")
def op_eval(ctx):
    y = SENTINEL
    try:
        x, a = ctx.argl
        if a is not EL:
            y, a = a
            if a is not EL:
                raise TypeError()
    except TypeError:
        raise SyntaxError("expected one or two args") from None
    n_up = 0 if y is SENTINEL else y
    if x.__class__ is str:
        l = []
        parse(ctx, x, l.append)
        x = l[-1] if l else EL
    e = ctx.env
    for _ in range(n_up):
        e = e[SENTINEL]
        if e is SENTINEL:
            raise SyntaxError("no frame available")
    ctx.exp = x
    ctx.env = e
    return k_leval


@glbl("exit")
def op_exit(ctx):
    x = ctx.unpack1()
    if isinstance(x, int):
        raise SystemExit(x)
    ctx.exp = x
    ctx.cont = k_op_exit
    return k_stringify


def k_op_exit(ctx):
    raise SystemExit(ctx.val)


@glbl("<")
def op_lt(ctx):
    return binary(ctx, op_lt_f)


def op_lt_f(x, y):
    return T if x < y else EL


@glbl("*")
def op_mul(ctx):
    return binary(ctx, op_mul_f)


def op_mul_f(x, y):
    return x * y


@glbl("nand")
def op_nand(ctx):
    return binary(ctx, op_nand_f)


def op_nand_f(x, y):
    if not (isinstance(x, int) and isinstance(y, int)):
        raise TypeError(f"expected integers, got {x!r} and {y!r}")
    return ~(x & y)


@glbl("null?")
def op_null(ctx):
    x = ctx.unpack1()
    ctx.val = T if x is EL else EL
    return ctx.cont


@glbl("obj>string")
def op_stringify(ctx):
    ctx.exp = ctx.unpack1()
    return k_stringify


@glbl("print")
def op_print(ctx):
    args = ctx.argl

    if args is EL:
        print()
        ctx.val = EL
        return ctx.cont

    arg, args = args

    ctx.push(ctx.cont)
    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


def k_op_print(ctx):
    args = ctx.pop()

    if args is EL:
        print(ctx.val)
        ctx.val = EL
        return ctx.pop()

    print(ctx.val, end=" ")

    arg, args = args

    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


@glbl("range")  ## this is a prim because ffi is too slow for large lists
def op_range(ctx):
    start, stop, step = ctx.unpack3()
    ret = EL
    for i in reversed(range(start, stop, step)):
        ret = cons(i, ret)
    ctx.val = ret
    return ctx.cont


@glbl("set-car!")
def op_setcar(ctx):
    return binary(ctx, set_car)


@glbl("set-cdr!")
def op_setcdr(ctx):
    return binary(ctx, set_cdr)


@glbl("-")
def op_sub(ctx):
    try:
        x, a = ctx.argl
        if a is EL:
            x, y = 0, x
        else:
            y, a = a
            if a is not EL:
                raise TypeError()
    except TypeError:
        raise SyntaxError("expected one or two args") from None
    ctx.val = x - y
    return ctx.cont


@glbl("type")
def op_type(ctx):
    def f(x):
        ## pylint: disable=too-many-return-statements
        if x is EL:
            return ctx.symbol("()")
        if x is T:
            return ctx.symbol("#t")
        if isinstance(x, list):
            return ctx.symbol("pair")
        if isinstance(x, Symbol):
            return ctx.symbol("symbol")
        if isinstance(x, int):
            return ctx.symbol("integer")
        if isinstance(x, float):
            return ctx.symbol("float")
        if isinstance(x, str):
            return ctx.symbol("string")
        if getattr(x, "lambda_", None):
            return ctx.symbol("lambda")
        if getattr(x, "continuation", False):
            return ctx.symbol("continuation")
        if callable(x):
            return ctx.symbol("primitive")
        return ctx.symbol("opaque")

    return unary(ctx, f)


## }}}
## {{{ ffi


def module_ffi(args, module):
    if not args:
        raise TypeError("at least one arg required")
    sym = symcheck(args.pop(0))
    func = getattr(module, str(sym), SENTINEL)
    if func is SENTINEL:
        raise ValueError(f"function {sym!r} does not exist")
    if callable(func):
        return func(*args)
    if args:
        raise TypeError("not a callable object")
    return func


@ffi("math")
def op_ffi_math(args):
    import math  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, math)


@ffi("pow")
def op_ffi_pow(args):
    return pow(*args)


@ffi("random")
def op_ffi_random(args):
    import random  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, random)


@ffi("shuffle")
def op_ffi_shuffle(args):
    import random  ## pylint: disable=import-outside-toplevel

    (l,) = args
    random.shuffle(l)
    return l


@ffi("time")
def op_ffi_time(args):
    import time  ## pylint: disable=import-outside-toplevel

    def f(args):
        return [tuple(arg) if isinstance(arg, list) else arg for arg in args]

    return module_ffi(f(args), time)


## }}}


def main():
    ctx = Context()
    load(ctx, "lisp.lisp", ctx.leval)
    return lmain(ctx)


if __name__ == "__main__":
    main()


## EOF
