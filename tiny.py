#!/usr/bin/env python3

"tiny.py"

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


class error(Exception):
    pass


EL = object()
SENTINEL = object()
T = True


class Symbol:
    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    def __repr__(self):
        return "'" + self.s


def make_symbol_table():
    t = {}

    def symbol(s):
        if s not in t:
            t[s] = Symbol(s)
        return t[s]

    return symbol


def symcheck(x):
    if x.__class__ is Symbol:
        return x
    raise TypeError(f"expected symbol, got {x!r}")


def atom(x):
    return EL if x.__class__ is list else T


def eq(x, y):
    return x is y


def listcheck(x):
    if x.__class__ is list:
        return x
    raise TypeError(f"expected pair, got {x!r}")


def make_environment(ctx, params, args, parent):
    e = { SENTINEL: parent }
    dot = ctx.symbol(".")
    variadic = False
    try:
        while params is not EL:
            p, params = params
            if p.__class__ is not Symbol:
                raise SyntaxError(f"expected param symbol, got {p!r}")
            if p is dot:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '.'")
                e[p] = args
                return e
            else:
                a, args = args
                e[p] = a
        if variadic:
            raise SyntaxError("params ends with '.'")
        if args is not EL:
            raise SyntaxError("too many args")
        return e
    except TypeError:
        if args is EL:
            raise SyntaxError("not enough args") from None
        raise SyntaxError("malformed param list") from None


P__ = {}


def primitive(name):
    def wrap(func):
        P__[name] = func
        func.special = False
        return func

    return wrap


def special(name):
    def wrap(func):
        P__[name] = func
        func.special = True
        return func

    return wrap


class Context:
    ## pylint: disable=too-many-instance-attributes

    def __init__(self):
        self.argl = self.cont = self.env = self.exp = self.val = EL
        self.s = EL
        self.symbol = make_symbol_table()
        self.g = make_environment(self, EL, EL, SENTINEL)
        self.g[self.symbol("#t")] = T
        for name, func in P__.items():
            self.g[self.symbol(name)] = func
        self.quotes = {
            "'": self.symbol("quote"),
            "`": self.symbol("quasiquote"),
            ",": self.symbol("unquote"),
            ",@": self.symbol("unquote-splicing"),
        }
        self.last = EL

    def restore(self, x):
        (
            self.argl,
            self.cont,
            self.env,
            self.exp,
            self.val,
            self.s,
        ) = x

    def save(self):
        return (
            self.argl,
            self.cont,
            self.env,
            self.exp,
            self.val,
            self.s,
        )

    class Land(Exception):
        pass

    def land(self, _):
        raise self.Land()

    def trampoline(self, func):
        try:
            while True:
                func = func(self)
        except self.Land:
            return self.val

    def unpack1(self):
        try:
            x, args = self.argl
            if args is not EL:
                raise TypeError()
            return x
        except TypeError:
            raise SyntaxError("expected one arg") from None

    def unpack2(self):
        try:
            x, args = self.argl
            y, args = args
            if args is not EL:
                raise TypeError()
            return x, y
        except TypeError:
            raise SyntaxError("expected two args") from None

    def unpack3(self):
        try:
            x, args = self.argl
            y, args = args
            z, args = args
            if args is not EL:
                raise TypeError()
            return x, y, z
        except TypeError:
            raise SyntaxError("expected three args") from None

    def leval(self, x, env=SENTINEL):
        try:
            self.exp = x
            self.env = self.g if env is SENTINEL else env
            self.cont = self.land
            return self.trampoline(k_leval)
        except:  ## pylint: disable=bare-except
            self.s = EL
            raise


def create_lambda(params, body, env):
    def closure(ctx):
        parent = ctx.env if closure.special else env
        ctx.env = make_environment(ctx, params, ctx.argl, parent)
        ctx.exp = body
        return k_leval

    closure.special = False
    closure.params = params
    closure.body = body

    return closure


def k_leval(ctx):
    ## pylint: disable=too-many-branches
    x = ctx.exp
    t = x.__class__
    if t is Symbol:
        e = ctx.env
        while e is not SENTINEL:
            try:
                ctx.val = e[x]
                return ctx.cont
            except KeyError:
                e = e[SENTINEL]
        raise NameError(str(x))

    if t is not list:
        ctx.val = x
        return ctx.cont

    op, args = x
    if op.__class__ is Symbol:
        e = ctx.env
        while e is not SENTINEL:
            try:
                op = e[op]
                break
            except KeyError:
                e = e[SENTINEL]
        else:
            raise NameError(str(op))
        try:
            if op.special:
                ctx.argl = args
                return op
        except AttributeError:
            pass

    ctx.s = [args, [ctx.env, [ctx.cont, ctx.s]]]
    try:
        _ = op.__call__
        ctx.val = op
        return k_leval_proc_done
    except AttributeError:
        pass
    if op.__class__ is not list:
        raise SyntaxError(f"expected list or proc, got {op!r}")
    ctx.cont = k_leval_proc_done
    ctx.exp = op
    return k_leval


def k_leval_proc_done(ctx):
    proc = ctx.val
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError(f"expected callable, got {proc!r}") from None
    ctx.argl, s = ctx.s
    ctx.env, s = s

    if ctx.argl is EL or proc.special:
        ctx.cont, ctx.s = s
        return proc

    s = [ctx.env, [SENTINEL, [proc, s]]]
    ctx.exp, args = ctx.argl
    if args is EL:
        ctx.cont = k_leval_last
    elif args.__class__ is list:
        s = [args, s]
        ctx.cont = k_leval_next
    else:
        raise TypeError(f"expected list, got {args!r}")
    ctx.s = s
    return k_leval


def k_leval_next(ctx):
    args, s = ctx.s
    ctx.env, s = s

    s = [ctx.env, [ctx.val, s]]
    ctx.exp, args = args
    if args is EL:
        ctx.cont = k_leval_last
    elif args.__class__ is list:
        s = [args, s]
        ctx.cont = k_leval_next
    else:
        raise TypeError(f"expected list, got {args!r}")
    ctx.s = s
    return k_leval


def k_leval_last(ctx):
    ctx.env, s = ctx.s
    args = [ctx.val, EL]
    while True:
        x, s = s
        if x is SENTINEL:
            break
        args = [x, args]
    ctx.argl = args
    proc, s = s
    ctx.cont, ctx.s = s
    return proc


class ListBuilder:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def append(self, x):
        n = [x, EL]
        if self.h is EL:
            self.h = n
        else:
            if self.t[1] is not EL:
                raise SyntaxError("extra junk after .")
            self.t[1] = n
        self.t = n

    def cons_tail(self, x):
        if self.h is EL:
            raise SyntaxError("saw . at start of list")
        if self.t[1] is not EL:
            raise SyntaxError("ambiguous use of '.'")
        self.t[1] = x

    def empty(self):
        return self.h is EL

    def get(self):
        return self.h


class Parser:
    ## pylint: disable=too-many-instance-attributes

    S_SYM = 0
    S_COMMENT = 1
    S_STRING = 2
    S_ESC = 3
    S_COMMA = 4

    def __init__(self, ctx, callback):
        self.ctx = ctx
        self.callback = callback
        self.qt = ctx.quotes    ## quotes and replacements
        self.pos = [0]  ## yup, a list, see feed() and S_COMMA code
        self.token = []
        self.add = self.token.append
        self.parens = EL  ## () and [] pairing
        self.qstack = EL  ## parser quotes
        self.lstack = EL  ## parsed lists
        self.stab = (  ## this corresponds to the S_* index constants
            self.do_sym,
            self.do_comment,
            self.do_string,
            self.do_esc,
            self.do_comma,
        )
        self.state = self.S_SYM

    def feed(self, text):
        if text is None:
            self.sym()
            if self.state not in (self.S_SYM, self.S_COMMENT):
                raise SyntaxError(f"eof in {self.state!r}")
            if self.parens is not EL:
                raise SyntaxError(f"eof expecting {self.parens[0]!r}")
            if self.qstack is not EL:
                raise SyntaxError("unclosed quasiquote")
            return
        pos = self.pos
        n = len(text)
        stab = self.stab
        p = pos[0] = 0
        while p < n:
            stab[self.state](text[p])
            p = pos[0] = pos[0] + 1  ## re-read in case of comma adjustment

    def append(self, x):
        if self.lstack is EL:
            self.callback(self.quote_wrap(x))
        else:
            self.lstack[0].append(self.quote_wrap(x))

    def quote_wrap(self, x):
        qs = self.qstack
        while qs is not EL and qs[0].__class__ is Symbol:
            q, qs = qs
            x = [q, [x, EL]]
        self.qstack = qs
        return x

    def sym(self):
        if self.token:
            t = "".join(self.token)
            self.token.clear()  ## faster than del[:]
            if t[0].lower() in "0123456789-.+abcdef":
                try:
                    t = int(t, 0)
                except ValueError:
                    try:
                        t = float(t)
                    except:  ## pylint: disable=bare-except
                        t = self.ctx.symbol(t)
            else:
                t = self.ctx.symbol(t)
            self.append(t)

    def do_sym(self, ch):
        ## pylint: disable=too-many-branches
        ## this and what follows is actually faster!
        if ch in "()[] \n\r\t;\"',`":
            if ch in "([":
                self.sym()
                ## faster than a lut:
                self.parens = [")" if ch == "(" else "]", self.parens]
                self.qstack = [SENTINEL, self.qstack]
                self.lstack = [ListBuilder(), self.lstack]
            elif ch in ")]":
                self.sym()
                if self.parens is EL:
                    raise SyntaxError(f"too many {ch!r}")
                p, self.parens = self.parens
                if p != ch:
                    raise SyntaxError(f"unexpected {ch!r}")
                self.qstack = self.qstack[1]
                lb, self.lstack = self.lstack
                self.append(lb.get())
            elif ch in " \n\r\t":
                self.sym()
            elif ch == ";":
                self.sym()
                self.state = self.S_COMMENT
            else:
                ## less common cases that aren't delimiters: ["] ['] [,] [`]
                if self.token:
                    raise SyntaxError(f"{ch!r} not a delimiter")
                if ch == '"':
                    self.state = self.S_STRING
                    return
                if ch in "'`":
                    self.qstack = [self.qt[ch], self.qstack]
                else:
                    self.state = self.S_COMMA
        else:
            self.add(ch)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == '"':
            self.append("".join(self.token))
            self.token.clear()  ## faster than del[:]
            self.state = self.S_SYM
        elif ch == "\\":
            self.state = self.S_ESC
        else:
            self.add(ch)

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def do_esc(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STRING

    def do_comma(self, ch):
        if ch == "@":
            q = self.qt[",@"]
        else:
            ## pos is a list so it can communicate
            ## with feed() without calling getattr()
            ## on self. yes, it's actually faster.
            self.pos[0] -= 1
            q = self.qt[","]
        self.qstack = [q, self.qstack]
        self.state = self.S_SYM


def parse(ctx, text, callback):
    p = Parser(ctx, callback)
    p.feed(text)
    p.feed(None)


def execute(ctx, text):
    results = []

    def callback(expr):
        results.append(ctx.leval(expr))

    parse(ctx, text, callback)
    return results


def load(ctx, filename, callback=None):
    if os.path.isabs(filename):
        path = filename
    else:
        for d in ["", os.path.dirname(__file__)] + sys.path:
            path = os.path.join(d, filename)
            if os.path.isfile(path):
                break
        else:
            raise FileNotFoundError(filename)
    with open(path, "r", encoding=locale.getpreferredencoding()) as fp:
        if callback:
            parse(ctx, fp.read(), callback)
        else:
            execute(ctx, fp.read())


def repl(ctx, callback):
    try:
        import readline as _  ## pylint: disable=import-outside-toplevel
    except ImportError:
        pass

    ## pylint: disable=unused-variable
    p, rc, stop = Parser(ctx, callback), 0, False

    def feed(x):
        nonlocal p, rc, stop
        try:
            p.feed(x)
        except SystemExit as exc:
            stop, rc = True, exc.args[0]
        except:  ## pylint: disable=bare-except
            p = Parser(ctx, callback)
            traceback.print_exception(*sys.exc_info())

    while not stop:
        try:
            line = input("lisp> ") + "\n"
        except (EOFError, KeyboardInterrupt):
            feed(None)
            break
        feed(line)
    print("\nbye")
    return rc


def main(ctx=None, force_repl=False):
    try:
        sys.set_int_max_str_digits(0)
    except AttributeError:
        pass

    if ctx is None:
        ctx = Context()

    def callback(expr):
        try:
            ctx.leval(expr)
        except SystemExit:
            raise
        except:
            print("Offender (python):", expr)
            raise

    stop = True
    for filename in sys.argv[1:]:
        if filename == "-":
            stop = False
            break
        load(ctx, filename, callback=callback)
        stop = True
    try:
        if force_repl or not stop:
            raise SystemExit(repl(ctx, callback))
    finally:
        ## debug code can go here
        # assert ctx.s is EL, ctx.s
        pass


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


@special("define")
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


@special("if")
def op_if(ctx):
    a = EL
    try:
        ctx.exp, rest = ctx.argl
        c, rest = rest
        if rest is not EL:
            a, rest = rest
        if rest is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected two or three args") from None
    ctx.s = [(c, a), [ctx.env, [ctx.cont, ctx.s]]]
    ctx.cont = k_op_if
    return k_leval


def k_op_if(ctx):
    ca, s = ctx.s
    ctx.env, s = s
    ctx.cont, ctx.s = s
    ctx.exp = ca[1] if ctx.val is EL else ca[0]
    return k_leval


@special("lambda")
def op_lambda(ctx):
    try:
        params, body = ctx.argl
        if body.__class__ is not list:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected at least 2 args") from None
    if body[1] is EL:
        body = body[0]
    else:
        body = [ctx.symbol("begin"), body]
    ctx.val = create_lambda(params, body, ctx.env)
    return ctx.cont


@special("quote")
def op_quote(ctx):
    try:
        ctx.val, a = ctx.argl
        if a is not EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected one arg") from None
    return ctx.cont


@special("set!")
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


@special("special")
def op_special(ctx):
    try:
        sym, body = ctx.argl
        if body is EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("define takes at leats 2 args") from None

    if sym.__class__ is list:
        sym, params = sym
        if body[1] is EL:
            body = body[0]
        else:
            body = [ctx.symbol("begin"), body]
        lam = create_lambda(params, body, ctx.env)
        lam.special = True
        ctx.env[symcheck(sym)] = lam
        ctx.val = EL
        return ctx.cont

    if body[1] is not EL:
        raise SyntaxError("body must be a single value")
    ctx.s = [symcheck(sym), [ctx.env, [ctx.cont, ctx.s]]]
    ctx.exp = body[0]
    ctx.cont = k_op_special
    return k_leval


def k_op_special(ctx):
    sym, s = ctx.s
    ctx.env, s = s
    ctx.cont, ctx.s = s
    proc = ctx.val
    try:
        proc.__call__
    except AttributeError:
        raise SyntaxError("expected proc") from None
    proc.special = True
    ctx.env[sym] = proc
    ctx.val = EL
    return ctx.cont


@special("trap")
def op_trap(ctx):
    x, ctx.exp = ctx.unpack2()  ## exp is the exc handler, need to eval it
    ctx.s = [x, [ctx.env, [ctx.cont, ctx.s]]]
    ctx.cont = k_op_trap
    return k_leval


def k_op_trap(ctx):
    xhandler = ctx.val
    try:
        _ = xhandler.__call__
    except AttributeError:
        raise TypeError(
            f"expected callable exception handler, got {xhandler!r}"
        ) from None
    expr, s = ctx.s
    ctx.env, s = s
    ctx.cont, ctx.s = s
    state = ctx.save()  ## ... ok, we're good to go
    ctx.s = [ctx.env, [ctx.cont, ctx.s]]  ## ops, need these back!
    try:
        res = ctx.leval(expr, ctx.env)
    except Exception as e:  ## pylint: disable=broad-except
        ctx.restore(state)  ## unwind the stack back to here
        ## py err will have a string as args[0]; (error) has an object here...
        ctx.argl = [e.args[0], [expr, EL]]
        return xhandler
    else:
        ## no exception occurred, proceed
        ctx.env, s = ctx.s
        ctx.cont, ctx.s = s
        ctx.val = res  ## restored... *now* set val
        return ctx.cont


@special("quasiquote")
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


@primitive("apply")
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


@primitive("car")
def op_car(ctx):
    return unary(ctx, lambda x: listcheck(x)[0])


@primitive("cdr")
def op_cdr(ctx):
    return unary(ctx, lambda x: EL if x is EL else listcheck(x)[1])


@primitive("cons")
def op_cons(ctx):
    return binary(ctx, lambda x, y: [x, y])


@primitive("display")
def op_display(ctx):
    x = ctx.unpack1()

    if x is EL:
        x = "()"
    elif x is T:
        x = "#t"
    elif x.__class__ is list:
        raise TypeError("display only works with atoms")
    else:
        x = str(x)
    print(x, end="")


@primitive("/")
def op_div(ctx):
    return binary(ctx, op_div_f)


def op_div_f(x, y):
    if isinstance(x, int) and isinstance(y, int):
        return x // y
    return x / y


@primitive("eq?")
def op_eq(ctx):
    return binary(ctx, lambda x, y: T if eq(x, y) else EL)


@primitive("equal?")
def op_equal(ctx):
    return binary(ctx, lambda x, y: T if x == y else EL)


@primitive("error")
def op_error(ctx):
    args = ctx.argl
    if args is EL:
        raise error("*** unspecified error ***")
    if args[1] is EL:
        raise error(args[0])
    raise error(args)


@primitive("eval")
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


@primitive("exit")
def op_exit(ctx):
    x = ctx.unpack1()
    if x.__class__ is not int:
        raise TypeError("expected int")
    raise SystemExit(x)


@primitive("<")
def op_lt(ctx):
    return binary(ctx, lambda x, y: T if x < y else EL)


@primitive("*")
def op_mul(ctx):
    return binary(ctx, lambda x, y: x * y)


@primitive("nand")
def op_nand(ctx):
    return binary(ctx, op_nand_f)


def op_nand_f(x, y):
    if not (isinstance(x, int) and isinstance(y, int)):
        raise TypeError(f"expected integers, got {x!r} and {y!r}")
    return ~(x & y)


@primitive("atom>string")
def op_stringify_atom(ctx):
    x = ctx.unpack1()
    if x.__class__ is list:
        raise SyntaxError("expected atom, got list")
    try:
        _ = x.__call__
        raise SyntaxError("expected atom, got callable")
    except AttributeError:
        pass
    ctx.val = str(x)
    return ctx.cont


@primitive("set-car!")
def op_setcar(ctx):
    return binary(ctx, op_set_car_f)


def op_set_car_f(x, y):
    listcheck(x)[0] = y


@primitive("set-cdr!")
def op_setcdr(ctx):
    return binary(ctx, op_set_cdr_f)


def op_set_cdr_f(x, y):
    listcheck(x)[1] = y


@primitive("string>number")
def op_string_to_number(ctx):
    s = ctx.unpack1()
    if s.__class__ is not str:
        raise TypeError("expected nonempty string")
    try:
        ctx.val = int(s, 0)
    except ValueError:
        try:
            ctx.val = float(s)
        except:  ## pylint: disable=bare-except
            raise SyntaxError("cannot convert to number") from None
    return ctx.cont


@primitive("string>symbol")
def op_string_to_symbol(ctx):
    s = ctx.unpack1()
    if not (s.__class__ is str and s):
        raise TypeError("expected nonempty string")
    ctx.val = ctx.symbol(s)
    return ctx.cont


@primitive("-")
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


@primitive("type")
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
        if getattr(x, "params", None):
            return ctx.symbol("lambda")
        if callable(x):
            return ctx.symbol("primitive")
        return ctx.symbol("opaque")

    return unary(ctx, f)


if __name__ == "__main__":
    main()


## EOF
