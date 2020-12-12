# scheme.py
#
# Challenge: Can you implement a mini-scheme interpreter capable of
# executing the following code:
#
# Three things:
#  (1) Primitives (numbers)
#  (2) Symbols (strings)
#  (3) Tuples (list)
#


class Procedure:
    def __init__(self, argnames, expr, env):
        self.argnames = argnames
        self.expr = expr
        self.env = env

    def __call__(self, *argvals):
        print("Calling:", argvals)
        expr = self.expr
        for name, val in zip(self.argnames, argvals):
            expr = substitute(expr, name, val)
        return seval(expr, dict(self.env))


def substitute(sexp, name, value):
    if sexp == name:
        return value
    elif isinstance(sexp, tuple):
        return tuple(substitute(item, name, value) for item in sexp)
    else:
        return sexp


def seval(sexp, env):
    # Primitives self-evaluate
    if isinstance(sexp, (int, float, bool, Procedure)):
        return sexp
    # Symbols ...
    elif isinstance(sexp, str):
        # Name lookup in some environment holding names
        return env[sexp]
    # Tuples, call a procedure
    elif isinstance(sexp, tuple):
        # Handle special forms first
        # (define name value)
        if sexp[0] == "define":
            name = sexp[1]
            value = seval(sexp[2], env)
            env[name] = value  # save variable
            return
        # if statement
        elif sexp[0] == "if":
            test, consequence, alternative = sexp[1:]
            if seval(test, env):
                return seval(consequence, env)
            else:
                return seval(alternative, env)
        # lambda (args) expr)
        elif sexp[0] == "lambda":
            # Creates a procedure
            return Procedure(sexp[1], sexp[2], env)
        else:
            proc = seval(sexp[0], env)  # Lookup the procedure by name
            args = [seval(arg, env) for arg in sexp[1:]]
            return proc(*args)


# A function definition expressed as an S-expression (in tuples)
fact = (
    "define",
    "fact",
    ("lambda", ("n",), ("if", ("=", "n", 1), 1, ("*", "n", ("face", ("-", "n", 1))))),
)

# Some test code

# Prime our environment.
env = {
    "+": lambda x, y: x + y,
    "-": lambda x, y: x - y,
    "*": lambda x, y: x * y,
    "/": lambda x, y: x / y,
    "%": lambda x, y: x % y,
    "<": lambda x, y: x < y,
    ">": lambda x, y: x > y,
    "=": lambda x, y: x == y,
}

r = seval(("+", 2, 3), env)
print(r)

seval(("define", "x", ("+", 10, 20)), env)
x = seval(("+", "x", 200), env)
print(x)

b = seval(("if", ("<", 2, 3), 10, 20), env)
print(b)

seval(fact, env)
seval(('define', 'n', 5), env)
l = seval((fact, 'n'), env)
print(l)
