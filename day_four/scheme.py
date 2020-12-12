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


# Prime our environment.
glenv = {
    "+": lambda x, y: x + y,
    "-": lambda x, y: x - y,
    "*": lambda x, y: x * y,
    "/": lambda x, y: x / y,
    "%": lambda x, y: x % y,
    "<": lambda x, y: x < y,
    ">": lambda x, y: x > y,
    "=": lambda x, y: x == y,
}

class Procedure:
    def __init__(self, argnames, expr, env):
        self.argnames = argnames
        self.expr = expr
        self.env = env

    def __call__(self, *argvals):
        print("Calling:", argvals)
        # 1. create a new environnent for execution (stack frame idea)
        newenv = {}
        # 2. new environment must link to the definition environment
        newenv["__parent__"] = self.env

        for name, val in zip(self.argnames, argvals):
            # 3. Bind argument names to values in the new environment
            # 4. Bind the names to the values in the new environment
            newenv[name] = val
            pass
        # Evaluate the procedure in the new environment
        return seval(self.expr, newenv)


def seval(sexp, env):
    # Primitives self-evaluate
    if isinstance(sexp, (int, float, bool, Procedure)):
        return sexp
    # Symbols ...
    elif isinstance(sexp, str):
        # Name lookup in some environment holding names
        while env:
            if sexp in env:
                return env[sexp]
            env = env["__parent__"]
        raise KeyError("Unknown Name")
    # Tuples, call a procedure
    elif isinstance(sexp, tuple):
        # Handle special forms first
        # (define name value)
        if sexp[0] == "define":
            name = sexp[1]
            value = seval(sexp[2], env)
            env[name] = value  # save variable
        if sexp[0] == "set!":
            name = sexp[1]
            value = seval(sexp[2], env)
            while env:
                if name in env:
                    env[name] = value
                    return
                else:
                    env = env["__parent__"]
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
    ("lambda", ("n",), ("if", ("=", "n", 1), 1, ("*", "n", ("fact", ("-", "n", 1))))),
)

# Some test code

s = seval(('set!', 'x', 4), glenv)
print(s)
print(seval("x", glenv))

l = seval(("devine", "f", ("lambda", ("x", "y"), ("lambda", (), ("+", "x", "y")))), glenv)
print(l)

r = seval(("+", 2, 3), glenv)
print(r)

seval(("define", "x", ("+", 10, 20)), glenv)
x = seval(("+", "x", 200), glenv)
print(x)

b = seval(("if", ("<", 2, 3), 10, 20), glenv)
print(b)

# seval(fact, env)
# seval(("define", "n", 5), env)
# l = seval((fact, "n"), env)
# print(l)
