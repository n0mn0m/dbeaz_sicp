# scheme.py
#
# Challenge:  Can you implement a mini-scheme interpreter capable of 
# executing the following code:

def seval(sexp):
    ...

# A function definition expressed as a S-expression (in tuples)
fact = ('define', 'fact', 
        ('lambda', ('n',), ('if', ('=', 'n', 1), 1, ('*', 'n', ('fact', ('-', 'n', 1))))))

# Some test code
seval(fact)
seval(('define', 'n', 5))
result = seval(('fact', 'n'))
assert result == 120
