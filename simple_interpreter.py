import re
from collections import namedtuple

Token = namedtuple('Token', ['type', 'value'])

TOKEN_TYPES = [
        ('fn', 'fn'),
        ('rocket', '=>'),
        ('identifier', '[A-Za-z_][A-Za-z0-9_]*'),
        ('number', '[0-9]*\.?[0-9]+'),
        ('operator', '[-+*\/\%]'),
        ('oparen', '\('),
        ('cparen', '\)'),
        ]

def tokenize(expression):
    tokens = []
    while expression:
        for token_type, regexp in TOKEN_TYPES:
            match = re.match('\s*({})\s*'.format(regexp), expression)
            if match:
                expression = expression[len(match.group(0)):]
                tokens.append(Token(token_type, match.group(1))) 
                break

    return tokens

class Interpreter:
    def __init__(self):
        self.vars = {}
        self.functions = {}

    def input(self, expression):
        tokens = tokenize(expression)

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens

    def parse(self):
        if self.peek('fn'):
            return self.parse_fn()
        else:
            return self.parse_expr()

    def consume(self, expected_type):
        actual = self.tokens.pop()
        if actual.type != expected_type:
            raise ValueError('Expected {} but got {}.'
                             .format(expected_type, actual.type))
        return actual

    def peek(self, expected_type):
        return self.tokens[0].type == expected_type

    def parse_expr(self):
        self.parse_number()
        consume('operator')
        self.parse_number()

if __name__ == "__main__":
    code = "ab + 2.1"
    tokens = tokenize(code)
    for t in tokens:
        print(t)

