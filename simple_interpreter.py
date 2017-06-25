import re
from collections import namedtuple
from operator import add, sub, mul, truediv, mod

Token = namedtuple('Token', ['type', 'value'])
OperatorNode = namedtuple('OperatorNode', ['left', 'operator', 'right'])
AssignmentNode = namedtuple('AssignmentNode', ['name', 'expr'])
NumberNode = namedtuple('NumberNode', ['value'])
CallNode = namedtuple('CallNode', ['name', 'arg_exprs'])
VarNode = namedtuple('VarNode', ['name'])
FunctionNode = namedtuple('FunctionNode', ['name', 'arg_names', 'body'])

TOKEN_TYPES = [
        ('fn', r'\bfn\b'),
        ('rocket', r'=>'),
        ('identifier', r'\b[A-Za-z_][A-Za-z0-9_]*\b'),
        ('number', r'\b[0-9]*\.?[0-9]+\b'),
        ('operator', r'[-+*\/\%]'),
        ('equals', r'='),
        ('oparen', r'\('),
        ('cparen', r'\)'),
        ]

def tokenize(expression):
    tokens = []
    expression = expression.strip()
    while expression:
        match = None
        for token_type, regexp in TOKEN_TYPES:
            match = re.match(r'\s*({})\s*'.format(regexp), expression)
            if match:
                expression = expression[len(match.group(0)):]
                tokens.append(Token(token_type, match.group(1))) 
                break
        if not match:
            raise ValueError

    return tokens

class Parser:
    def __init__(self):
        self.vars = set()
        self.signatures = {}

    def consume(self, expected_type):
        actual = self.tokens[0]
        self.tokens = self.tokens[1:]
        if actual.type != expected_type:
            raise ValueError('Expected {} but got {}.'
                             .format(expected_type, actual.type))
        return actual.value

    def peek(self, expected_type, index=0):
        try:
            return self.tokens[index].type == expected_type
        except IndexError:
            return False

    def parse(self, tokens):
        self.tokens = tokens
        self.parsing_call = 0

        if self.peek('fn'):
            return self.parse_fn()
        else:
            return self.parse_expr()

    def parse_fn(self):
        self.consume('fn')
        name = self.consume('identifier')
        if name in self.vars:
            raise ValueError
        arg_names = self.parse_arg_names()
        self.consume('rocket')
        
        frozen_vars = self.vars
        self.vars = set(arg_names)

        body = self.parse_expr()

        self.vars = frozen_vars
        self.signatures[name] = len(arg_names)

        return FunctionNode(name, arg_names, body)

    def parse_arg_names(self):
        arg_names = []
        if self.peek('identifier'):
            arg_names = [self.consume('identifier')]
            while not self.peek('rocket'):
                arg_names.append(self.consume('identifier'))

        if len(arg_names) != len(set(arg_names)):
            raise ValueError

        return arg_names

    def parse_expr(self):
        factor = self.parse_factor()
        if self.peek('operator'):
            return self.parse_operator_expression(factor)
        elif not self.tokens or self.peek('cparen') or self.parsing_call:
            return factor

        raise ValueError

    def parse_operator_expression(self, left):
        operator = self.consume('operator')

        if self.peek('oparen'):
            right = self.parse_expr()
            return OperatorNode(left, operator, right)
        else:
            right = self.parse_expr()
            if isinstance(right, OperatorNode):
                return self.fix_precedence(left, operator, right)
            else:
                return OperatorNode(left, operator, right)

    def fix_precedence(self, left_value, left_op, op_node):
        middle_value, right_op, right_value = op_node
        if left_op in {'-', '+'} and right_op in {'*', '/', '%'}:
            return OperatorNode(left_value, left_op, op_node)
        else:
            new_op_node = OperatorNode(left_value, left_op, middle_value)
            return OperatorNode(new_op_node, right_op, right_value)

    def parse_factor(self):
        if self.peek('equals', 1):
            return self.parse_assignment()
        elif self.peek('identifier'):
            return self.parse_identifier()
        elif self.peek('number'):
            return NumberNode(float(self.consume('number')))
        elif self.peek('oparen'):
            return self.parse_inside_parens()

        raise ValueError("Expected a factor but didn't get one")

    def parse_identifier(self):
        name = self.consume('identifier')
        if name in self.signatures and name not in self.vars:
            return self.parse_call(name)
        elif name in self.vars and name not in self.functions:
            return VarNode(name)
        else:
            raise ValueError

    def parse_assignment(self):
        name = self.consume('identifier')
        if name in self.signatures:
            raise ValueError
        self.consume('equals')
        expr = self.parse_expr()
        self.vars.add(name)
        return AssignmentNode(name, expr)

    def parse_call(self, name):
        n = self.signatures[name]
        self.parsing_call += 1
        arg_exprs = [self.parse_expr() for _ in range(n)]
        self.parsing_call -= 1
        return CallNode(name, arg_exprs)

    def parse_inside_parens(self):
        self.consume('oparen')
        inner_expr = self.parse_expr()
        self.consume('cparen')
        return inner_expr

class Interpreter:
    def __init__(self):
        self.vars = {}
        self.functions = {}
        self.parser = Parser()

    def input(self, expression):
        tokens = tokenize(expression)
        if not tokens:
            return ''
        root = self.parser.parse(tokens)
        return self.evaluate(root)

    def evaluate(self, node):
        if isinstance(node, VarNode):
            return self.vars[node.name]
        elif isinstance(node, NumberNode):
            return node.value
        elif isinstance(node, AssignmentNode):
            evaluation = self.evaluate(node.expr)
            self.vars[node.name] = evaluation
            return evaluation
        elif isinstance(node, OperatorNode):
            f = {'+': add,
                 '-': sub,
                 '*': mul,
                 '/': truediv,
                 '%': mod
                 }[node.operator]
            return f(self.evaluate(node.left), self.evaluate(node.right))
        elif isinstance(node, FunctionNode):
            self.functions[node.name] = node
            return ''
        elif isinstance(node, CallNode):
            return self.evaluate_call(node)
        else:
            raise RuntimeError

    def evaluate_call(self, node):
        if node.name not in self.functions:
            raise RuntimeError

        function = self.functions[node.name]

        frozen_vars = self.vars
        self.vars = {}

        arg_values = [self.evaluate(expr) for expr in node.arg_exprs]

        for name, value in zip(function.arg_names, arg_values):
            self.vars[name] = value

        call_value = self.evaluate(function.body)
        self.vars = frozen_vars

        return call_value

import pytest

def test_too_many_args():
    interpreter = Interpreter()
    interpreter.input('fn avg x y => (x + y) / 2')
    with pytest.raises(ValueError):
        interpreter.input('avg 1 2 3')

def test_function_call():
    interpreter = Interpreter()
    interpreter.input('fn avg x y => (x + y) / 2')
    interpreter.input('fn add x y => x + y')
    interpreter.input('fn echo x => x')
    assert interpreter.input('avg echo add 1 2 add 3 4') == 5
