import re
from collections import namedtuple
from operator import add, sub, mul, truediv, mod

Token = namedtuple('Token', ['type', 'value'])
OperatorNode = namedtuple('OperatorNode', ['left', 'operator', 'right'])
AssignmentNode = namedtuple('AssignmentNode', ['name', 'expr'])
NumberNode = namedtuple('NumberNode', ['value'])
CallNode = namedtuple('CallNode', ['arg_exprs'])
NameNode = namedtuple('NameNode', ['name'])
FunctionNode = namedtuple('FunctionNode', ['name', 'arg_names', 'body'])

TOKEN_TYPES = [
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
    def __init__(self, tokens):
        self.tokens = tokens

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

    def parse_expr(self):
        factor = self.parse_factor()
        if self.peek('operator'):
            return self.parse_operator_expression(factor)
        elif not self.tokens or self.peek('cparen'):
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
            return NameNode(self.consume('identifier'))
        elif self.peek('number'):
            return NumberNode(float(self.consume('number')))
        elif self.peek('oparen'):
            return self.parse_inside_parens()

        raise ValueError("Expected a factor but didn't get one")

    def parse_assignment(self):
        name = self.consume('identifier')
        self.consume('equals')
        expr = self.parse_expr()
        return AssignmentNode(name, expr)

    def parse_inside_parens(self):
        self.consume('oparen')
        inner_expr = self.parse_expr()
        self.consume('cparen')
        return inner_expr

class Interpreter:
    def __init__(self):
        self.vars = {}

    def input(self, expression):
        tokens = tokenize(expression)
        if not tokens:
            return ''
        root = Parser(tokens).parse_expr()
        return self.evaluate(root)

    def evaluate(self, node):
        if isinstance(node, NameNode):
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

if __name__ == "__main__":
    code = "2 * (2 + 3)"
    interpreter = Interpreter()
    print(interpreter.input(code))
