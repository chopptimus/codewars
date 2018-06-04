import ast
from operator import add, sub, mul, truediv, neg
import _ast


ops = {'Add': add,
       'Sub': sub,
       'Mult': mul,
       'Div': truediv}


def eval_expression(node):
    if isinstance(node, _ast.BinOp):
        operator = ops[node.op.__class__.__name__]
        return operator(eval_expression(node.left),
                        eval_expression(node.right))
    elif isinstance(node, _ast.UnaryOp):
        return neg(eval_expression(node.operand))
    elif isinstance(node, _ast.Num):
        return node.n
    else:
        raise RuntimeError


def calc(expression):
    parse_tree = ast.parse(expression)
    return eval_expression(parse_tree.body[0].value)


tests = [
    ["1 + 1", 2],
    ["8/16", 0.5],
    ["3 -(-1)", 4],
    ["2 + -2", 0],
    ["10- 2- -5", 13],
    ["(((10)))", 10],
    ["3 * 5", 15],
    ["-7 * -(6 / 3)", 14]
]

for s, r in tests:
    print(calc(s) == r)
