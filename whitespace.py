import re
from collections import namedtuple

Command = namedtuple('Command', ['instruction', 'param'])

COMMANDS = {
    'stack': {
        's':  (stack_push, 'num'),
        'ts': (stack_dup, 'num'),
        'tn': (stack_dis, None),
        'ns': (stack_dup_top, None),
        'nt': (stack_swap, None),
        'nn': (stack_dis_top, None)},
    'math': {
        'ss': (math_add, None),
        'st': (math_sub, None),
        'sn': (math_mul, None),
        'ts': (math_div, None),
        'tt': (math_mod, None)},
    'heap': {
        's': (heap_store, None)  ,
        't': (heap_store, None)} ,
    'io': {
        'ss': (io_ochar, None),
        'st': (io_onum, None),
        'ts': (io_ichar, None),
        'tt': (io_inum, None)},
    'flow': {
        'ss': (flow_mark, 'lab'),
        'st': (flow_call, 'lab'),
        'sn': (flow_jump, 'lab'),
        'ts': (flow_pop_eq, 'lab'),
        'tt': (flow_pop_lt, 'lab'),
        'tn': (flow_return, None),
        'nn': (flow_exit, None)}
    }

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens

    def parse(self):
        while self.tokens:
            yield self.parse_command()
        return commands

    def consume(self, expected):
        actual = self.tokens[:len(expected)]
        self.tokens = self.tokens[len(expected):]

        if actual != expected:
            print(self.tokens)
            raise ValueError('expected {} but found {} instead'.format(
                             expected, actual))

    def peek(self, expected):
        actual = self.tokens[:len(expected)]
        return actual == expected

    def peek_consume(self, expected):
        if self.peek(expected):
            self.consume(expected)
            return True
        return False

    def parse_command(self):
        imp_type = self.parse_imp()
        print(imp_type)
        commands =  COMMANDS[imp_type]
        for sequence, (function, param_type) in commands.items():
            if self.peek_consume(sequence):
                if param_type == 'num':
                    param = self.parse_number()
                elif param_type == 'lab':
                    param = self.parse_label() 
                else:
                    param = None
                return function, param

    def parse_imp(self):
        if self.peek_consume('s'):
            return 'stack'
        elif self.peek_consume('ts'):
            return 'math'
        elif self.peek_consume('tt'):
            return 'heap'
        elif self.peek_consume('tn'):
            return 'io'
        else:
            self.consume('n')
            return 'flow'

    def parse_number(self):
        sign = self.parse_sign()
        number = 0
        while not self.peek('n'):
            number = (number << 1) + self.parse_digit()
        self.consume('n')
        return number

    def parse_sign(self):
        if self.peek_consume('t'):
            return -1
        self.consume('s')
        return 1

    def parse_digit(self):
        if self.peek_consume('s'):
            return 0
        self.consume('t')
        return 1

    def parse_label(self):
        name = ''
        while not self.peek('n'):
            name += self.parse_character()
        consume('n')
        return name

    def parse_character(self):
        if self.peek_consume('t'):
            return 't'
        consume('s')
        return 's'

class VM:
    def __init__(self, input_=''):
        self.stack = []
        self.heap = {}
        self.output = ''
        self.call_stack = []
        self.input = input_

    def dispatch(self, (instruction, param)):
        pass

    def push(self, n):
        self.stack.append(n) 

    def duplicate(self, n):
        if n:
            self.stack.append(self.stack[-n])
        else:
            self.stack.append(self.stack[-1])

    def discard(self):
        self.stack.pop()

    def slide(self, n):
        if n < 0 or n > len(self.stack):
            self.stack[:-1] = []
        else:
            self.stack[-n - 1:-1] = []

    def swap(self):
        self.stack[-2:] = self.stack[-2::-1]
    
    def infix(self, operator):
        self.stack[-2:] = operator(self.stack[-2], self.stack[-1])

    def store(self):
        self.heap[self.stack.pop()] = self.stack.pop()

    def retrieve(self):
        self.stack.append(heap[self.stack.pop()])

    def output_char(self):
        self.output += chr(self.stack.pop())

    def output_num(self):
        self.output += ord(self.stack.pop())

    def read_char(self):
        heap[self.stack.pop()] = chr(self.input[0])
        self.input = self.input[1:]

    def read_num(self):
        heap[self.stack.pop()] = self.input[0]
        self.input = self.input[1:]

    def label(self, label):
        pass

def whitespace(code, inp=''):
    output = ''
    stack = []
    heap = {}

    tokens = code.replace(' ', 's').replace('\t', 't').replace('\n', 'n')
    parser = Parser(tokens)
    for command, param in parser.parse():
        print(command)

    return output

code = "   \t\n\t\n \t\n\n\n"
whitespace(code)
