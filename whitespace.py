import re
from collections import namedtuple
from operator import add, sub, mul, floordiv, mod, lt, eq

Command = namedtuple('Command', ['instruction', 'params'])
Instruction = namedtuple('Instruction', ['name', 'params'])

COMMANDS = {
    'stack': {
        's':  Instruction('push', ['num']),
        'ts': Instruction('ref', ['num']),
        'tn': Instruction('slide', ['num']),
        'ns': Instruction('duplicate', []),
        'nt': Instruction('swap', []),
        'nn': Instruction('discard', [])},
    'math': {
        'ss': Instruction('infix', [add]),
        'st': Instruction('infix', [sub]),
        'sn': Instruction('infix', [mul]),
        'ts': Instruction('infix', [floordiv]),
        'tt': Instruction('infix', [mod])},
    'heap': {
        's': Instruction('store', []),
        't': Instruction('retrieve', [])},
    'io': {
        'ss': Instruction('output_char', []),
        'st': Instruction('output_num', []),
        'ts': Instruction('read_char', []),
        'tt': Instruction('read_num', [])},
    'flow': {
        'ss': Instruction('mark', ['lab']),
        'st': Instruction('call', ['lab']),
        'sn': Instruction('jump', ['lab']),
        'ts': Instruction('if_', ['lab', eq]),
        'tt': Instruction('if_', ['lab', lt]),
        'tn': Instruction('return_', []),
        'nn': Instruction('exit', [])}
    }

class Parser:
    def __init__(self, tokens):
        self.original = list(tokens)
        self.tokens = tokens

    def parse(self):
        while self.tokens:
            yield self.parse_command()

    def consume(self, expected):
        actual = self.tokens[:len(expected)]
        self.tokens = self.tokens[len(expected):]

        if actual != expected:
            raise SyntaxError('expected {} but found {} instead, in {}'.format(
                             expected, actual, ''.join(self.original)))

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
        commands =  COMMANDS[imp_type]
        for sequence, instruction in commands.items():
            if self.peek_consume(sequence):
                params = []
                for pt in instruction.params:
                    if pt == 'num':
                        params.append(self.parse_number())
                    elif pt == 'lab':
                        params.append(self.parse_label())
                    else:
                        params.append(pt)
                return Command(instruction.name, params)

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
        return sign * number

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
        self.consume('n')
        return name

    def parse_character(self):
        if self.peek_consume('t'):
            return 't'
        self.consume('s')
        return 's'

class VM:
    def __init__(self, program, input_=''):
        self.program = program

        self.input = input_
        self.output = ''

        self.stack = []
        self.heap = {}
        self.call_stack = []
        self.program_counter = 0
        self.labels = {}

        self.check_for_repeated_labels()

    def check_for_repeated_labels(self):
        labels = [params[0] for instruction, params in self.program
                  if instruction == 'mark']
        if len(set(labels)) != len(labels):
            raise RuntimeError('Repeated labels.')

    def run(self):
        while self.program_counter < len(self.program):
            command = self.program[self.program_counter]
            if command.instruction == 'exit':
                return
            self.dispatch(command)
            self.program_counter += 1

        raise RuntimeError("Unclean termination.")

    def dispatch(self, command):
        instruction, params = command
        getattr(self, instruction)(*params)

    def find_label(self, label):
        for i, (instruction, params) in enumerate(self.program):
            if instruction == 'mark' and params[0] == label:
                return i
        raise ValueError

    def push(self, n):
        self.stack.append(n)

    def duplicate(self):
        self.stack.append(self.stack[-1])

    def ref(self, n):
        if n < 0:
            raise ValueError
        self.stack.append(self.stack[-n-1])

    def discard(self):
        self.stack.pop()

    def slide(self, n):
        if n < 0 or n >= len(self.stack):
            self.stack = [self.stack.pop()]
        else:
            self.stack[-n - 1:-1] = []

    def swap(self):
        self.stack[-2:] = [self.stack[-1], self.stack[-2]]

    def infix(self, operator):
        self.stack[-2:] = [operator(self.stack[-2], self.stack[-1])]

    def store(self):
        n = self.stack.pop()
        loc = self.stack.pop()
        self.heap[loc] = n

    def retrieve(self):
        n = self.stack.pop()
        self.stack.append(self.heap[n])

    def output_char(self):
        self.output += chr(self.stack.pop())

    def output_num(self):
        self.output += str(self.stack.pop())

    def read_char(self):
        self.heap[self.stack.pop()] = ord(self.input[0])
        self.input = self.input[1:]

    def read_num(self):
        match = re.match('([0-9]+)\n', self.input)
        self.heap[self.stack.pop()] = match.group(1)
        self.input = self.input[len(match.group(0)):]

    def mark(self, label):
        pass

    def call(self, label):
        self.call_stack.append(self.program_counter)
        self.program_counter = self.find_label(label)

    def jump(self, label):
        self.program_counter = self.find_label(label)

    def if_(self, label, operator):
        if operator(self.stack.pop(), 0):
            self.jump(label)

    def return_(self):
        self.program_counter = self.call_stack.pop() 

def whitespace(code, inp=''):
    code = ''.join([t for t in code if t in ' \t\n'])
    tokens = code.replace(' ', 's').replace('\t', 't').replace('\n', 'n')
    parser = Parser(tokens)
    program = list(parser.parse())
    vm = VM(program, inp)
    vm.run()

    return vm.output

def test_output():
    assert whitespace('   \t\n\n \t \n   \t \n\n \t \n   \t\t\n\n \t \n\n\n\n\n   \n\t\n \t\n\t\n') == '123'
