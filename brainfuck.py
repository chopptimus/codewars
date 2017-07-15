import re

INCP, DECP, INC, DEC, OUT, GET, FOR, BAC = [
        '>', '<', '+', '-', '.', ',', '[', ']'
    ]

def tokenize(string):
    tokens = re.findall('>|<|\+|-|.|,|[|]', string)
    return tokens

class VM:
    def __init__(self, program, input_):
        self.program = program
        self.data_ptr = 0
        self.instruction_ptr = 0
        self.data = [0] * 30000
        self.output = ''
        self.input = iter(input_)
        
        self.create_bracemap()

    def create_bracemap(self):
        stack, self.bracemap = [], {}
        for i, char in enumerate(self.program):
            if char == FOR:
                stack.append(i)
            elif char == BAC:
                start = stack.pop()
                self.bracemap[start] = i
                self.bracemap[i] = start

    def execute(self):
        try:
            while True:
                instruction = self.program[self.instruction_ptr]
                self.execute_instruction(instruction)
                self.instruction_ptr += 1
        except IndexError:
            pass

        return self.output

    def execute_instruction(self, instruction):
        if instruction == INCP:
            self.data_ptr += 1
        elif instruction == DECP:
            self.data_ptr -= 1
        elif instruction == INC:
            self.data[self.data_ptr] = (self.data[self.data_ptr] + 1) % 256
        elif instruction == DEC:
            self.data[self.data_ptr] = (self.data[self.data_ptr] - 1) % 256
        elif instruction == OUT:
            self.output += chr(self.data[self.data_ptr])
        elif instruction == GET:
            self.data[self.data_ptr] = ord(next(self.input))
        elif instruction == FOR:
            if self.data[self.data_ptr] == 0:
                self.instruction_ptr = self.bracemap[self.instruction_ptr]
        elif instruction == BAC:
            if self.data[self.data_ptr] != 0:
                self.instruction_ptr = self.bracemap[self.instruction_ptr]
        else:
            awoijf

def brain_luck(code, input_):
    tokens = tokenize(code)
    vm = VM(tokens, input_)
    output = vm.execute()
    return output
