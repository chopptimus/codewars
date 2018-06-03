from operator import or_
from functools import reduce

def ranks(i, j):
    return reduce(or_, [1023 << (10 * i) for i in range(max(0, i), min(10, j))])

def files(i, j):
    mask = 1239150146850664126585242625
    return reduce(or_, [mask << i for i in range(max(0, i), min(10, j))])

def _mask_pairs(n):
    for i in range(10):
        for j in range(11 - n):
            yield (1 << n) - 1 << j + 10*i, ranks(i-1, i+2) & files(j-1, n+j+1)

def validateBattlefield(field):
    horizontal = sum(1 << i for i in range(100) if field[i//10][i%10])
    vertical = sum(1 << i for i in range(100) if field[i%10][i//10])
    for n in range(4):
        count = 0
        for p, s in _mask_pairs(n+1):
            if p & horizontal == p and s & horizontal == p:
                count += 1
            elif n > 0 and p & vertical == p and s & vertical == p:
                count += 1
        if count != 4 - n:
            return False

    return True

class BitField:
    def __init__(self, bf):
        self.bf = bf

    def __repr__(self):
        rows = []
        for i in range(10):
            row = ''
            for j in range(20):
                if not j % 2:
                    row += '@' if self.bf & 1 << i*10 + j//2 else '.'
                else:
                    row += ' '
            rows.append(row)
        return '\n'.join(rows)

battle_field = [[1, 0, 0, 0, 0, 1, 1, 0, 0, 0],
                [1, 0, 1, 0, 0, 0, 0, 0, 1, 0],
                [1, 0, 1, 0, 1, 1, 1, 0, 1, 0],
                [1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

print(validateBattlefield(battle_field))

