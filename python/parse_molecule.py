import re
import sys
from collections import Counter, deque

group_regex = re.compile('([\(\[\{].*[\)\]\}])([1-9][0-9]*)?')
counting_regex = re.compile('([A-Z][a-z]?)([1-9][0-9]*)?')
matching = {'{': '}', '[': ']', '(': ')'}

def divide(molecule):
    remaining = molecule
    inners = []

    while True:
        bracket = None
        for i, c in enumerate(remaining):
            if c in '([{':
                bracket = c
                break
        if not bracket:
            return remaining, inners

        for i, c in enumerate(remaining):
            if c == bracket:
                start = i
                break
        
        stack = 0
        for i, c in enumerate(remaining):
            if c == bracket:
                stack += 1
            elif c == matching[bracket]:
                stack -= 1
                if stack == 0:
                    end = i + 1
                    count = ''
                    try:
                        while remaining[end].isdigit():
                            count += remaining[end]
                            end += 1
                    except IndexError:
                        pass
                    
                    if not count:
                        count = 1
                    else:
                        count = int(count)

                    inners.append((remaining[start+1:i], count))
                    remaining = remaining.replace(remaining[start:end], '')
                    break

    return remaining, inners

def parse_molecule(molecule):
    molecule, inners = divide(molecule)

    outer = Counter()
    for atom, count in counting_regex.findall(molecule):
        if count:
            outer[atom] = int(count)
        else:
            outer[atom] = 1

    for group, count in inners:
        inner = parse_molecule(group)
        inner = Counter({k: v * int(count) for k, v in inner.items()})
        outer += inner
    
    return outer

print(parse_molecule('Al14O5'))
