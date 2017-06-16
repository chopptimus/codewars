import re

def parse_group(tokens, counts, scale=1):
    _scale = scale
    for t in tokens:
        if t in '])}':
            parse_group(tokens, counts, _scale)
        elif t in '([{':
            break
        elif t.isdigit():
            _scale = scale * int(t)
            continue
        elif t.isalpha():
            counts[t] = counts.get(t, 0) + _scale
        _scale = scale
    return counts

def parse_molecule (formula):
    tokens = re.findall(r'[A-Z][a-z]?|[()\[\]{}]|\d+', formula)
    return parse_group(iter(reversed(tokens)), {})
