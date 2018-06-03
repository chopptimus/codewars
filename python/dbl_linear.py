def dbl_linear(n):
    current = 1
    doubles = double()
    triples = triple()
    u = 3
    v = 4
    for _ in range(n):
        if u < v:
            u = next(doubles)
        else:
            v = next(triples)

    return max(u, v)

def double():
    v = 1
    while True:
        v = v * 2 + 1
        yield v

def triple():
    v = 1
    while True:
        v = v * 3 + 1
        yield v

print(dbl_linear(10))
