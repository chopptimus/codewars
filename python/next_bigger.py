def next_bigger(n):
    str_n = list(str(n))

    for i, c in enumerate(str_n[:-1]):
        if int(c) < int(str_n[i + 1]):
            a = i

    try:
        str_n[a + 1:] = sorted(str_n[a + 1:])
    except NameError:
        return -1

    for i, c in enumerate(str_n[a + 1:]):
        if str_n[a] < c:
            str_n[a], str_n[a + 1 + i] = str_n[a + 1 + i], str_n[a]
            break

    return int(''.join(str_n))

print(next_bigger(513))
