def last_digit(a, b):
    if b == 0:
        return 1

    cycle_table = [
            [0],
            [1],
            [2, 4, 8, 6],
            [3, 9, 7, 1],
            [4, 6],
            [5],
            [6],
            [7, 9, 3, 1],
            [8, 4, 2, 6],
            [9, 1]
            ]
    
    base_last_digit = int(str(a)[-1])
    return cycle_table[base_last_digit][b % len(cycle_table[base_last_digit]) - 1]

