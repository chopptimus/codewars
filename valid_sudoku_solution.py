def validSolution(board):
    flat = [c for row in board for c in row]
    if 0 in flat:
        return False

    for row in board:
        if sorted(row) != list(range(1, 10)):
            print(sorted(row))
            return False

    for i in range(9):
        if sorted(flat[i::9]) != range(1, 10):
            print('bar')
            return False

    for i in range(0, 9, 3):
        triple_row = from_iterable(board[i:i+3])
        for j in range(0, 9, 3):
            square = [row[j:j+3] for row in triple_row]
            square = [c for row in square for c in row]
            if sorted(square) != range(1, 10):
                print('baz')
                return False

print(validSolution([[5, 3, 4, 6, 7, 8, 9, 1, 2], 
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 1, 7, 9]]))
