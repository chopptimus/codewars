VALID_UNIT = {1, 2, 3, 4, 5, 6, 7, 8, 9}

def sudoku(puzzle):
    seen = set()
    puzzle = tuple([tuple(row) for row in puzzle])
    stack = [puzzle]
    while stack:
        board = stack.pop()

        if board in seen: continue
        if validate(board, solution=True): return [list(row) for row in board]
        if not validate(board): continue

        seen.add(board)

        i = [i for i, row in enumerate(board) if 0 in row][0]
        j = [j for j, cell in enumerate(board[i]) if not cell][0]

        for n in range(1, 10):
            stack.append(change_cell(board, i, j, n))

def change_cell(board, i, j, n):
    new_board = [list(row) for row in board]
    new_board[i][j] = n
    return tuple([tuple(row) for row in new_board])

def validate(board, solution=False):
    for row in board:
        if not validate_unit(row, solution): return False

    for col in zip(*board):
        if not validate_unit(col, solution): return False

    for x in (0, 3, 6):
        for y in (0, 3, 6):
            block = [board[x + a][y + b] for a in (0, 1, 2) for b in (0, 1, 2)]
            if not validate_unit(block, solution): return False

    return True

def validate_unit(unit, solution):
    if solution:
        return set(unit) == VALID_UNIT
    else:
        no_zeros = [u for u in unit if u]
        return len(set(no_zeros)) == len(no_zeros)

puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

solution = [[5,3,4,6,7,8,9,1,2],
            [6,7,2,1,9,5,3,4,8],
            [1,9,8,3,4,2,5,6,7],
            [8,5,9,7,6,1,4,2,3],
            [4,2,6,8,5,3,7,9,1],
            [7,1,3,9,2,4,8,5,6],
            [9,6,1,5,3,7,2,8,4],
            [2,8,7,4,1,9,6,3,5],
            [3,4,5,2,8,6,1,7,9]]

def test_words():
    assert sudoku(puzzle) == solution
