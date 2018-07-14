from collections import deque


def algebraic_notation_to_indices(s):
    return ord(s[0]) - 97, int(s[1]) - 1 


def is_valid_position(position):
    file_, rank = position
    return file_ >= 0 and file_ < 8 and rank >= 0 and rank < 8


def knight_moves(position):
    file_, rank = position
    pseudo_moves = [(file_ + 1, rank + 2),
                    (file_ - 1, rank + 2),
                    (file_ + 1, rank - 2),
                    (file_ - 1, rank - 2),
                    (file_ + 2, rank + 1),
                    (file_ - 2, rank + 1),
                    (file_ + 2, rank - 1),
                    (file_ - 2, rank - 1)]
    return filter(is_valid_position, pseudo_moves)


def knight(p1, p2):
    target_position = algebraic_notation_to_indices(p2)
    open_set = deque([(algebraic_notation_to_indices(p1), 0)])
    closed_set = set()
    while open_set:
        position, n = open_set.popleft()
        for move in knight_moves(position):
            if move == target_position:
                return n + 1
            elif move not in closed_set:
                open_set.append((move, n + 1))

        closed_set.add(position)
    raise RuntimeError
