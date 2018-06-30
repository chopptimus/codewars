from collections import deque
from itertools import product

DELTAS = [-1, 0, 1]


def next_moves(grid, position):
    x = position.x
    y = position.y
    height = len(grid)
    width = len(grid[0])
    pseudo_moves = [(x + dx, y + dy) for dx, dy in product(DELTAS, DELTAS) if abs(dx + dy) == 1]
    return [grid[x][y] for x, y in pseudo_moves
            if x >= 0 and x < height
            and y >= 0 and y < width
            and grid[x][y].passable]


def backtrace(node, meta_dict):
    path = []
    while node:
        path.append(node)
        node, _ = meta_dict[node]
    return list(reversed(path))


def find_shortest_path(grid, start, end):
    fifo = deque([start])
    closed_set = set()
    meta_dict = {start: (None, None)}
    while fifo:
        parent = fifo.pop()
        if parent is end:
            return backtrace(parent, meta_dict)
        for child in next_moves(grid, parent.position):
            if child not in closed_set:
                meta_dict[child] = (parent, child)
                fifo.appendleft(child)
        closed_set.add(parent)
