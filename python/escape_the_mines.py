def solve(mine_map, miner, exit):
    paths = [[[] for c in column] for column in mine_map]
    search(mine_map, miner['x'], miner['y'], [], paths)
    return paths[exit['x']][exit['y']]

def search(mine_map, x, y, path, paths):
    if len(paths[x][y]) > len(path) or not paths[x][y]:
        paths[x][y] = path
    else:
        return

    children = []
    if x > 0:
        children.append((x - 1, y, list(path) + ['left']))
    if y > 0:
        children.append((x, y - 1, list(path) + ['up']))
    children.append((x + 1, y, list(path) + ['right']))
    children.append((x, y + 1, list(path) + ['down']))

    for new_x, new_y, new_path in children:
        try:
            cell = mine_map[new_x][new_y]
        except IndexError:
            continue

        if mine_map[new_x][new_y]:
            search(mine_map, new_x, new_y, new_path, paths)
