import itertools
from pprint import pprint


def get_input():
    with open("day5.input") as f:
        # remove those annoying newlines
        return map(lambda x: x[:-1], f.readlines())


def get_crate_layout(input):
    return list(itertools.takewhile(is_crate_layout, input))


def get_arrangement_spec(input):
    return list(itertools.dropwhile(is_crate_layout, input))[1:]


def is_crate_layout(cs: str) -> bool:
    return "[" in cs


def parse_crate_layout(layout: str) -> list[str]:
    crates = []
    for col, c in enumerate(layout):
        if (col - 1) % 4 == 0:
            crates.append(c.isalpha() and c or None)
    return crates


def parse_arrangement_spec(spec: list[str]) -> list[tuple[int, int, int]]:
    parsed = []
    for action in spec:
        items = action.split()
        parsed.append((int(items[1]), int(items[3]) - 1, int(items[5]) - 1))
    return parsed


def transpose(M: list[list[str]]) -> list[list[str]]:
    #size = len(matrix)
    #for row in range(size):
    #    for col in range(row + 1, size):
    #        matrix[col][row], matrix[row][col] = matrix[row][col], matrix[col][row]
    #return matrix

    return [[M[j][i] for j in range(len(M))] for i in range(len(M[0]))]


input = get_input()

layout = transpose([parse_crate_layout(i) for i in get_crate_layout(input)])
    
spec = parse_arrangement_spec(get_arrangement_spec(input))

for i in range(len(layout)):
    layout[i] = list(itertools.dropwhile(lambda x: x is None, layout[i]))

for moves, from_, to in spec:
    #for i in range(len(layout[from_][:moves])):
    #    layout[to].insert(0, list(reversed(layout[from_][:moves]))[i])
    layout[to] = layout[from_][:moves] + layout[to]

    #for i in range(len(layout[from_][:moves])):
    #    layout[from_].pop(0)
    layout[from_] = layout[from_][moves:]

buf = []
for col in layout:
    buf.append(col[0])
#pprint(layout)
print("".join(buf))
