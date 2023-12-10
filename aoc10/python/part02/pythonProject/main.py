import re
from enum import Enum, auto

lines = open("data.txt").read().splitlines()
height = len(lines)
width = len(lines[0])


def lookup(row, column):
    return lines[row][column]


route = set(eval(open("route.txt").read()))
print(route)

new_lines = []
for row in range(height):
    line = ""
    for col in range(width):
        if (row, col) in route:
            char = lookup(row, col)
            line += char
        else:
            line += "."
    new_lines.append(line)

print(new_lines)


class State(Enum):
    CLOSED = auto()
    OPENING = auto()
    OPEN = auto()
    CLOSING = auto()


patterns = [("L-*J", "||"), ("F-*7", "||"), ("F-*J", "|"), ("L-*7", "|")]


def process_line(line):
    for pattern, replacement in patterns:
        line = re.sub(pattern, replacement, line)
    return line


new_lines = [process_line(line) for line in new_lines]
print(new_lines)

result = 0
for row, line in enumerate(new_lines):
    print(f"{row=}")
    line_result = 0
    counter = 0
    for char in line:
        if char == "|":
            counter += 1
        if char == "." and counter % 2 == 1:
            line_result += 1
    print(f"{line_result=}")
    result += line_result
print(result)
