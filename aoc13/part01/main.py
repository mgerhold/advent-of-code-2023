from enum import auto
from enum import Enum


class Axis(Enum):
    HORIZONTAL = auto()
    VERTICAL = auto()


def determine_mirror_axis(pattern: list[str]) -> tuple[Axis, int]:
    width, height = len(pattern[0]), len(pattern)
    for column in range(width - 1):
        is_mirror_axis = True
        for line in pattern:
            for (left, right) in ((line[a], line[b]) for (a, b) in
                                  zip(range(column, -1, -1), range(column + 1, width))):
                if left != right:
                    is_mirror_axis = False
                    break
        if is_mirror_axis:
            return Axis.VERTICAL, column
    horizontal_result = determine_mirror_axis(transpose(pattern))
    return Axis.HORIZONTAL, horizontal_result[1]


def transpose(pattern: list[str]) -> list[str]:
    return [''.join(row) for row in zip(*pattern)]


def calculate_score(result: tuple[Axis, int]) -> int:
    match result[0]:
        case Axis.HORIZONTAL:
            return 100 * (result[1] + 1)
        case Axis.VERTICAL:
            return result[1] + 1


patterns = [pattern.split("\n") for pattern in open("data.txt").read().split("\n\n")]
print(patterns)
result = sum(calculate_score(determine_mirror_axis(result)) for result in patterns)
print(result)
