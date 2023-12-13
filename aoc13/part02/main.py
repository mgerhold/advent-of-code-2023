from enum import auto
from enum import Enum
from typing import Optional


class Axis(Enum):
    HORIZONTAL = auto()
    VERTICAL = auto()


def determine_mirror_axis(pattern: list[str],
                          to_ignore: Optional[tuple[Axis, int]],
                          axis: Axis = Axis.VERTICAL) -> Optional[tuple[Axis, int]]:
    if axis == Axis.HORIZONTAL:
        pattern = transpose(pattern)
    width, height = len(pattern[0]), len(pattern)
    for column in range(width - 1):
        if to_ignore is not None and axis == to_ignore[0] and column == to_ignore[1]:
            continue
        is_mirror_axis = True
        for line in pattern:
            for (left, right) in ((line[a], line[b]) for (a, b) in
                                  zip(range(column, -1, -1), range(column + 1, width))):
                if left != right:
                    is_mirror_axis = False
                    break
            if not is_mirror_axis:
                break
        if is_mirror_axis:
            return axis, column
    return determine_mirror_axis(pattern, to_ignore, Axis.HORIZONTAL) if axis == Axis.VERTICAL else None


def transpose(pattern: list[str]) -> list[str]:
    return [''.join(row) for row in zip(*pattern)]


def calculate_score(result: tuple[Axis, int]) -> int:
    match result[0]:
        case Axis.HORIZONTAL:
            return 100 * (result[1] + 1)
        case Axis.VERTICAL:
            return result[1] + 1


def process_pattern_variations(pattern: list[str], to_ignore: tuple[Axis, int]) -> tuple[Axis, int]:
    width, height = len(pattern[0]), len(pattern)
    for row in range(height):
        for column in range(width):
            copy = pattern[:]
            line = copy[row]
            char = line[column]
            line = line[:column] + ("." if char == "#" else ".") + line[column + 1:]
            copy[row] = line
            # print("\n".join(copy))
            result = determine_mirror_axis(copy, to_ignore)
            if result is not None and result != to_ignore:
                return result
    raise Exception("No solution found")


def main() -> None:
    patterns = [pattern.split("\n") for pattern in open("data.txt").read().split("\n\n")]
    print(patterns)
    score = 0
    for pattern in patterns:
        original_solution = determine_mirror_axis(pattern, None)
        assert original_solution is not None
        result = process_pattern_variations(pattern, original_solution)
        score += calculate_score(result)
        print(result)
    print(score)


if __name__ == "__main__":
    main()
