from enum import auto
from enum import Enum
from typing import Optional


class Axis(Enum):
    HORIZONTAL = auto()
    VERTICAL = auto()


def is_line_mirrored_by(line: str, column: int) -> bool:
    for (left, right) in ((line[a], line[b]) for (a, b) in
                          zip(range(column, -1, -1), range(column + 1, len(line)))):
        if left != right:
            return False
    return True


def determine_vertical_mirror_axis(pattern: list[str], column_to_ignore: Optional[int]) -> Optional[int]:
    width, height = len(pattern[0]), len(pattern)
    for column in (c for c in range(width - 1) if c != column_to_ignore):
        if all(is_line_mirrored_by(line, column) for line in pattern):
            return column
    return None


def determine_horizontal_mirror_axis(pattern: list[str], row_to_ignore: Optional[int]) -> Optional[int]:
    return determine_vertical_mirror_axis(transpose(pattern), row_to_ignore)


def determine_mirror_axis(pattern: list[str], to_ignore: Optional[tuple[Axis, int]]) -> Optional[tuple[Axis, int]]:
    vertical = determine_vertical_mirror_axis(
        pattern,
        None
        if to_ignore is None or to_ignore[0] != Axis.VERTICAL
        else to_ignore[1]
    )
    if vertical is not None:
        return Axis.VERTICAL, vertical

    horizontal = determine_horizontal_mirror_axis(
        pattern,
        None
        if to_ignore is None or to_ignore[0] != Axis.HORIZONTAL
        else to_ignore[1]
    )
    if horizontal is not None:
        return Axis.HORIZONTAL, horizontal
    return None


def transpose(pattern: list[str]) -> list[str]:
    return [''.join(row) for row in zip(*pattern)]


def calculate_score(result: tuple[Axis, int]) -> int:
    axis, row_or_column = result
    return (100 if axis == Axis.HORIZONTAL else 1) * (row_or_column + 1)


def process_pattern_variations(pattern: list[str], to_ignore: tuple[Axis, int]) -> tuple[Axis, int]:
    width, height = len(pattern[0]), len(pattern)
    for row in range(height):
        for column in range(width):
            copy = pattern[:]
            line = copy[row]
            char = line[column]
            line = line[:column] + ("." if char == "#" else ".") + line[column + 1:]
            copy[row] = line
            result = determine_mirror_axis(copy, to_ignore)
            if result is not None and result != to_ignore:
                return result
    raise Exception("No solution found")


def main() -> None:
    patterns = [pattern.split("\n") for pattern in open("data.txt").read().split("\n\n")]
    score = 0
    for pattern in patterns:
        original_solution = determine_mirror_axis(pattern, None)
        assert original_solution is not None
        result = process_pattern_variations(pattern, original_solution)
        score += calculate_score(result)
    print(score)


if __name__ == "__main__":
    main()
