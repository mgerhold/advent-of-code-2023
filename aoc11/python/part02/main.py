from dataclasses import dataclass

DISTANCE_FACTOR = 1000000


@dataclass
class Point:
    row: int
    column: int

    def distance(self, other: "Point", empty_lines: list[int], empty_columns: list[int]) -> int:
        min_row = min(self.row, other.row)
        max_row = max(self.row, other.row)
        min_column = min(self.column, other.column)
        max_column = max(self.column, other.column)
        num_empty_lines_in_range = len([row for row in range(min_row, max_row) if row in empty_lines])
        num_empty_columns_in_range = len(
            [column for column in range(min_column, max_column) if column in empty_columns])
        num_non_empty_lines_in_range = max_row - min_row - num_empty_lines_in_range
        num_non_empty_columns_in_range = max_column - min_column - num_empty_columns_in_range
        return num_non_empty_lines_in_range + num_non_empty_columns_in_range + DISTANCE_FACTOR * (
                num_empty_lines_in_range + num_empty_columns_in_range)


def main() -> None:
    data = open("data.txt").read().splitlines()
    (empty_lines, empty_columns) = determine_empty_lines_and_columns(data)
    stars = get_stars(data)
    result = sum_of_distances(stars, empty_lines, empty_columns)
    print(result)


def determine_empty_lines_and_columns(lines: list[str]) -> tuple[list[int], list[int]]:
    empty_lines = [i for (i, line) in enumerate(lines) if all(char == "." for char in line)]
    empty_columns = [column for column in range(len(lines[0])) if
                     all(lines[row][column] == "." for row in range(len(lines)))]

    return empty_lines, empty_columns


def get_stars(transformed: list[str]) -> list[Point]:
    stars: list[Point] = []
    for row, line in enumerate(transformed):
        for column, char in enumerate(line):
            if char == "#":
                stars.append(Point(row, column))
    return stars


def sum_of_distances(stars: list[Point], empty_rows: list[int], empty_columns: list[int]) -> int:
    result = 0
    for i, star1 in enumerate(stars):
        for j in range(i + 1, len(stars)):
            star2 = stars[j]
            result += star1.distance(star2, empty_rows, empty_columns)
    return result


if __name__ == "__main__":
    main()
