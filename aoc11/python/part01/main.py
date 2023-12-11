from dataclasses import dataclass


@dataclass
class Point:
    row: int
    column: int

    def distance(self, other: "Point") -> int:
        return abs(self.row - other.row) + abs(self.column - other.column)


def main() -> None:
    data = open("data.txt").read().splitlines()
    (empty_lines, empty_columns) = determine_empty_lines_and_columns(data)
    transformed = transform_data(data, empty_columns, empty_lines)
    stars = get_stars(transformed)
    result = sum_of_distances(stars)
    print(result)


def determine_empty_lines_and_columns(lines: list[str]) -> tuple[list[int], list[int]]:
    empty_lines = [i for (i, line) in enumerate(lines) if all(char == "." for char in line)]
    empty_columns = [column for column in range(len(lines[0])) if
                     all(lines[row][column] == "." for row in range(len(lines)))]

    return empty_lines, empty_columns


def transform_data(data: list[str], empty_columns: list[int], empty_lines: list[int]) -> list[str]:
    transformed = []
    for i, row in enumerate(data):
        line = ""
        for j, char in enumerate(row):
            line += char
            if j in empty_columns:
                line += char
        transformed.append(line)
        if i in empty_lines:
            transformed.append(line)
    return transformed


def get_stars(transformed: list[str]) -> list[Point]:
    stars: list[Point] = []
    for row, line in enumerate(transformed):
        for column, char in enumerate(line):
            if char == "#":
                stars.append(Point(row, column))
    return stars


def sum_of_distances(stars: list[Point]) -> int:
    result = 0
    for i, star1 in enumerate(stars):
        for j in range(i + 1, len(stars)):
            star2 = stars[j]
            result += star1.distance(star2)
    return result


if __name__ == "__main__":
    main()
