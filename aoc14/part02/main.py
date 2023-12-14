from typing import NamedTuple
from typing import Optional


class Map(NamedTuple):
    lines: list[str]

    @property
    def width(self) -> int:
        return len(self.lines[0])

    @property
    def height(self) -> int:
        return len(self.lines)

    @property
    def joined(self) -> str:
        return "\n".join(self.lines)


class CacheHit(NamedTuple):
    step: int
    joined_map_state: str


def rotate_map_90_degrees_clockwise(map_data: Map) -> Map:
    return Map(["".join(x[::-1]) for x in zip(*map_data.lines)])


def calculate_fixed_rock_positions(map_data: Map) -> list[list[tuple[int, int]]]:
    fixed_rock_positions: list[list[tuple[int, int]]] = [[(-1, 0)] for _ in range(map_data.width)]
    for column in range(map_data.width):
        rounded_rock_counter = 0
        for row in range(map_data.height):
            match map_data.lines[row][column]:
                case "O":
                    rounded_rock_counter += 1
                case "#":
                    fixed_rock_positions[column][-1] = (fixed_rock_positions[column][-1][0], rounded_rock_counter)
                    rounded_rock_counter = 0
                    fixed_rock_positions[column].append((row, 0))
        fixed_rock_positions[column][-1] = (fixed_rock_positions[column][-1][0], rounded_rock_counter)
    return fixed_rock_positions


def rock_positions_to_map(width: int, height: int, positions: list[list[tuple[int, int]]]) -> Map:
    lines = ["." * width] * height
    for column, rock_positions in enumerate(positions):
        for row, amount in rock_positions:
            if row >= 0:
                current_string = lines[row]
                new_string = current_string[:column] + "#" + current_string[column + 1:]
                lines[row] = new_string
            for i in range(amount):
                current_string = lines[row + 1 + i]
                new_string = current_string[:column] + "O" + current_string[column + 1:]
                lines[row + 1 + i] = new_string
    return Map(lines)


def advance_one_cycle(cache: dict[str, str], map_data: Map) -> Map:
    input_joined = map_data.joined
    if input_joined in cache:
        return Map(cache[input_joined].splitlines())
    for i in range(4):
        positions = calculate_fixed_rock_positions(map_data)
        map_data = rock_positions_to_map(map_data.width, map_data.height, positions)
        map_data = rotate_map_90_degrees_clockwise(map_data)
    cache[input_joined] = map_data.joined
    return map_data


def calculate_load(map_data: Map) -> int:
    return sum(
        (map_data.height - row) * sum(1 for char in line_content if char == "O")
        for row, line_content
        in enumerate(map_data.lines)
    )


NUM_CYCLES = 1000000000


def main() -> None:
    data = Map(open("data.txt").read().splitlines())
    cache: dict[str, str] = dict()
    first_cache_hit: Optional[CacheHit] = None
    step = 0
    while step < NUM_CYCLES:
        data = advance_one_cycle(cache, data)
        step += 1
        if data.joined in cache:
            if first_cache_hit is not None and first_cache_hit.joined_map_state == data.joined:
                offset = first_cache_hit.step
                period = step - offset
                num_periods = (NUM_CYCLES - offset - period) // period
                step += num_periods * period
            if first_cache_hit is None:
                first_cache_hit = CacheHit(step, data.joined)
    print(calculate_load(data))


if __name__ == "__main__":
    main()
