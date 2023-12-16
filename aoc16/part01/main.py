from collections import deque
from enum import Enum
from typing import NamedTuple
from typing import Self


class Vec2(NamedTuple):
    x: int
    y: int

    def __add__(self, other: Self) -> Self:
        return Vec2(self.x + other.x, self.y + other.y)


class Direction(Enum):
    UP = Vec2(0, -1)
    DOWN = Vec2(0, 1)
    LEFT = Vec2(-1, 0)
    RIGHT = Vec2(1, 0)


class Beam(NamedTuple):
    position: Vec2
    direction: Direction

    def advance(self) -> Self:
        return Beam(self.position + self.direction.value, self.direction)


class TileType(Enum):
    EMPTY = "."
    VERTICAL = "|"
    HORIZONTAL = "-"
    FORWARD_SLASH = "/"
    BACKWARD_SLASH = "\\"


class Map:
    def __init__(self, contents: str) -> None:
        self._width = contents.index("\n")
        self._contents = [TileType(char) for char in contents.replace("\n", "")]
        assert len(self._contents) % self._width == 0

    @property
    def width(self) -> int:
        return self._width

    @property
    def height(self) -> int:
        return len(self._contents) // self._width

    def get(self, position: Vec2) -> TileType:
        return self._contents[position.y * self.width + position.x]

    def is_valid_position(self, position: Vec2) -> bool:
        return position.x in range(self._width) and position.y in range(self.height)

    def to_string(self, visited: set[Beam]) -> str:
        result = str(self).splitlines()
        for beam in visited:
            if result[beam.position.y][beam.position.x] != TileType.EMPTY.value:
                continue
            match beam.direction:
                case Direction.UP:
                    char = "^"
                case Direction.DOWN:
                    char = "v"
                case Direction.LEFT:
                    char = "<"
                case Direction.RIGHT:
                    char = ">"
                case _:
                    raise Exception(f"Invalid direction: {beam.direction}")
            line = result[beam.position.y]
            line = f"{line[:beam.position.x]}{char}{line[beam.position.x + 1:]}"
            result[beam.position.y] = line
        return "\n".join(result) + "\n"

    def to_energized_string(self, visited: set[Beam]) -> str:
        result = ["." * self.width] * self.height
        for beam in visited:
            line = result[beam.position.y]
            line = f"{line[:beam.position.x]}#{line[beam.position.x + 1:]}"
            result[beam.position.y] = line
        return "\n".join(result) + "\n"

    def __str__(self) -> str:
        result = ""
        for y in range(self.height):
            result += "".join(tile_type.value
                              for tile_type
                              in self._contents[y * self.width:(y + 1) * self.width]
                              ) + "\n"
        return result


def beam_can_advance_further(beam: Beam, contraption: Map) -> bool:
    if not contraption.is_valid_position(beam.position):
        return False
    if contraption.get(beam.position) == TileType.EMPTY:
        return True
    if beam.direction in (Direction.UP, Direction.DOWN) and contraption.get(beam.position) == TileType.VERTICAL:
        return True
    if beam.direction in (Direction.LEFT, Direction.RIGHT) and contraption.get(beam.position) == TileType.HORIZONTAL:
        return True
    return False


def main() -> None:
    queue = deque([Beam(position=Vec2(0, 0), direction=Direction.RIGHT)])
    energized: set[Beam] = set()
    contraption = Map(open("data.txt").read())
    print(contraption)
    while len(queue) > 0:
        next_beam = queue.pop()
        while beam_can_advance_further(next_beam, contraption):
            if next_beam in energized:
                break
            energized.add(next_beam)
            next_beam = next_beam.advance()

        if next_beam in energized or not contraption.is_valid_position(next_beam.position):
            continue

        if next_beam not in energized:
            energized.add(next_beam)

        assert contraption.get(next_beam.position) != TileType.EMPTY
        match next_beam.direction, contraption.get(next_beam.position):
            case (Direction.RIGHT, TileType.VERTICAL) | (Direction.LEFT, TileType.VERTICAL):
                queue.append(Beam(position=next_beam.position + Direction.UP.value, direction=Direction.UP))
                queue.append(Beam(position=next_beam.position + Direction.DOWN.value, direction=Direction.DOWN))
            case (Direction.UP, TileType.HORIZONTAL) | (Direction.DOWN, TileType.HORIZONTAL):
                queue.append(Beam(position=next_beam.position + Direction.RIGHT.value, direction=Direction.RIGHT))
                queue.append(Beam(position=next_beam.position + Direction.LEFT.value, direction=Direction.LEFT))
            case (Direction.RIGHT, TileType.BACKWARD_SLASH) | (Direction.LEFT, TileType.FORWARD_SLASH):
                queue.append(Beam(position=next_beam.position + Direction.DOWN.value, direction=Direction.DOWN))
            case (Direction.RIGHT, TileType.FORWARD_SLASH) | (Direction.LEFT, TileType.BACKWARD_SLASH):
                queue.append(Beam(position=next_beam.position + Direction.UP.value, direction=Direction.UP))
            case (Direction.DOWN, TileType.BACKWARD_SLASH) | (Direction.UP, TileType.FORWARD_SLASH):
                queue.append(Beam(position=next_beam.position + Direction.RIGHT.value, direction=Direction.RIGHT))
            case (Direction.DOWN, TileType.FORWARD_SLASH) | (Direction.UP, TileType.BACKWARD_SLASH):
                queue.append(Beam(position=next_beam.position + Direction.LEFT.value, direction=Direction.LEFT))

    print(contraption.to_string(energized))
    print(contraption.to_energized_string(energized))
    print(len({beam.position for beam in energized}))


if __name__ == "__main__":
    main()
