from __future__ import annotations

import re
from enum import Enum
from typing import NamedTuple
from typing import Optional


class Vec2(NamedTuple):
    x: int
    y: int

    def __add__(self, other: object) -> Vec2:
        if not isinstance(other, Vec2):
            raise NotImplemented
        return Vec2(self.x + other.x, self.y + other.y)


class Direction(Enum):
    UP = Vec2(0, -1)
    DOWN = Vec2(0, 1)
    LEFT = Vec2(-1, 0)
    RIGHT = Vec2(1, 0)


class DigInstruction(NamedTuple):
    direction: Direction
    distance: int
    color: str

    @classmethod
    def from_str(cls, s: str) -> DigInstruction:
        direction_str, distance_str, color = s.split()
        match direction_str:
            case "R":
                direction = Direction.RIGHT
            case "L":
                direction = Direction.LEFT
            case "U":
                direction = Direction.UP
            case "D":
                direction = Direction.DOWN
            case _:
                raise ValueError(f"invalid direction")

        distance = int(distance_str)
        color = color[1:-1]
        return cls(direction, distance, color)


class TileType(Enum):
    VERTICAL = "|"
    HORIZONTAL = "-"
    UP_RIGHT_BEND = "F"
    UP_LEFT_BEND = "7"
    DOWN_RIGHT_BEND = "L"
    DOWN_LEFT_BEND = "J"


class Map:
    def __init__(self, instructions: list[DigInstruction]) -> None:
        current = Vec2(0, 0)
        self._dug_out_tiles: dict[Vec2, TileType] = dict()
        last_direction: Optional[Direction] = None
        last_tile_type: Optional[TileType] = None
        for instruction in instructions:
            match last_tile_type, instruction.direction:
                case (None, Direction.RIGHT) | (None, Direction.LEFT):
                    tile_type = TileType.HORIZONTAL
                case (None, Direction.UP) | (None, Direction.DOWN):
                    tile_type = TileType.VERTICAL
                case (TileType.HORIZONTAL, Direction.UP):
                    assert last_direction is not None
                    current += last_direction.value
                    if last_direction == Direction.LEFT:
                        self._dig(current, TileType.DOWN_RIGHT_BEND)
                    else:
                        assert last_direction == Direction.RIGHT
                        self._dig(current, TileType.DOWN_LEFT_BEND)
                    tile_type = TileType.VERTICAL
                case (TileType.HORIZONTAL, Direction.DOWN):
                    assert last_direction is not None
                    current += last_direction.value
                    if last_direction == Direction.LEFT:
                        self._dig(current, TileType.UP_RIGHT_BEND)
                    else:
                        assert last_direction == Direction.RIGHT
                        self._dig(current, TileType.UP_LEFT_BEND)
                    tile_type = TileType.VERTICAL
                case (TileType.VERTICAL, Direction.LEFT):
                    assert last_direction is not None
                    current += last_direction.value
                    if last_direction == Direction.UP:
                        self._dig(current, TileType.UP_LEFT_BEND)
                    else:
                        assert last_direction == Direction.DOWN
                        self._dig(current, TileType.DOWN_LEFT_BEND)
                    tile_type = TileType.HORIZONTAL
                case (TileType.VERTICAL, Direction.RIGHT):
                    assert last_direction is not None
                    current += last_direction.value
                    if last_direction == Direction.UP:
                        self._dig(current, TileType.UP_RIGHT_BEND)
                    else:
                        assert last_direction == Direction.DOWN
                        self._dig(current, TileType.DOWN_RIGHT_BEND)
                    tile_type = TileType.HORIZONTAL
                case _:
                    raise Exception("unreachable")

            for _ in range(instruction.distance - 1):
                current += instruction.direction.value
                self._dig(current, tile_type)

            last_direction = instruction.direction
            last_tile_type = tile_type

        current += last_direction.value
        connecting_direction = next(
            direction
            for direction
            in (Direction.RIGHT, Direction.LEFT, Direction.UP, Direction.DOWN)
            if direction.value in self._dug_out_tiles
        )
        match last_direction, connecting_direction:
            case (Direction.RIGHT, Direction.UP):
                tile_type = TileType.DOWN_LEFT_BEND
            case (Direction.RIGHT, Direction.DOWN):
                tile_type = TileType.UP_LEFT_BEND
            case (Direction.LEFT, Direction.UP):
                tile_type = TileType.DOWN_RIGHT_BEND
            case (Direction.LEFT, Direction.DOWN):
                tile_type = TileType.UP_RIGHT_BEND
            case (Direction.UP, Direction.LEFT):
                tile_type = TileType.UP_LEFT_BEND
            case (Direction.UP, Direction.RIGHT):
                tile_type = TileType.UP_RIGHT_BEND
            case (Direction.DOWN, Direction.LEFT):
                tile_type = TileType.DOWN_LEFT_BEND
            case (Direction.DOWN, Direction.RIGHT):
                tile_type = TileType.DOWN_RIGHT_BEND
            case _:
                raise Exception("unreachable")
        self._dig(current, tile_type)
        assert current == Vec2(0, 0)

    def calculate_volume(self) -> int:
        patterns = [("L-*J", "||"), ("F-*7", "||"), ("F-*J", "|"), ("L-*7", "|")]

        volume = 0
        for line in str(self).splitlines():
            num_walls = sum(int(char != ".") for char in line)
            volume += num_walls

            for pattern, replacement in patterns:
                line = re.sub(pattern, replacement, line)
            bar_counter = 0
            num_between_walls = 0
            for char in line:
                if char == "|":
                    bar_counter += 1
                if char == "." and bar_counter % 2 == 1:
                    num_between_walls += 1
            volume += num_between_walls

        return volume

    def _dig(self, position: Vec2, type_: TileType) -> None:
        assert position not in self._dug_out_tiles
        self._dug_out_tiles[position] = type_

    @property
    def bounds(self) -> tuple[Vec2, Vec2]:
        assert len(self._dug_out_tiles) > 0
        top_left: Optional[Vec2] = None
        bottom_right = top_left
        for tile in self._dug_out_tiles.keys():
            if top_left is None:
                top_left = tile
            else:
                top_left = Vec2(min(top_left.x, tile.x), min(top_left.y, tile.y))
            if bottom_right is None:
                bottom_right = tile
            else:
                bottom_right = Vec2(max(bottom_right.x, tile.x), max(bottom_right.y, tile.y))
        return top_left, bottom_right

    def __str__(self) -> str:
        top_left, bottom_right = self.bounds
        print(f"{top_left=}, {bottom_right=}")
        result = ""
        for y in range(top_left.y, bottom_right.y + 1):
            for x in range(top_left.x, bottom_right.x + 1):
                if Vec2(x, y) in self._dug_out_tiles:
                    result += self._dug_out_tiles[Vec2(x, y)].value
                else:
                    result += "."
            result += "\n"
        return result


def main() -> None:
    dig_plan = [DigInstruction.from_str(line) for line in open("data.txt")]
    landscape = Map(dig_plan)
    print(landscape)
    print(landscape.calculate_volume())


if __name__ == "__main__":
    main()
