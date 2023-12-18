from __future__ import annotations

from enum import Enum
from typing import NamedTuple


class Vec2(NamedTuple):
    x: int
    y: int

    def __add__(self, other: object) -> Vec2:
        if not isinstance(other, Vec2):
            raise NotImplemented
        return Vec2(self.x + other.x, self.y + other.y)

    def __mul__(self, other: object) -> Vec2:
        if not isinstance(other, int):
            raise NotImplemented
        return Vec2(self.x * other, self.y * other)

    def __rmul__(self, other: object) -> Vec2:
        return self * other


class Direction(Enum):
    UP = Vec2(0, -1)
    DOWN = Vec2(0, 1)
    LEFT = Vec2(-1, 0)
    RIGHT = Vec2(1, 0)


class DigInstruction(NamedTuple):
    direction: Direction
    distance: int

    @classmethod
    def from_str(cls, s: str) -> DigInstruction:
        instruction_string = s.split()[-1][2:-1]
        match instruction_string[-1]:
            case "0":
                direction = Direction.RIGHT
            case "2":
                direction = Direction.LEFT
            case "3":
                direction = Direction.UP
            case "1":
                direction = Direction.DOWN
            case _:
                raise ValueError(f"invalid direction")

        distance = int(instruction_string[:-1], base=16)

        return cls(direction, distance)


class Map:
    def __init__(self, instructions: list[DigInstruction]) -> None:
        points: list[Vec2] = []
        current_position = Vec2(0, 0)
        total_length = 0
        for instruction in instructions:
            current_position += instruction.direction.value * instruction.distance
            total_length += instruction.distance
            points.append(current_position)
        assert current_position == Vec2(0, 0)
        self._points = points
        self._total_length = total_length

    def calculate_area(self) -> int:
        num_bends = len(self._points)
        num_outer_bends = num_bends // 2 + 2
        num_inner_bends = num_bends // 2 - 2
        num_straights = self._total_length - num_bends
        print(f"{num_outer_bends = }, {num_inner_bends = }, {num_straights = }")

        area = 0
        n = len(self._points)

        for i in range(n):
            j = (i + 1) % n
            area += self._points[i].x * self._points[j].y
            area -= self._points[j].x * self._points[i].y

        area = abs(area) // 2
        return (area * 4 + 3 * num_outer_bends + num_inner_bends + 2 * num_straights) // 4


def main() -> None:
    dig_plan = [DigInstruction.from_str(line) for line in open("data.txt")]
    landscape = Map(dig_plan)
    print(landscape.calculate_area())


if __name__ == "__main__":
    main()
