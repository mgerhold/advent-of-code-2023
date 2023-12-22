from __future__ import annotations

import copy
import functools
import heapq
from dataclasses import dataclass
from enum import auto
from enum import Enum
from itertools import product
from typing import Generator
from typing import NamedTuple
from typing import override
from typing import Self


class Vec3(NamedTuple):
    x: int
    y: int
    z: int

    @classmethod
    def from_str(cls, string: str) -> Self:
        x, y, z = string.strip().split(",")
        return cls(int(x), int(y), int(z))

    @override
    def __add__(self, other: object) -> Vec3:
        if not isinstance(other, Vec3):
            return NotImplemented
        return Vec3(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: object) -> Vec3:
        if not isinstance(other, Vec3):
            return NotImplemented
        return Vec3(self.x - other.x, self.y - other.y, self.z - other.z)


class NameGenerator:
    def __init__(self) -> None:
        self._next = 0

    def __next__(self) -> str:
        result = f"BRICK_{self._next}"
        self._next += 1
        return result


@functools.total_ordering
@dataclass
class Brick():
    name: str
    position: Vec3
    size: Vec3

    @classmethod
    def from_str(cls, string: str, name_generator: NameGenerator) -> Self:
        start, end = string.strip().split("~")
        position = Vec3.from_str(start)
        size = Vec3.from_str(end) - position + Vec3(1, 1, 1)
        assert size.x >= 1 and size.y >= 1 and size.z >= 1
        return cls(next(name_generator), position, size)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, Brick):
            return NotImplemented
        return self.position.z < other.position.z

    @property
    def xs(self) -> range:
        return range(self.position.x, self.position.x + self.size.x)

    @property
    def ys(self) -> range:
        return range(self.position.y, self.position.y + self.size.y)

    @property
    def zs(self) -> range:
        return range(self.position.z, self.position.z + self.size.z)

    @property
    def blocks(self) -> Generator[Vec3, None, None]:
        return (Vec3(x, y, z) for x, y, z in product(self.xs, self.ys, self.zs))


class Axis(Enum):
    X = auto()
    Y = auto()
    Z = auto()


class VisualizationKind(Enum):
    XZ = auto()
    YZ = auto()


def determine_range(axis: Axis, matrix: dict[Vec3, Brick]) -> range:
    match axis:
        case Axis.X:
            min_ = min(position.x for position in matrix.keys())
            max_ = max(position.x for position in matrix.keys())
        case Axis.Y:
            min_ = min(position.y for position in matrix.keys())
            max_ = max(position.y for position in matrix.keys())
        case Axis.Z:
            min_ = min(position.z for position in matrix.keys())
            max_ = max(position.z for position in matrix.keys())
        case _:
            raise ValueError(f"no brick found")
    return range(min_, max_ + 1)


def visualize(kind: VisualizationKind, matrix: dict[Vec3, Brick]) -> None:
    abscissa_range = determine_range(Axis.X if kind == VisualizationKind.XZ else Axis.Y, matrix)
    non_abscissa_range = determine_range(Axis.Y if kind == VisualizationKind.XZ else Axis.X, matrix)
    ordinate_range = determine_range(Axis.Z, matrix)
    if kind == VisualizationKind.XZ:
        print(" x")
    else:
        print(" y")
    print("".join(str(number) for number in abscissa_range))
    for z in reversed(ordinate_range):
        for abscissa in abscissa_range:
            contains_brick = False
            for non_abscissa in non_abscissa_range:
                position = (Vec3(abscissa, non_abscissa, z)
                            if kind == VisualizationKind.XZ
                            else Vec3(non_abscissa, abscissa, z))
                if position in matrix:
                    print(f"{chr(int(matrix[position].name[-1]) + ord("A"))}", end="")
                    contains_brick = True
                    break
            if not contains_brick:
                print(".", end="")
        print(f" {z}")
    print(f"{"-" * len(abscissa_range)} 0")


def drop_brick(brick: Brick, matrix: dict[Vec3, Brick]) -> set[str]:
    def is_blocked_by(depth: int) -> set[str]:
        blocking_bricks: set[str] = set()
        for x, y in product(brick.xs, brick.ys):
            position = Vec3(x, y, brick.position.z - depth)
            if position.z < 1:
                return {"GROUND"}
            if position in matrix:
                blocking_bricks.add(matrix[position].name)
        return blocking_bricks

    drop_depth = 1
    while len(result := is_blocked_by(drop_depth)) == 0:
        drop_depth += 1
    drop_depth -= 1

    offset = Vec3(0, 0, -drop_depth)
    for position in brick.blocks:
        matrix.pop(position)
        matrix[position + offset] = brick

    brick.position += offset

    return result


def get_supported_bricks(brick: Brick, matrix: dict[Vec3, Brick]) -> set[str]:
    return {
        matrix[position].name
        for x, y
        in product(brick.xs, brick.ys)
        if (position := Vec3(x, y, brick.position.z + brick.size.z)) in matrix
    }


def get_num_dropping_bricks(
        brick: Brick,
        matrix: dict[Vec3, Brick],
        bricks_below: dict[str, set[str]],
        bricks_by_name: dict[str, Brick],
        indentation: int = 0
) -> int:
    # print(f"{" " * indentation}looking at brick {brick.name}")
    supported_bricks = get_supported_bricks(brick, matrix)
    # print(f"{" " * indentation}  brick supports {len(supported_bricks)} bricks: {", ".join(supported_bricks)}")
    result = 0

    for position in brick.blocks:
        matrix.pop(position)
    for bricks in bricks_below.values():
        bricks.discard(brick.name)

    for supported_brick in supported_bricks:
        # print(f"{" " * indentation}    supported brick {supported_brick}")
        supporting_bricks = bricks_below[supported_brick]
        if len(supporting_bricks) > 0:
            # print(
            #     f"{" " * indentation}    this brick is supported by at least one other brick: {", ".join(supporting_bricks)}")
            continue
        # print(f"{" " * indentation}    this brick is not supported by any other brick")

        result += 1 + get_num_dropping_bricks(
            bricks_by_name[supported_brick],
            matrix,
            bricks_below,
            bricks_by_name,
            indentation + 6
        )
    return result


def main() -> None:
    name_generator = NameGenerator()
    bricks = [Brick.from_str(line, name_generator) for line in open("data.txt")]
    print("\n".join(str(brick) for brick in bricks))

    matrix: dict[Vec3, Brick] = dict()
    for brick in bricks:
        for position in brick.blocks:
            matrix[position] = brick

    queue = bricks[:]
    heapq.heapify(queue)

    bricks_below: dict[str, set[str]] = dict()

    while len(queue) > 0:
        brick = heapq.heappop(queue)
        landed_on = drop_brick(brick, matrix)
        landed_on.discard("GROUND")
        bricks_below[brick.name] = landed_on

    visualize(VisualizationKind.XZ, matrix)

    bricks_by_name: dict[str, Brick] = {brick.name: brick for brick in bricks}

    result = 0
    for brick in bricks:
        num_falling_bricks = get_num_dropping_bricks(
            brick,
            copy.deepcopy(matrix),
            copy.deepcopy(bricks_below),
            bricks_by_name
        )
        result += num_falling_bricks
        print(f"{brick.name}: {num_falling_bricks = }")

    print(f"{result = }")


if __name__ == "__main__":
    main()
