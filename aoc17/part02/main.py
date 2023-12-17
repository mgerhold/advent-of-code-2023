from __future__ import annotations

import heapq
from enum import Enum
from functools import total_ordering
from typing import NamedTuple
from typing import Optional
from typing import override


class Vec2(NamedTuple):
    x: int
    y: int

    @override
    def __add__(self, other: object) -> Vec2:
        if not isinstance(other, Vec2):
            raise NotImplemented
        return Vec2(self.x + other.x, self.y + other.y)


class Direction(Enum):
    UP = Vec2(0, -1)
    DOWN = Vec2(0, 1)
    LEFT = Vec2(-1, 0)
    RIGHT = Vec2(1, 0)


class Map:
    def __init__(self, data: str) -> None:
        self._lines = [[int(char) for char in line] for line in data.splitlines()]

    @property
    def width(self) -> int:
        return len(self._lines[0])

    @property
    def height(self) -> int:
        return len(self._lines)

    def is_inside_map(self, position: Vec2) -> bool:
        return 0 <= position.x < self.width and 0 <= position.y < self.height

    def with_path(self, destination_node: Node) -> str:
        path: list[Node] = []
        node: Optional[Node] = destination_node
        while node is not None:
            path.append(node)
            node = node.predecessor
        path.reverse()

        def node_symbol(n: Node) -> str:
            match n.movement:
                case None:
                    return "?"
                case _, length:
                    assert length > 0
                    return chr(ord("A") - 1 + length)
                case _:
                    assert False, "unreachable"

        return "\n".join(
            "".join(
                str(cost) if (path_node := next((n for n in path if n.position == Vec2(x, y)),
                                                None)) is None else node_symbol(path_node)
                # node_symbol() if Vec2(x, y) in path else str(cost)
                for x, cost
                in enumerate(line)
            )
            for y, line
            in enumerate(self._lines)
        )

    def __getitem__(self, position: Vec2) -> int:
        if not self.is_inside_map(position):
            raise Exception("position outside of map")
        return self._lines[position.y][position.x]

    def __setitem__(self, position: Vec2, cost: int) -> None:
        if not self.is_inside_map(position):
            raise Exception("position outside of map")
        self._lines[position.y][position.x] = cost

    def __str__(self) -> str:
        return "\n".join("".join(str(char) for char in line) for line in self._lines)


@total_ordering
class Node(NamedTuple):
    cost: int
    heuristic: int
    position: Vec2
    predecessor: Optional[Node]
    movement: Optional[tuple[Direction, int]]

    @property
    def total_cost(self) -> int:
        return self.cost + self.heuristic

    @override
    def __lt__(self, other: object) -> bool:
        if not isinstance(other, Node):
            return NotImplemented
        return self.total_cost < other.total_cost

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Node):
            return False
        return self.position == other.position and self.movement == other.movement

    def __hash__(self) -> int:
        return hash((self.position, self.movement))


def heuristic(current: Vec2, destination: Vec2) -> int:
    return abs(current.x - destination.x) + abs(current.y - destination.y)


def neighbors(city: Map, current_node: Node, destination: Vec2) -> set[Node]:
    assert current_node.movement is None or current_node.movement[1] <= 10
    match current_node.movement:
        case None:
            neighbor_directions = [Direction.RIGHT, Direction.DOWN]
            new_direction_counts = [1, 1]
        case direction, length if length < 4:
            neighbor_directions = [direction]
            new_direction_counts = [length + 1]
        case Direction.RIGHT, length if length < 10:
            neighbor_directions = [Direction.RIGHT, Direction.DOWN, Direction.UP]
            new_direction_counts = [length + 1, 1, 1]
        case Direction.RIGHT, _:
            neighbor_directions = [Direction.DOWN, Direction.UP]
            new_direction_counts = [1, 1]
        case Direction.LEFT, length if isinstance(length, int) and length < 10:
            neighbor_directions = [Direction.LEFT, Direction.UP, Direction.DOWN]
            new_direction_counts = [length + 1, 1, 1]
        case Direction.LEFT, _:
            neighbor_directions = [Direction.UP, Direction.DOWN]
            new_direction_counts = [1, 1]
        case Direction.UP, length if isinstance(length, int) and length < 10:
            neighbor_directions = [Direction.UP, Direction.RIGHT, Direction.LEFT]
            new_direction_counts = [length + 1, 1, 1]
        case Direction.UP, _:
            neighbor_directions = [Direction.RIGHT, Direction.LEFT]
            new_direction_counts = [1, 1]
        case Direction.DOWN, length if isinstance(length, int) and length < 10:
            neighbor_directions = [Direction.DOWN, Direction.LEFT, Direction.RIGHT]
            new_direction_counts = [length + 1, 1, 1]
        case Direction.DOWN, _:
            neighbor_directions = [Direction.LEFT, Direction.RIGHT]
            new_direction_counts = [1, 1]
        case _:
            raise Exception(f"invalid movement {current_node.movement}")

    return {
        Node(
            cost=current_node.cost + city[current_node.position + direction.value],
            heuristic=heuristic(current_node.position + direction.value, destination),
            position=current_node.position + direction.value,
            predecessor=current_node,
            movement=(direction, count)
        ) for direction, count in zip(
            neighbor_directions,
            new_direction_counts, strict=True
        )
        if city.is_inside_map(current_node.position + direction.value)
    }


def a_star(city: Map, start: Vec2, destination: Vec2) -> Node:
    open_list: list[Node] = []  # known nodes (but maybe with suboptimal path)
    heapq.heappush(
        open_list,
        Node(
            cost=0,
            heuristic=heuristic(start, destination),
            position=start,
            predecessor=None,
            movement=None
        )
    )
    closed_set = set()  # nodes which we know the definitive minimum cost of
    while len(open_list) > 0:
        current_node = heapq.heappop(open_list)
        if current_node in closed_set:
            continue

        if current_node.position == destination and current_node.movement is not None and current_node.movement[1] >= 4:
            return current_node

        closed_set.add(current_node)

        for neighbor in neighbors(city, current_node, destination):
            if neighbor in closed_set:
                continue

            heapq.heappush(open_list, neighbor)

    raise Exception("No path found")


def main() -> None:
    city = Map(open("data.txt").read())
    destination_node = a_star(city=city, start=Vec2(0, 0), destination=Vec2(city.width - 1, city.height - 1))
    print(city.with_path(destination_node))
    print(destination_node.cost)


if __name__ == "__main__":
    main()
