from __future__ import annotations

import copy
from enum import Enum
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


class PlotType(Enum):
    GARDEN = "."
    ROCK = "#"
    START = "S"
    STEPPED_ON = "O"


class Map:
    def __init__(self, data: str) -> None:
        self._lines = [[PlotType(char) for char in line] for line in data.splitlines()]

    @property
    def width(self) -> int:
        return len(self._lines[0])

    @property
    def height(self) -> int:
        return len(self._lines)

    def count_plots_by_type(self, plot_type: PlotType) -> int:
        return sum(sum(1 for plot in line if plot == plot_type) for line in self._lines)

    def is_valid_coordinate(self, coordinate: Vec2) -> bool:
        return (
                coordinate.x in range(self.width)
                and coordinate.y in range(self.height)
                and self._lines[coordinate.y][coordinate.x] != PlotType.ROCK
        )

    def find(self, plot_type: PlotType) -> Optional[Vec2]:
        for y, line in enumerate(self._lines):
            for x, plot in enumerate(line):
                if plot_type == plot:
                    return Vec2(x, y)
        return None

    def _clear_stepped_on_plots(self) -> None:
        for line in self._lines:
            for x, plot in enumerate(line):
                if plot == PlotType.STEPPED_ON:
                    line[x] = PlotType.GARDEN

    def step(self, original_garden_state: Map) -> Map:
        new_state = copy.deepcopy(original_garden_state)
        for y, line in enumerate(self._lines):
            for x, plot in enumerate(line):
                if not new_state.is_valid_coordinate(Vec2(x, y)):
                    continue
                neighbor_positions = list(filter(
                    lambda position: self.is_valid_coordinate(position),
                    (Vec2(x, y) + offset
                     for offset
                     in (Vec2(0, 1), Vec2(0, -1), Vec2(-1, 0), Vec2(1, 0)))
                ))
                if any(self[position] == PlotType.STEPPED_ON for position in neighbor_positions):
                    new_state[Vec2(x, y)] = PlotType.STEPPED_ON
        return new_state

    def __deepcopy__(self, original: Map) -> Map:
        new_map_instance = Map("")
        new_map_instance._lines = copy.deepcopy(self._lines)
        return new_map_instance

    def __getitem__(self, coordinate: Vec2) -> PlotType:
        if not self.is_valid_coordinate(coordinate):
            raise ValueError(f"{coordinate} is not a valid coordinate")
        return self._lines[coordinate.y][coordinate.x]

    def __setitem__(self, coordinate: Vec2, plot_type: PlotType) -> None:
        if not self.is_valid_coordinate(coordinate):
            raise ValueError(f"{coordinate} is not a valid coordinate")
        self._lines[coordinate.y][coordinate.x] = plot_type

    def __str__(self) -> str:
        return "\n".join("".join(plot_type.value for plot_type in line) for line in self._lines)


def main() -> None:
    garden = Map(open("data.txt").read())
    start_position = garden.find(PlotType.START)
    assert start_position is not None
    print(start_position)
    garden[start_position] = PlotType.GARDEN

    current_state = copy.deepcopy(garden)
    current_state[start_position] = PlotType.STEPPED_ON
    # current_state[Vec2(0, 65)] = PlotType.STEPPED_ON

    counter = 0
    visited = set()
    left_edge_reached = False
    right_edge_reached = False
    top_edge_reached = False
    bottom_edge_reached = False
    for _ in range(65 + 131):
        # if all((left_edge_reached, right_edge_reached, top_edge_reached, bottom_edge_reached)):
        #     break
        # if not left_edge_reached and sum(int(current_state[Vec2(0, y)] == PlotType.STEPPED_ON) for y in range(current_state.height)) == 1:
        #     for y in range(current_state.height):
        #         if current_state[Vec2(0, y)] == PlotType.STEPPED_ON:
        #             print(f"left edge reached at {Vec2(0, y)} after {counter} steps")
        #             left_edge_reached = True
        # if not right_edge_reached and sum(int(current_state[Vec2(current_state.width - 1, y)] == PlotType.STEPPED_ON) for y in
        #        range(current_state.height)) == 1:
        #     for y in range(current_state.height):
        #         if current_state[Vec2(current_state.width - 1, y)] == PlotType.STEPPED_ON:
        #             print(f"right edge reached at {Vec2(current_state.width - 1, y)} after {counter} steps")
        #             right_edge_reached = True
        # if not top_edge_reached and sum(int(current_state[Vec2(x, 0)] == PlotType.STEPPED_ON) for x in range(current_state.width)) == 1:
        #     for x in range(current_state.width):
        #         if current_state[Vec2(x, 0)] == PlotType.STEPPED_ON:
        #             print(f"upper edge reached at {Vec2(x, 0)} after {counter} steps")
        #             top_edge_reached = True
        # if not bottom_edge_reached and sum(int(current_state[Vec2(x, current_state.height - 1)] == PlotType.STEPPED_ON) for x in
        #        range(current_state.width)) == 1:
        #     for x in range(current_state.width):
        #         if current_state[Vec2(x, current_state.height - 1)] == PlotType.STEPPED_ON:
        #             print(f"lower edge reached at {Vec2(x, current_state.height - 1)} after {counter} steps")
        #             bottom_edge_reached = True
        # if (
        #         current_state[Vec2(0, 0)] == PlotType.STEPPED_ON
        #         and current_state[Vec2(garden.width - 1, 0)] == PlotType.STEPPED_ON
        #         and current_state[Vec2(garden.width - 1, garden.height - 1)] == PlotType.STEPPED_ON
        #         and current_state[Vec2(0, garden.height - 1)] == PlotType.STEPPED_ON
        # ):
        #     break
        current_state = current_state.step(garden)
        for y in range(current_state.height):
            for x in range(current_state.width):
                position = Vec2(x, y)
                if current_state.is_valid_coordinate(position) and current_state[position] == PlotType.STEPPED_ON:
                    visited.add(position)
        counter += 1

    print("positions that have not been reached:")
    for y in range(current_state.height):
        for x in range(current_state.width):
            position = Vec2(x, y)
            if current_state.is_valid_coordinate(position) and position not in visited:
                print(f"    {position = }")

    print(current_state)

    print(f"{counter = }")
    stepped_on_even = current_state.count_plots_by_type(PlotType.STEPPED_ON)
    print(f"{stepped_on_even = }")

    current_state = current_state.step(garden)
    stepped_on_odd = current_state.count_plots_by_type(PlotType.STEPPED_ON)
    print(f"{stepped_on_odd = }")

    current_state = current_state.step(garden)
    assert current_state.count_plots_by_type(PlotType.STEPPED_ON) == stepped_on_even

    # num_rocks = current_state.count_plots_by_type(PlotType.ROCK)
    # num_plots = current_state.width * current_state.height
    # print(f"{num_plots - num_rocks - stepped_on_odd - stepped_on_even = }")




if __name__ == "__main__":
    main()
