from typing import NamedTuple


def reindeer_hash(s: str) -> int:
    value = 0
    for char in s:
        value += ord(char)
        value *= 17
        value %= 256
    return value


class HashMap:
    def __init__(self) -> None:
        self._boxes: list[list[tuple[str, int]]] = [[] for _ in range(256)]

    def insert(self, lens_label: str, focal_length: int) -> None:
        index = reindeer_hash(lens_label)
        box = self._boxes[index]
        for i, (label, length) in enumerate(box):
            if label == lens_label:
                box[i] = label, focal_length
                return
        box.append((lens_label, focal_length))

    def remove(self, lens_label: str) -> None:
        index = reindeer_hash(lens_label)
        box = self._boxes[index]
        for i, (label, length) in enumerate(box):
            if label == lens_label:
                del box[i]

    def __str__(self) -> str:
        result = ""
        for i, box in ((i, box) for i, box in enumerate(self._boxes) if len(box) > 0):
            result += f"Box {i}:"
            for lens_label, focal_length in box:
                result += f" [{lens_label} {focal_length}]"
            result += "\n"
        return result

    def focusing_power(self) -> int:
        power = 0
        for i, box in enumerate(self._boxes):
            for j, (_, focal_length) in enumerate(box):
                power += (1 + i) * (1 + j) * focal_length
        return power


class Delete:
    pass


class Insert(NamedTuple):
    focal_length: int


class Instruction(NamedTuple):
    label: str
    what: Delete | Insert


def parse_instruction(instruction: str) -> Instruction:
    if instruction.endswith("-"):
        return Instruction(label=instruction[:-1], what=Delete())
    if (equals_pos := instruction.find("=")) >= 0:
        return Instruction(label=instruction[:equals_pos], what=Insert(int(instruction[equals_pos + 1:])))
    raise Exception("unreachable")


def execute_instruction(hash_map: HashMap, instruction: Instruction) -> None:
    label = instruction.label
    match instruction.what:
        case Delete():
            hash_map.remove(label)
        case Insert() as insert:
            hash_map.insert(label, insert.focal_length)


def main() -> None:
    parts = open("data.txt").read().split(",")

    print("part 1:")
    print(sum(reindeer_hash(instruction) for instruction in parts))

    print("part 2:")
    hash_map = HashMap()
    for instruction in (parse_instruction(part) for part in parts):
        execute_instruction(hash_map, instruction)
    print(hash_map.focusing_power())


if __name__ == "__main__":
    main()
