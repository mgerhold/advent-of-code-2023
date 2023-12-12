from typing import NamedTuple
from functools import cache


class Record(NamedTuple):
    conditions: str
    groups: tuple[int, ...]


def parse_line(line: str) -> Record:
    parts = line.split(" ")
    conditions = "?".join(parts[0] for _ in range(5))
    groups = tuple([int(number) for number in parts[1].split(",")] * 5)
    return Record(conditions, groups)


def matches(substring: str, conditions: str) -> bool:
    assert len(substring) <= len(conditions)
    return not any(b != "?" and a != b for a, b in zip(substring, conditions))


@cache
def generate_possibilities(record: Record, intact_springs_to_distribute: int, index: int = 0,
                           conditions_offset: int = 0) -> int:
    if index == len(record.groups):
        return int(matches("." * intact_springs_to_distribute, record.conditions[conditions_offset:]))

    is_at_edge = (index in (0, len(record.groups)))

    result = 0
    for distributed in range(intact_springs_to_distribute + 1):
        pattern = "." * (distributed + int(not is_at_edge))

        remaining_springs = intact_springs_to_distribute - distributed
        if index < len(record.groups):
            pattern += "#" * record.groups[index]
        if matches(pattern, record.conditions[conditions_offset:]):
            result += generate_possibilities(record, remaining_springs, index + 1, conditions_offset + len(pattern))
    return result


def process_record(record: Record) -> int:
    conditions_length = len(record.conditions)
    num_damaged_springs = sum(record.groups)
    num_gaps = len(record.groups) - 1
    num_intact_springs_to_distribute = conditions_length - num_damaged_springs - num_gaps
    return generate_possibilities(record, num_intact_springs_to_distribute)


data = [parse_line(line) for line in open("data.txt")]
total = 0
for result in (process_record(record) for record in data):
    total += result
print(f"{total = }")
