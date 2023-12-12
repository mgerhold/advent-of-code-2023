from typing import NamedTuple


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
    for a, b in zip(substring, conditions):
        if b != "?" and a != b:
            return False
    return True


caches: dict[Record, dict[tuple[Record, int, int], int]] = dict()


def generate_possibilities(record: Record, intact_springs_to_distribute: int, index: int = 0,
                           base_pattern: str = "") -> int:
    if index == len(record.groups):
        pattern = base_pattern + "." * intact_springs_to_distribute
        if matches(pattern, record.conditions):
            print(pattern)
            return 1
        return 0
    is_at_edge = (index in (0, len(record.groups)))
    if not is_at_edge:
        base_pattern += "."

    result = 0
    for distributed in range(intact_springs_to_distribute + 1):
        if distributed > 0:
            base_pattern += "."
        pattern = base_pattern
        remaining_springs = intact_springs_to_distribute - distributed
        if index < len(record.groups):
            pattern += "#" * record.groups[index]
        if matches(pattern, record.conditions):
            if record not in caches:
                caches[record] = dict()
            cache = caches[record]
            args = (record, remaining_springs, index + 1)
            if args in cache:
                result += cache[args]
            else:
                sub_result = generate_possibilities(record, remaining_springs, index + 1, pattern)
                cache[args] = sub_result
                result += sub_result
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
    print(result)
print(f"{total = }")
