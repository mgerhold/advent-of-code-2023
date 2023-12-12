from dataclasses import dataclass


@dataclass
class Record:
    conditions: str
    groups: list[int]


def parse_line(line: str) -> Record:
    parts = line.split(" ")
    conditions = parts[0]
    groups = [int(number) for number in parts[1].split(",")]
    return Record(conditions, groups)


def matches(substring: str, conditions: str) -> bool:
    assert len(substring) <= len(conditions)
    for a, b in zip(substring, conditions):
        if b != "?" and a != b:
            return False
    return True


def generate_possibilities(record: Record, intact_springs_to_distribute: int, index: int = 0,
                           base_pattern: str = "") -> int:
    if index == len(record.groups):
        pattern = base_pattern + "." * intact_springs_to_distribute
        if matches(pattern, record.conditions):
            # print(pattern)
            return 1
        return 0
    is_at_edge = (index in (0, len(record.groups)))

    result = 0
    for distributed in range(intact_springs_to_distribute, -1, -1):
        pattern = base_pattern
        if not is_at_edge:
            pattern += "."
        remaining_springs = intact_springs_to_distribute - distributed
        pattern += "." * distributed
        if index < len(record.groups):
            pattern += "#" * record.groups[index]
        if matches(pattern, record.conditions):
            result += generate_possibilities(record, remaining_springs, index + 1, pattern)
    return result


def process_record(record: Record) -> int:
    conditions_length = len(record.conditions)
    num_damaged_springs = sum(record.groups)
    num_gaps = len(record.groups) - 1
    num_intact_springs_to_distribute = conditions_length - num_damaged_springs - num_gaps
    return generate_possibilities(record, num_intact_springs_to_distribute)


data = [parse_line(line) for line in open("data.txt")]
print(sum(process_record(record) for record in data))
