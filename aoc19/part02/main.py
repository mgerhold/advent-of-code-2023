from __future__ import annotations

from enum import Enum
from typing import NamedTuple
from typing import override
from typing import Self

from interval3 import Interval
from interval3 import IntervalSet


class Category(Enum):
    EXTREMELY_COOL_LOOKING = "x"
    MUSICAL = "m"
    AERODYNAMIC = "a"
    SHINY = "s"


class ComparisonType(Enum):
    LESS_THAN = "<"
    GREATER_THAN = ">"


class WorkflowReference(NamedTuple):
    workflow_name: str

    @override
    def __str__(self) -> str:
        return self.workflow_name


class Accepted:
    @override
    def __str__(self) -> str:
        return "A"


class Rejected:
    @override
    def __str__(self) -> str:
        return "R"


def parse_result(result: str) -> WorkflowReference | Accepted | Rejected:
    match result.strip():
        case "A":
            return Accepted()
        case "R":
            return Rejected()
        case workflow_name:
            return WorkflowReference(workflow_name)


class Xmas(NamedTuple):
    x: IntervalSet
    m: IntervalSet
    a: IntervalSet
    s: IntervalSet


class Branch(NamedTuple):
    category: Category
    comparison: ComparisonType
    rhs: int
    result: WorkflowReference | Accepted | Rejected

    @classmethod
    def from_string(cls, string: str) -> Self:
        if (i := string.find(">")) >= 0:
            comparison_type = ComparisonType.GREATER_THAN
        elif (i := string.find("<")) >= 0:
            comparison_type = ComparisonType.LESS_THAN
        else:
            raise ValueError(f"Invalid comparison type: {string}")
        category = Category(string[:i])
        rhs_string, result_string = string[i + 1:].split(":")
        rhs = int(rhs_string)
        result = parse_result(result_string)
        return cls(category, comparison_type, rhs, result)

    @override
    def __str__(self) -> str:
        return f"{self.category.value}{self.comparison.value}{self.rhs}:{self.result}"


class Workflow(NamedTuple):
    name: str
    branches: list[Branch]
    default_case: WorkflowReference | Accepted | Rejected

    def execute(self, workflows: dict[str, Self], xmas: Xmas) -> list[Xmas]:
        results: list[Xmas] = []
        default_case_interval = xmas
        for branch in self.branches:
            match branch.category, branch.comparison:
                case Category.EXTREMELY_COOL_LOOKING, ComparisonType.GREATER_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x.intersection(IntervalSet.greater_than(branch.rhs)),
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x.intersection(IntervalSet.less_than_or_equal_to(branch.rhs)),
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s
                    )
                case Category.EXTREMELY_COOL_LOOKING, ComparisonType.LESS_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x.intersection(IntervalSet.less_than(branch.rhs)),
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x.intersection(IntervalSet.greater_than_or_equal_to(branch.rhs)),
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s
                    )
                case Category.MUSICAL, ComparisonType.GREATER_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m.intersection(IntervalSet.greater_than(branch.rhs)),
                        default_case_interval.a,
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m.intersection(IntervalSet.less_than_or_equal_to(branch.rhs)),
                        default_case_interval.a,
                        default_case_interval.s
                    )
                case Category.MUSICAL, ComparisonType.LESS_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m.intersection(IntervalSet.less_than(branch.rhs)),
                        default_case_interval.a,
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m.intersection(IntervalSet.greater_than_or_equal_to(branch.rhs)),
                        default_case_interval.a,
                        default_case_interval.s
                    )
                case Category.AERODYNAMIC, ComparisonType.GREATER_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a.intersection(IntervalSet.greater_than(branch.rhs)),
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a.intersection(IntervalSet.less_than_or_equal_to(branch.rhs)),
                        default_case_interval.s
                    )
                case Category.AERODYNAMIC, ComparisonType.LESS_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a.intersection(IntervalSet.less_than(branch.rhs)),
                        default_case_interval.s
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a.intersection(IntervalSet.greater_than_or_equal_to(branch.rhs)),
                        default_case_interval.s
                    )
                case Category.SHINY, ComparisonType.GREATER_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s.intersection(IntervalSet.greater_than(branch.rhs)),
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s.intersection(IntervalSet.less_than_or_equal_to(branch.rhs)),
                    )
                case Category.SHINY, ComparisonType.LESS_THAN:
                    temp_xmas = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s.intersection(IntervalSet.less_than(branch.rhs)),
                    )
                    default_case_interval = Xmas(
                        default_case_interval.x,
                        default_case_interval.m,
                        default_case_interval.a,
                        default_case_interval.s.intersection(IntervalSet.greater_than_or_equal_to(branch.rhs)),
                    )
                case _:
                    assert False, "unreachable"

            if isinstance(branch.result, WorkflowReference):
                results.extend(workflows[branch.result.workflow_name].execute(workflows, temp_xmas))
            elif isinstance(branch.result, Accepted):
                results.append(temp_xmas)
            else:
                assert isinstance(branch.result, Rejected)

        if isinstance(self.default_case, WorkflowReference):
            results.extend(workflows[self.default_case.workflow_name].execute(workflows, default_case_interval))
        elif isinstance(self.default_case, Accepted):
            results.append(default_case_interval)
        else:
            assert isinstance(self.default_case, Rejected)

        return results

    @classmethod
    def from_string(cls, string: str) -> Self:
        name, branches_string = string.strip().split("{")
        assert isinstance(name, str)
        assert isinstance(branches_string, str)
        branches_string = branches_string.rstrip("}")
        branches_string_parts = branches_string.split(",")
        branches = [Branch.from_string(s) for s in branches_string_parts[:-1]]
        default_case = parse_result(branches_string_parts[-1])
        return cls(name, branches, default_case)

    @override
    def __str__(self) -> str:
        return f"{self.name}{{{",".join(str(branch) for branch in self.branches)},{self.default_case}}}"


def main() -> None:
    workflow_string, shape_string = open("data.txt").read().split("\n\n")
    workflow_list = [Workflow.from_string(line) for line in workflow_string.splitlines()]
    workflows = {workflow.name: workflow for workflow in workflow_list}
    results = workflows["in"].execute(workflows, Xmas(
        x=IntervalSet([Interval(1, 4000)]),
        m=IntervalSet([Interval(1, 4000)]),
        a=IntervalSet([Interval(1, 4000)]),
        s=IntervalSet([Interval(1, 4000)])
    ))
    total_count = 0
    for result in results:
        assert len(result.x) == 1 and len(result.m) == 1 and len(result.a) == 1 and len(result.s) == 1

        product = 1

        lower = result.x.lower_bound() if result.x.lower_closed() else result.x.lower_bound() + 1
        upper = result.x.upper_bound() if result.x.upper_closed() else result.x.upper_bound() - 1
        product *= (upper - lower + 1)

        lower = result.m.lower_bound() if result.m.lower_closed() else result.m.lower_bound() + 1
        upper = result.m.upper_bound() if result.m.upper_closed() else result.m.upper_bound() - 1
        product *= (upper - lower + 1)

        lower = result.a.lower_bound() if result.a.lower_closed() else result.a.lower_bound() + 1
        upper = result.a.upper_bound() if result.a.upper_closed() else result.a.upper_bound() - 1
        product *= (upper - lower + 1)

        lower = result.s.lower_bound() if result.s.lower_closed() else result.s.lower_bound() + 1
        upper = result.s.upper_bound() if result.s.upper_closed() else result.s.upper_bound() - 1
        product *= (upper - lower + 1)

        total_count += product
    print(total_count)


if __name__ == "__main__":
    main()
