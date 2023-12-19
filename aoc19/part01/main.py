import os
import subprocess
from enum import Enum
from typing import NamedTuple
from typing import override
from typing import Self


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

    def emit(self) -> str:
        return f"{self.workflow_name}(x, m, a, s)"

    @override
    def __str__(self) -> str:
        return self.workflow_name


class Accepted:
    def emit(self) -> str:
        return "true"

    @override
    def __str__(self) -> str:
        return "A"


class Rejected:
    def emit(self) -> str:
        return "false"

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

    def emit_forward_declaration(self) -> str:
        return f"[[nodiscard]] static consteval bool {self.name}([[maybe_unused]] std::uint64_t x, [[maybe_unused]] std::uint64_t m, [[maybe_unused]] std::uint64_t a, [[maybe_unused]] std::uint64_t s);"

    def emit_definition(self) -> str:
        result = f"{self.emit_forward_declaration()[:-1]} {{\n"
        for branch in self.branches:
            result += f"    if ({branch.category.value} {branch.comparison.value} {branch.rhs}) {{\n"
            result += f"        return {branch.result.emit()};\n"
            result += f"    }}\n"
        result += f"    return {self.default_case.emit()};\n"
        result += f"}}"
        return result

    @override
    def __str__(self) -> str:
        return f"{self.name}{{{",".join(str(branch) for branch in self.branches)},{self.default_case}}}"


class Shape(NamedTuple):
    x: int
    m: int
    a: int
    s: int

    @classmethod
    def from_str(cls, string: str) -> Self:
        string = string.strip().lstrip("{").rstrip("}")
        x, m, a, s = (int(part.split("=")[-1]) for part in string.split(","))
        return cls(x, m, a, s)


def main() -> None:
    workflow_string, shape_string = open("data.txt").read().split("\n\n")
    workflows = [Workflow.from_string(line) for line in workflow_string.splitlines()]
    shapes = [Shape.from_str(line) for line in shape_string.splitlines()]
    print("\n".join(str(workflow) for workflow in workflows))
    print("\n".join(str(shape) for shape in shapes))
    cpp_source = f"""#include <iostream>

{"\n".join(workflow.emit_forward_declaration() for workflow in workflows)}

{"\n\n".join(workflow.emit_definition() for workflow in workflows)}

int main() {{
    static constinit auto sum = std::uint64_t{{
        {"\n        + ".join(f"(in({shape.x}, {shape.m}, {shape.a}, {shape.s}) ? {shape.x} + {shape.m} + {shape.a} + {shape.s} : 0)" for shape in shapes)}
    }};
"""

    cpp_source += "    std::cout << sum << '\\n';\n}\n"

    print(cpp_source)
    with open("main.cpp", "w") as file:
        file.write(cpp_source)

    # Compiling and execute the C++ code
    subprocess.run(
        ["g++", "-O3", "-std=c++2b", "-Wall", "-Wextra", "-Wconversion", "-Wshadow", "-pedantic", "main.cpp", "-o",
         "main.exe"],
        check=True)
    subprocess.run(["./main.exe"], check=True)

    os.unlink("main.cpp")
    os.unlink("main.exe")


if __name__ == "__main__":
    main()
