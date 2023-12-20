from __future__ import annotations

from abc import ABC
from abc import abstractmethod
from collections import deque
from enum import auto
from enum import Enum
from typing import final
from typing import Final
from typing import NamedTuple
from typing import Optional
from typing import override


class Pulse(Enum):
    LOW = auto()
    HIGH = auto()
    NONE = auto()


class Transmission(NamedTuple):
    sender: str
    receiver: str
    pulse: Pulse


class Module(ABC):
    _transmission_queue: deque[Transmission] = deque()
    _low_pulse_count = 0
    _high_pulse_count = 0
    button_pressed_timestamps: list[int] = []

    @staticmethod
    def low_pulse_count() -> int:
        return Module._low_pulse_count

    @staticmethod
    def high_pulse_count() -> int:
        return Module._high_pulse_count

    @staticmethod
    def pulse_count() -> int:
        return Module.high_pulse_count() + Module.low_pulse_count()

    @staticmethod
    def process_until_end(modules: dict[str, Module]) -> None:
        while len(Module._transmission_queue) > 0:
            transmission = Module._transmission_queue.popleft()
            modules[transmission.receiver]._handle_pulse(transmission.sender, transmission.pulse)

    def __init__(self, id_: str, destinations: list[str]) -> None:
        self._id = id_
        self._destinations = destinations
        self._sources: list[str] = []

    @property
    def id(self) -> str:
        return self._id

    @property
    def destinations(self) -> list[str]:
        return self._destinations

    @property
    def sources(self) -> list[str]:
        return self._sources

    def initialize(self, modules: dict[str, Module]) -> None:
        for module in modules.values():
            for i, destination in enumerate(module.destinations):
                if destination not in modules.keys():
                    print(f"WARNING: re-wiring output '{destination}' to 'output'")
                    module.destinations[i] = "output"

        for module in modules.values():
            if self.id in module.destinations:
                self._sources.append(module.id)

    @abstractmethod
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        pass

    @final
    def _send_pulse(self, pulse: Pulse) -> None:
        if pulse == Pulse.NONE:
            return
        for destination_module_id in self._destinations:
            if pulse == Pulse.LOW:
                Module._low_pulse_count += 1
            else:
                assert pulse == Pulse.HIGH
                Module._high_pulse_count += 1
            Module._transmission_queue.append(Transmission(self._id, destination_module_id, pulse))

    @classmethod
    def from_string(cls, string: str) -> Module:
        name, destination_string = string.split(" -> ")
        destinations: list[str] = destination_string.split(", ")
        match name[0]:
            case "%":
                name = name.lstrip("%")
                return FlipFlop(name, destinations)
            case "&":
                name = name.lstrip("&")
                return Conjunction(name, destinations)
            case _:
                assert name == "broadcaster"
                return Broadcaster(name, destinations)

    @override
    @abstractmethod
    def __str__(self) -> str:
        pass


class FlipFlop(Module):
    class State(Enum):
        ON = auto()
        OFF = auto()

    def __init__(self, id_: str, destinations: list[str]) -> None:
        super().__init__(id_, destinations)
        self._state = FlipFlop.State.OFF

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        assert pulse in (Pulse.LOW, Pulse.HIGH)
        if pulse == Pulse.LOW:
            self._state = FlipFlop.State.ON if self._state == FlipFlop.State.OFF else FlipFlop.State.OFF
            pulse_to_send = Pulse.HIGH if self._state == FlipFlop.State.ON else Pulse.LOW
            self._send_pulse(pulse_to_send)

    @override
    def __str__(self) -> str:
        return f"%{self.id}: {self._state} -> {", ".join(self.destinations)}"


class Conjunction(Module):
    def __init__(self, id_: str, destinations: list[str]) -> None:
        super().__init__(id_, destinations)
        self._memory: dict[str, Pulse] = dict()

    @override
    def initialize(self, modules: dict[str, Module]) -> None:
        super().initialize(modules)
        for module in modules.values():
            if self.id in module.destinations:
                self._memory[module.id] = Pulse.LOW

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        old_state = all(pulse == Pulse.HIGH for pulse in self._memory.values())
        self._memory[sender] = pulse
        if all(pulse == Pulse.HIGH for pulse in self._memory.values()):
            self._send_pulse(Pulse.LOW)
            # if self.id == "rk" and not old_state:
            #     print(f"L {Module.pulse_count() - Module.button_pressed_timestamps[-1]}")
        else:
            self._send_pulse(Pulse.HIGH)
            if self.id == "zf":
                #     print(f"H {Module.pulse_count() - Module.button_pressed_timestamps[-1]}, {len(Module.button_pressed_timestamps)} button presses")
                print(f"{len(Module.button_pressed_timestamps)}")

    @override
    def __str__(self) -> str:
        return f"&{self.id}: {self._memory} -> {", ".join(self.destinations)}"


class Broadcaster(Module):
    def __init__(self, id_: str, destinations: list[str]) -> None:
        super().__init__(id_, destinations)

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        self._send_pulse(pulse)

    def __str__(self) -> str:
        return f"broadcaster -> {", ".join(self.destinations)}"


class Button(Module):
    def __init__(self, id_: str, destinations: list[str]) -> None:
        super().__init__(id_, destinations)

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        assert False, "unreachable"

    def push(self) -> None:
        self._send_pulse(Pulse.LOW)

    @override
    def __str__(self) -> str:
        return f"button -> {", ".join(self.destinations)}"


class Output(Module):
    def __init__(self, id_: str) -> None:
        super().__init__(id_, [])

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        # print(f"DEBUG OUTPUT: {sender = }, {pulse = }")
        pass

    @override
    def __str__(self) -> str:
        return "output"


class MachineSwitch(Module):
    def __init__(self, id_: str) -> None:
        super().__init__(id_, [])

    @override
    def _handle_pulse(self, sender: str, pulse: Pulse) -> None:
        if pulse == Pulse.LOW:
            pass

    @override
    def __str__(self) -> str:
        return f"{self.id}"


def search_route(
        start: str,
        end: str,
        modules: dict[str, Module],
        visited: Optional[set[str]] = None
) -> Optional[list[str]]:
    if visited is None:
        visited = set()
    if start in visited:
        return None
    visited.add(start)
    if start == end:
        return [end]
    for destination in modules[start].destinations:
        result = search_route(destination, end, modules, visited)
        if result is not None:
            result.append(start)
            return result
    return None


def collect_influencing_modules(
        influencees: set[str],
        modules: dict[str, Module],
        candidate: str,
        visited: set[str]
) -> None:
    if candidate in visited:
        return
    visited.add(candidate)
    for source in modules[candidate].sources:
        collect_influencing_modules(influencees, modules, source, visited)


def main() -> None:
    modules = {module.id: module for module in (Module.from_string(line.strip()) for line in open("data.txt"))}
    modules["rx"] = MachineSwitch("rx")
    modules["output"] = Output("output")
    button = Button("button", ["broadcaster"])
    modules["button"] = button
    for module in modules.values():
        module.initialize(modules)

    route = search_route("button", "rx", modules)
    route.reverse()
    print(route)

    influencers = set()
    influencees: Final[set[str]] = set(route)
    for module_id in route:
        collect_influencing_modules(influencees, modules, module_id, influencers)

    print(influencers)
    print(len(influencers))

    for module in modules.values():
        for destination in module.destinations:
            print(f"{module.id} -> {destination};")

    num_button_presses = 0
    while True:
        Module.button_pressed_timestamps.append(Module.pulse_count())
        button.push()
        num_button_presses += 1
        Module.process_until_end(modules)
        # print(f"{Module.low_pulse_count() + Module.high_pulse_count()}")

    # for module in modules.values():
    #     print(module)

    # print(f"{Module.low_pulse_count() = }")
    # print(f"{Module.high_pulse_count() = }")
    # print(f"result = {Module.low_pulse_count() * Module.high_pulse_count()}")


if __name__ == "__main__":
    main()
