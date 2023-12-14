data = open("data.txt").read().splitlines()

width, height = len(data[0]), len(data)

fixed_rock_positions: list[list[tuple[int, int]]] = [[(-1, 0)] for _ in range(width)]
for column in range(width):
    rounded_rock_counter = 0
    for row in range(height):
        match data[row][column]:
            case "O":
                rounded_rock_counter += 1
            case "#":
                fixed_rock_positions[column][-1] = (fixed_rock_positions[column][-1][0], rounded_rock_counter)
                rounded_rock_counter = 0
                fixed_rock_positions[column].append((row, 0))
    fixed_rock_positions[column][-1] = (fixed_rock_positions[column][-1][0], rounded_rock_counter)

result = 0
for fixed_rocks in fixed_rock_positions:
    for fixed_rock in fixed_rocks:
        index, amount = fixed_rock
        starting_weight = height - index - 1
        result += sum(range(starting_weight, starting_weight - amount, -1))

print(result)
