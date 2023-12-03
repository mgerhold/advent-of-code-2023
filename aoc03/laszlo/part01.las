struct PartNumber {
    y: I32,
    x_start: I32,
    x_end: I32,
    value: I32,
}

struct Symbol {
    y: I32,
    x: I32,
    c: Char,
}

struct Schematic {
    part_numbers: [?],
    symbols: [?],
}

function is_digit(c: Char) ~> Bool {
    return c >= '0' and c <= '9';
}

// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..

function parse(s: String) ~> ? {
    let part_numbers = [];
    let symbols = [];
    let lines = s.split('\n');
    for y in 0..lines.size {
        let current = "";
        for x in 0..lines[y].length {
            let c = lines[y][x];
            if c.is_digit() {
                current += c;
            } else {
                if current.length > 0 {
                    part_numbers += [new PartNumber{
                        y: y,
                        x_start: x - current.length,
                        x_end: x,
                        value: current => I32,
                    }];
                    current = "";
                }
                if c != '.' {
                    symbols += [new Symbol{
                        y: y,
                        x: x,
                        c: c,
                    }];
                }
            }
        }
        if current.length > 0 {
            part_numbers += [new PartNumber{
                y: y,
                x_start: lines[y].length - current.length,
                x_end: lines[y].length,
                value: current => I32,
            }];
        }
    }
    return new Schematic{
        part_numbers: part_numbers,
        symbols: symbols,
    };
}

function main() {
    let data = read("data.txt");
    let schematic = data.parse();

    let sum = 0;
    for part_number in schematic.part_numbers {
        for symbol in schematic.symbols {
            let overlaps = false;
            for x in symbol.x - 1..=symbol.x + 1 {
                for y in symbol.y - 1..=symbol.y + 1 {
                    if x == symbol.x and y == symbol.y {
                        continue;
                    }
                    if y == part_number.y and x >= part_number.x_start and x < part_number.x_end {
                        overlaps = true;
                        break;
                    }
                }
                if overlaps {
                    break;
                }
            }
            if overlaps {
                println(part_number);
                sum += part_number.value;
                break;
            }
        }
    }

    println(sum);
}

main();
