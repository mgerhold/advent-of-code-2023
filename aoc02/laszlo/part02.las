function append(array: [?], object: ?) {
    array += [object];
}

struct Draw {
    red: I32,
    green: I32,
    blue: I32,
}

struct Game {
    id: I32,
    draws: [?],
}

function parse_draw(s: String) ~> ? {
    let parts = s.split(',');
    let red = 0;
    let green = 0;
    let blue = 0;
    for part in parts {
        let sub_parts = part.trim().split(' ');
        let count = sub_parts[0] => I32;
        let color = sub_parts[1];
        if color == "red" {
            red += count;
        } else if color == "green" {
            green += count;
        }  else if color == "blue" {
            blue += count;
        } else {
            assert(false); // unreachable
        }
    }
    return new Draw{
        red: red,
        green: green,
        blue: blue,
    };
}

function parse_game(line: String) ~> ? {
    // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let parts = line.split(':');
    let id = (parts[0].split(' ')[1]) => I32;
    parts = parts[1].split(';');
    let draws = [];
    for part in parts {
        draws.append(parse_draw(part.trim()));
    }
    return new Game {
        id: id,
        draws: draws,
    };
}

function is_game_possible(game: ?, max_cubes: ?) ~> Bool {
    for draw in game.draws {
        if draw.red > max_cubes.red
           or draw.green > max_cubes.green
           or draw.blue > max_cubes.blue {
            return false;
        }
    }
    return true;
}

function calculate_minimum_bag_contents(game: ?) ~> ? { // returns Draw
    let required = new Draw {
        red: 0,
        green: 0,
        blue: 0,
    };
    for draw in game.draws {
        if draw.red > required.red {
            required.red = draw.red;
        }
        if draw.green > required.green {
            required.green = draw.green;
        }
        if draw.blue > required.blue {
            required.blue = draw.blue;
        }
    }
    return required;
}

function power(draw: ?) ~> I32 {
    return draw.red * draw.green * draw.blue;
}

function main() {
    let input = read("data.txt");
    let lines = input.split('\n');
    let games = [];
    for line in lines {
        games.append(parse_game(line));
    }

    let sum_powers = 0;

    for game in games {
        sum_powers += game.calculate_minimum_bag_contents().power();
    }

    println(sum_powers);
}

main();
