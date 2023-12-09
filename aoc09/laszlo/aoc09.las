function map(list: [?], f: Function(?) ~> ?) ~> [?] {
    let result = [];
    for element in list {
        result += [f(element)];
    }
    return result;
}

function string_to_int(s: String) ~> I32 {
    return s => I32;
}

function parse_line(line: String) ~> [I32] {
    return line.split(' ').map(string_to_int);
}

function all_zero(list: [I32]) ~> Bool {
    for element in list {
        if element != 0 {
            return false;
        }
    }
    return true;
}

function pre(history: [I32], sub_diction: I32) ~> I32 {
    return history[history.size - 1] + sub_diction;
}

function post(history: [I32], sub_diction: I32) ~> I32 {
    return history[0] - sub_diction;
}

function dict(history: [I32], accumulator: Function([I32], I32) ~> I32) ~> I32 {
    if history.all_zero() {
        return 0;
    }
    let differences = [];
    for i in 0..history.size - 1 {
        differences += [history[i + 1] - history[i]];
    }
    return accumulator(history, differences.dict(accumulator));
}

function predict(history: [I32]) ~> I32 {
    return history.dict(pre);
}

function postdict(history: [I32]) ~> I32 {
    return history.dict(post);
}

function sum(list: [I32]) ~> I32 {
    let result = 0;
    for element in list {
        result += element;
    }
    return result;
}

function add(a: I32, b: I32) ~> I32 {
    return a + b;
}

function main() {
    let lines = read("data.txt").split('\n').map(parse_line);
    println("part 1: " + lines.map(predict).sum());
    println("part 2: " + lines.map(postdict).sum());
}

main();
