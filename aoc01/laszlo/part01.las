function is_digit(c: Char) ~> Bool {
    return c >= '0'and c <= '9';
}

let input = read("data.txt");

let sum = 0;

let parts = input.split('\n');
for part in parts {
    let left = ' ';
    for c in part {
        if c.is_digit() {
            left = c;
            break;
        }
    }
    assert(left != ' ');

    let right = ' ';
    for i in part.length - 1..=0 {
        if part[i].is_digit() {
            right = part[i];
            break;
        }
    }
    assert(right != ' ');

    let number_string = "" + left + right;
    let number = (number_string => I32);

    sum += number;
}

println(sum);
