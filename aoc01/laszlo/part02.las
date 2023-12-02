function is_digit(c: Char) ~> Bool {
    return c >= '0'and c <= '9';
}

function starts_with(s: String, offset: I32, prefix: String) ~> Bool {
    if prefix.length > s.length - offset {
        return false;
    }
    for i in 0..prefix.length {
        if s[i + offset] != prefix[i] {
            return false;
        }
    }
    return true;
}

let number_words = ["_", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

function get_number(string: String, offset: I32) ~> I32 {
    if string[offset].is_digit() {
        return ("" + string[offset]) => I32;
    }
    for i in 0..number_words.size {
        if string.starts_with(offset, number_words[i]) {
            return i;
        }
    }
    return -1;
}

function main() {
    let input = read("data.txt");
    let parts = input.split('\n');

    let sum = 0;

    for part in parts {
        let left = -1;
        for i in 0..part.length {
            let number = get_number(part, i);
            if number >= 0 {
                left = number;
                break;
            }
        }
        assert(left >= 0);

        let right = -1;
        for i in part.length - 1..=0 {
            let number = get_number(part, i);
            if number >= 0 {
                right = number;
                break;
            }
        }

        sum += left * 10 + right;
    }

    println(sum);
}

main();
