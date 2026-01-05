use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};

// 数字名称数据结构
#[derive(Clone, Debug)]
struct NumberInfo {
    value: i64,
    syllables: Vec<i64>,
    names: Vec<String>,
    equations: Vec<String>,
    original: i64,
    zeroes: i64,
    digits: i64,
    nonzero: i64,
    auto_pass: bool,
}

const PEMDAS_COUNT: usize = 6;

// 一位数和十几的名称: [[基数词, 音节数], [序数词, 音节数]]
// 对于 "two": [[\"two\", 1, 1], [\"second\", 2]] - 第三个参数 1 表示特殊处理
fn get_one_names() -> Vec<((String, i64, Option<i64>), (String, i64))> {
    vec![
        (("zero".to_string(), 2, None), ("zeroeth".to_string(), 2)),
        (("one".to_string(), 1, None), ("first".to_string(), 1)),
        (("two".to_string(), 1, Some(1)), ("second".to_string(), 2)),
        (("three".to_string(), 1, None), ("third".to_string(), 1)),
        (("four".to_string(), 1, None), ("fourth".to_string(), 1)),
        (("five".to_string(), 1, None), ("fifth".to_string(), 1)),
        (("six".to_string(), 1, None), ("sixth".to_string(), 1)),
        (("seven".to_string(), 2, None), ("seventh".to_string(), 2)),
        (("eight".to_string(), 1, None), ("eighth".to_string(), 1)),
        (("nine".to_string(), 1, None), ("ninth".to_string(), 1)),
        (("ten".to_string(), 1, None), ("tenth".to_string(), 1)),
        (("eleven".to_string(), 3, None), ("eleventh".to_string(), 3)),
        (("twelve".to_string(), 1, None), ("twelfth".to_string(), 1)),
        (
            ("thirteen".to_string(), 2, None),
            ("thirteenth".to_string(), 2),
        ),
        (
            ("fourteen".to_string(), 2, None),
            ("fourteenth".to_string(), 2),
        ),
        (
            ("fifteen".to_string(), 2, None),
            ("fifteenth".to_string(), 2),
        ),
        (
            ("sixteen".to_string(), 2, None),
            ("sixteenth".to_string(), 2),
        ),
        (
            ("seventeen".to_string(), 3, None),
            ("seventeenth".to_string(), 3),
        ),
        (
            ("eighteen".to_string(), 2, None),
            ("eighteenth".to_string(), 2),
        ),
        (
            ("nineteen".to_string(), 2, None),
            ("nineteenth".to_string(), 2),
        ),
    ]
}

// 十位数名称
fn get_ten_names() -> Vec<Option<((String, i64), (String, i64))>> {
    vec![
        None,
        None,
        Some((("twenty".to_string(), 2), ("twentieth".to_string(), 3))),
        Some((("thirty".to_string(), 2), ("thirtieth".to_string(), 3))),
        Some((("forty".to_string(), 2), ("fortieth".to_string(), 3))),
        Some((("fifty".to_string(), 2), ("fiftieth".to_string(), 3))),
        Some((("sixty".to_string(), 2), ("sixtieth".to_string(), 3))),
        Some((("seventy".to_string(), 3), ("seventieth".to_string(), 4))),
        Some((("eighty".to_string(), 2), ("eightieth".to_string(), 3))),
        Some((("ninety".to_string(), 2), ("ninetieth".to_string(), 3))),
    ]
}

// 大数名称: [名称, 音节数, 值, 零的个数]
fn get_large_names() -> Vec<(String, i64, i64, i64)> {
    vec![
        ("hundred".to_string(), 2, 100, 2),
        ("thousand".to_string(), 2, 1000, 3),
        ("million".to_string(), 2, 1000000, 6),
        ("billion".to_string(), 2, 1000000000, 9),
    ]
}

fn get_superscripts() -> Vec<&'static str> {
    vec![
        "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "¹⁰", "¹¹", "¹²", "¹³", "¹⁴", "¹⁵", "¹⁶",
        "¹⁷", "¹⁸", "¹⁹", "²⁰", "²¹", "²²", "²³",
    ]
}

fn base_syllables(
    n: i64,
    number_names: &[NumberInfo],
    one_names: &[((String, i64, Option<i64>), (String, i64))],
    ten_names: &[Option<((String, i64), (String, i64))>],
    large_names: &[(String, i64, i64, i64)],
) -> (i64, String, i64, String, i64, i64) {
    if n < 20 {
        let (ref cardinal, ref ordinal) = one_names[n as usize];
        return (
            cardinal.1,
            cardinal.0.clone(),
            ordinal.1,
            ordinal.0.clone(),
            0,
            1,
        );
    }

    if n < 100 {
        let n_mod = n % 10;
        let n_div = n / 10;

        if n_mod == 0 {
            let ten = ten_names[n_div as usize].as_ref().unwrap();
            return (ten.0.1, ten.0.0.clone(), ten.1.1, ten.1.0.clone(), 1, 2);
        }

        let ten = ten_names[n_div as usize].as_ref().unwrap();
        return (
            ten.0.1 + number_names[n_mod as usize].syllables[1],
            format!("{}-{}", ten.0.0, number_names[n_mod as usize].names[1]),
            ten.0.1 + number_names[n_mod as usize].syllables[0],
            format!("{}-{}", ten.0.0, number_names[n_mod as usize].names[0]),
            0,
            2,
        );
    }

    // 找到合适的 large_index
    let mut large_index = 0;
    while large_index + 1 < large_names.len() && large_names[large_index + 1].2 <= n {
        large_index += 1;
    }

    let n_mod = n % large_names[large_index].2;
    let n_div = n / large_names[large_index].2;

    if n_mod == 0 {
        return (
            number_names[n_div as usize].syllables[1] + large_names[large_index].1,
            format!(
                "{} {}",
                number_names[n_div as usize].names[1], large_names[large_index].0
            ),
            number_names[n_div as usize].syllables[1] + large_names[large_index].1,
            format!(
                "{} {}th",
                number_names[n_div as usize].names[1], large_names[large_index].0
            ),
            large_names[large_index].3 + number_names[n_div as usize].zeroes,
            large_names[large_index].3 + number_names[n_div as usize].digits,
        );
    }

    let connect_word = " ";
    let connect_syllables: i64 = 0;

    (
        number_names[n_div as usize].syllables[1]
            + large_names[large_index].1
            + connect_syllables
            + number_names[n_mod as usize].syllables[1],
        format!(
            "{} {}{}{}",
            number_names[n_div as usize].names[1],
            large_names[large_index].0,
            connect_word,
            number_names[n_mod as usize].names[1]
        ),
        number_names[n_div as usize].syllables[1]
            + large_names[large_index].1
            + connect_syllables
            + number_names[n_mod as usize].syllables[0],
        format!(
            "{} {}{}{}",
            number_names[n_div as usize].names[1],
            large_names[large_index].0,
            connect_word,
            number_names[n_mod as usize].names[0]
        ),
        number_names[n_mod as usize].zeroes,
        large_names[large_index].3 + number_names[n_div as usize].digits,
    )
}

#[derive(Clone)]
struct UnaryOp {
    id: String,
    syllables: i64,
    text: String,
    value: i64,
    pemdas_input: usize,
    pemdas_result: usize,
}

#[derive(Clone)]
struct BinaryOp {
    id: String,
    syllables: i64,
    text: String,
    suffix: String,
    pemdas_left: usize,
    pemdas_right: usize,
    pemdas_result: usize,
}

fn get_first_extremes(op_id: &str, min_missing: i64, max_number: i64) -> (f64, f64) {
    match op_id {
        "²" => (
            (min_missing as f64).powf(0.5),
            (max_number as f64).powf(0.5),
        ),
        "³" => (
            (min_missing as f64).powf(1.0 / 3.0),
            (max_number as f64).powf(1.0 / 3.0),
        ),
        "+" => (6.0, (max_number - 1) as f64),
        "*" => (2.0, (max_number as f64).powf(0.5)),
        "-" => ((min_missing + 1) as f64, max_number as f64),
        "/" | "fraction" => ((min_missing * 2) as f64, max_number as f64),
        "^" => (2.0, (max_number as f64).powf(0.2)),
        _ => (0.0, 0.0),
    }
}

fn get_second_extremes(
    op_id: &str,
    min_missing: i64,
    max_number: i64,
    left_value: i64,
) -> (f64, f64) {
    match op_id {
        "+" => (1.0, (left_value.min(max_number - left_value)) as f64),
        "*" => (
            (left_value as f64).max(min_missing as f64 / left_value as f64),
            max_number as f64 / left_value as f64,
        ),
        "-" => (1.0, (left_value - min_missing) as f64),
        "/" | "fraction" => (2.0, left_value as f64 / 2.0),
        "^" => (5.0, (max_number as f64).ln() / (left_value as f64).ln()),
        _ => (0.0, 0.0),
    }
}

fn get_output(op_id: &str, left_value: i64, right_value: i64) -> (i64, bool) {
    match op_id {
        "²" => (left_value * left_value, true),
        "³" => (left_value * left_value * left_value, true),
        "^" => {
            let result = (left_value as f64).powi(right_value as i32) as i64;
            (result, true)
        }
        "+" => (left_value + right_value, true),
        "*" => (left_value * right_value, true),
        "-" => (left_value - right_value, true),
        "/" | "fraction" => {
            if right_value != 0 && left_value % right_value == 0 {
                (left_value / right_value, true)
            } else {
                (0, false)
            }
        }
        _ => (0, false),
    }
}

fn number_names_generator(leave_point: i64, max_number: i64) -> Vec<NumberInfo> {
    let one_names = get_one_names();
    let ten_names = get_ten_names();
    let large_names = get_large_names();
    let superscripts = get_superscripts();

    let mut number_names: Vec<NumberInfo> = Vec::new();
    let mut max_syllables: i64 = 0;

    // 初始化所有数字的基本信息
    for n in 0..=max_number {
        let (n_syllables, n_name, frac_syllables, frac_name, zeroes, digits) =
            base_syllables(n, &number_names, &one_names, &ten_names, &large_names);

        let mut adj_zeroes = zeroes;
        if zeroes > 3 {
            adj_zeroes = (zeroes / 3) * 3;
        }

        let mut syllables = vec![frac_syllables];
        syllables.extend(std::iter::repeat(n_syllables).take(PEMDAS_COUNT - 1));

        let mut names = vec![frac_name];
        names.extend(std::iter::repeat(n_name.clone()).take(PEMDAS_COUNT - 1));

        let equations = vec![n.to_string(); PEMDAS_COUNT];

        let n_mod_100 = n % 100;
        let auto_pass = (n_mod_100 < 20 && n_mod_100 > 0) || zeroes < 1 || digits < 3;

        number_names.push(NumberInfo {
            value: n,
            syllables,
            names,
            equations,
            original: n_syllables,
            zeroes: adj_zeroes,
            digits,
            nonzero: digits - zeroes,
            auto_pass,
        });

        max_syllables = max_syllables.max(n_syllables);
    }

    // 特殊处理 2 的序数形式
    number_names[2].syllables[0] = 1;
    number_names[2].names[0] = "halve".to_string();

    // 初始化 syllable_key
    let mut syllable_key: Vec<Vec<Vec<i64>>> = vec![vec![Vec::new(); PEMDAS_COUNT]];

    // 定义一元运算符
    let unary = vec![
        UnaryOp {
            id: "²".to_string(),
            syllables: 1,
            text: " squared".to_string(),
            value: 2,
            pemdas_input: 2,
            pemdas_result: 2,
        },
        UnaryOp {
            id: "³".to_string(),
            syllables: 1,
            text: " cubed".to_string(),
            value: 3,
            pemdas_input: 2,
            pemdas_result: 2,
        },
    ];

    // 定义二元运算符
    let binary = vec![
        BinaryOp {
            id: "+".to_string(),
            syllables: 1,
            text: " plus ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 5,
            pemdas_right: 5,
            pemdas_result: 5,
        },
        BinaryOp {
            id: "*".to_string(),
            syllables: 1,
            text: " times ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 3,
            pemdas_right: 4,
            pemdas_result: 4,
        },
        BinaryOp {
            id: "*".to_string(),
            syllables: 1,
            text: " times ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 3,
            pemdas_right: 3,
            pemdas_result: 3,
        },
        BinaryOp {
            id: "-".to_string(),
            syllables: 2,
            text: " minus ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 5,
            pemdas_right: 4,
            pemdas_result: 5,
        },
        BinaryOp {
            id: "/".to_string(),
            syllables: 2,
            text: " over ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 3,
            pemdas_right: 2,
            pemdas_result: 4,
        },
        BinaryOp {
            id: "fraction".to_string(),
            syllables: 0,
            text: " ".to_string(),
            suffix: "s".to_string(),
            pemdas_left: 2,
            pemdas_right: 0,
            pemdas_result: 2,
        },
        BinaryOp {
            id: "^".to_string(),
            syllables: 2,
            text: " to the ".to_string(),
            suffix: "".to_string(),
            pemdas_left: 2,
            pemdas_right: 0,
            pemdas_result: 2,
        },
    ];

    let mut min_missing: i64 = 1;

    for s in 1..=max_syllables {
        println!("searching {} syllables, at {}", s, min_missing);

        syllable_key.push(vec![Vec::new(); PEMDAS_COUNT]);

        // 填充当前音节数的数字
        for n in min_missing..=max_number {
            for u in 0..PEMDAS_COUNT {
                if number_names[n as usize].syllables[u] < s {
                    break;
                }
                if number_names[n as usize].syllables[u] == s {
                    syllable_key[s as usize][u].push(number_names[n as usize].value);
                } else if u > 0 {
                    break;
                }
            }
        }

        // 处理二元运算符
        for op in &binary {
            let (min_left, max_left) = get_first_extremes(&op.id, min_missing, max_number);

            for left_syllables in 0..(s - op.syllables) {
                if left_syllables < 0 || left_syllables as usize >= syllable_key.len() {
                    continue;
                }

                let left_values: Vec<i64> =
                    syllable_key[left_syllables as usize][op.pemdas_left].clone();

                for &left_value in &left_values {
                    if (left_value as f64) < min_left {
                        continue;
                    }
                    if (left_value as f64) > max_left {
                        break;
                    }

                    let (min_right, max_right) =
                        get_second_extremes(&op.id, min_missing, max_number, left_value);

                    let right_idx = (s - op.syllables - left_syllables) as usize;
                    if right_idx >= syllable_key.len() {
                        continue;
                    }

                    let right_values: Vec<i64> = syllable_key[right_idx][op.pemdas_right].clone();

                    for &right_value in &right_values {
                        if (right_value as f64) < min_right {
                            continue;
                        }
                        if (right_value as f64) > max_right {
                            break;
                        }

                        // fraction 特殊检查
                        if op.id == "fraction"
                            && !number_names[left_value as usize].auto_pass
                            && right_value != 2
                            && number_names[left_value as usize].zeroes
                                >= number_names[right_value as usize].digits
                            && (number_names[left_value as usize].nonzero > 1
                                || number_names[right_value as usize].nonzero > 1)
                            && number_names[left_value as usize].names[1]
                                == number_names[left_value as usize].names[2]
                        {
                            continue;
                        }

                        let (op_output, valid_output) = get_output(&op.id, left_value, right_value);
                        if !valid_output {
                            continue;
                        }

                        if op_output < 0 || op_output > max_number {
                            continue;
                        }

                        let new_name = format!(
                            "{}{}{}{}",
                            number_names[left_value as usize].names[op.pemdas_left],
                            op.text,
                            number_names[right_value as usize].names[op.pemdas_right],
                            op.suffix
                        );

                        let new_equation = if op.id == "^" {
                            format!(
                                "{} {}",
                                number_names[left_value as usize].equations[op.pemdas_left],
                                superscripts[right_value as usize]
                            )
                        } else {
                            let op_symbol = if op.id == "fraction" { "/" } else { &op.id };
                            format!(
                                "{} {} {}",
                                number_names[left_value as usize].equations[op.pemdas_left],
                                op_symbol,
                                number_names[right_value as usize].equations[op.pemdas_right]
                            )
                        };

                        for u in op.pemdas_result..PEMDAS_COUNT {
                            if number_names[op_output as usize].syllables[u] >= s {
                                number_names[op_output as usize].names[u] = new_name.clone();
                                number_names[op_output as usize].equations[u] =
                                    new_equation.clone();

                                if number_names[op_output as usize].syllables[u] > s {
                                    number_names[op_output as usize].syllables[u] = s;
                                    syllable_key[s as usize][u].push(op_output);
                                }
                            }
                        }
                    }
                }
            }
        }

        // 处理一元运算符
        for op in &unary {
            if s <= op.syllables {
                continue;
            }

            let (min_value, max_value) = get_first_extremes(&op.id, min_missing, max_number);

            let input_idx = (s - op.syllables) as usize;
            if input_idx >= syllable_key.len() {
                continue;
            }

            let input_values: Vec<i64> = syllable_key[input_idx][op.pemdas_input].clone();

            for &input_value in &input_values {
                if (input_value as f64) < min_value {
                    continue;
                }
                if (input_value as f64) > max_value {
                    break;
                }

                let (op_output, valid_output) = get_output(&op.id, input_value, 0);
                if !valid_output {
                    continue;
                }

                if op_output < 0 || op_output > max_number {
                    continue;
                }

                let new_name = format!(
                    "{}{}",
                    number_names[input_value as usize].names[op.pemdas_input], op.text
                );
                let new_equation = format!(
                    "{} {}",
                    number_names[input_value as usize].equations[op.pemdas_input], op.id
                );

                for u in op.pemdas_result..PEMDAS_COUNT {
                    if number_names[op_output as usize].syllables[u] >= s {
                        number_names[op_output as usize].names[u] = new_name.clone();
                        number_names[op_output as usize].equations[u] = new_equation.clone();

                        if number_names[op_output as usize].syllables[u] > s {
                            number_names[op_output as usize].syllables[u] = s;
                            syllable_key[s as usize][u].push(op_output);
                        }
                    }
                }
            }
        }

        // 排序 syllable_key
        for i in 0..PEMDAS_COUNT {
            syllable_key[s as usize][i].sort();
        }

        // 更新 min_missing
        while min_missing <= max_number
            && number_names[min_missing as usize].syllables[PEMDAS_COUNT - 1] <= s
        {
            min_missing += 1;
            if min_missing > leave_point {
                break;
            }
        }

        if min_missing > leave_point {
            break;
        }
    }

    number_names[0..=(leave_point as usize)].to_vec()
}

fn numbers_out(number_names: &[NumberInfo], file_name: &str) -> std::io::Result<()> {
    let file = File::create(file_name)?;
    let mut writer = BufWriter::new(file);

    for info in number_names {
        writeln!(
            writer,
            "{},{},{},{}",
            info.value,
            info.names[PEMDAS_COUNT - 1],
            info.equations[PEMDAS_COUNT - 1],
            info.syllables[PEMDAS_COUNT - 1]
        )?;
    }

    Ok(())
}

fn main() {
    let fast_numbers = number_names_generator(100000, 1000000);
    numbers_out(&fast_numbers, "fastest_numbers_rust.csv").expect("Failed to write output file");
    println!("Done! Output written to fastest_numbers_rust.csv");
}
