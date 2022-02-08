fn main() {
    another_function(32);
    let x = returning_function(2);
    println!("5 * 5 is {x}");

    let y = if x < 25 {
        let z = 5;
        z + 42
    } else {
        42
    };

    println!("y is {y}");

    let mut i = 0;
    'loopname: loop {
        i += 1;

        if i > 5 {
            break 'loopname;
        }

        if i % 2 == 0 {
            println!("i is {i} and it's even");
        }
    }

    let a = [1, 2, 3, 4, 5];
    let mut index = 0;
    while index < a.len() {
        let value = a[index];
        println!("The value at index {index} is {value}");
        index += 1;
    }

    for value in a {
        println!("The value is {value}");
    }

    for number in 1..4 {
        println!("T-minus {number}");
    }
}

fn another_function(var: u32) {
    println!("Hi! I'm a function and I got {var}");
}

fn returning_function(test: u32) -> u32 {
    test * 5
}
