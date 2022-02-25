fn main() {
    say_hello();
    moved_hello();
    cloned_hello();

    let s = String::from("waaah, stolen");
    steal_string(s);

    // println!("I can't use this anymore {s}");

    let s2 = String::from("can I be mutated?");
    // s2.push_str(" No, because I never specified mutable!");
    println!("{s2} - Nope");
}

fn say_hello() {
    let mut s = String::from("Hello");
    s.push_str(", world!");
    println!("{}", s);
}

fn moved_hello() {
    // Fundamentally similar to the new-fangled C++ concepts with std::move
    // but in this case it's built in to the language core.
    let s1 = String::from("hi");
    let s2 = s1;

    // println!("Can't use because it's been moved {s1}");
    println!("{s2}");
}

fn cloned_hello() {
    let s1 = String::from("cloned");
    let s2 = s1.clone();

    println!("s1 = {}, s2 = {}", s1, s2);
}

fn steal_string(s: String) {
    println!("haha, I stole the string - {s}");
}
