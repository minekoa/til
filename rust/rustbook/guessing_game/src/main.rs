extern crate rand;

use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("Guess the number! (数を当ててご覧)");

    let secret_number = rand::thread_rng().gen_range(1, 101);
//    println!("The secret number is : {}", secret_number);

    loop {
        println!("Please input your guess. (予想を入力してね)");
        let mut guess = String::new();

        io::stdin().read_line(&mut guess)
            .expect("Failed to read line (行の読み込みに失敗しました)");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Please type a number! (数値を入力してね)");
                continue;
            },
        };

        println!("You gessed (あなたの予想): {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small! (ちいさすぎ)"),
            Ordering::Greater => println!("Too big! (おおきすぎ)"),
            Ordering::Equal   => {
                println!("You win! (やったねたえちゃん！）");
                break;
            },
        }
    }
}
