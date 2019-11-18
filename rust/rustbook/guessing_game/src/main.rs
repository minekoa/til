use std::io;

fn main() {
    println!("Guess the number! (数を当ててご覧)");

    println!("Please input your guess. (予想を入力してね)");

    let mut guess = String::new();


    io::stdin().read_line(&mut guess)
        .expect("Failed to read line (行の読み込みに失敗しました)");

    println!("You gessed (あなたの予想): {}", guess);
}
