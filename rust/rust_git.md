# [rust] Rust からGitを操作する

Rust の練習として、この週末に、RustからGitを操作するクライアントを書いてみよう。
参考にするのはこちらだ。

* [Rust で Git クライアントを書き始めた | d.sunnyone.org](http://d.sunnyone.org/2016/16/rust-git.html)
* [sunyone/metal-git: Metal Git -a GUI Git Client written in gtk-rs ](https://github.com/sunnyone/metal-git)

ライブラリとして [git2-rs](https://github.com/alexcrichton/git2-rs) というのがあるそうな。


`init` して、`add`して`commit`する一通りをやってみた。

```rust
extern crate git2;

use git2::Repository;
use std::path::Path;
use std::fs::File;
use std::io::Write;

fn git_init() -> Result<(), git2::Error> {
    let path = Path::new("./tmp" );
    try!(Repository::init(path));
    Ok(())
}

fn file_save() -> Result<(), std::io::Error> {
    let path = Path::new("./tmp/hoge.txt");
    let s = "abcdefghijk".to_string();

    try!(File::create(path).and_then( |mut f| f.write_all(s.as_bytes()) ));
    Ok(())
}

fn git_add() -> Result<(), git2::Error> {
    let rep_path = Path::new("./tmp" );

    let repo      = try!(Repository::open(rep_path));
    let mut index = try!(repo.index());

    let mut files = Vec::new();
    files.push("hoge.txt".to_string());

    try!(index.add_all( files.iter(), git2::ADD_DEFAULT, None));
    try!(index.write());

    Ok(())
}

fn git_commit() -> Result<(), git2::Error> {
    let rep_path = Path::new("./tmp" );
    let repo      = try!(Repository::open(rep_path));

    let head_ref    = try!(repo.head());
    let head_obj    = try!(head_ref.peel(git2::ObjectType::Commit));
    let head_commit = head_obj.as_commit().unwrap();

    let signature = try!(repo.signature());

    let message   = "commit!";

    let mut index = try!(repo.index());
    let tree_oid = try!(index.write_tree());
    let tree_obj = try!(repo.find_object(tree_oid, Some(git2::ObjectType::Tree)));
    let tree = tree_obj.as_tree().unwrap();

    try!(repo.commit(Some("HEAD"),
                     &signature,           // auther
                     &signature,           // commiter
                     &message,             // message
                     &tree,                // tree
                     &[head_commit]));     // parents

    Ok(())
}


fn main() {

    print!("git init...");
    match git_init() {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git add...");
    match file_save() {
        Ok(_)    => print!("(file saved)..."),
        Err(err) => println!("Error: {:?}", err),
    }

    match git_add() {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git commit...");
    match git_commit() {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }
}
```

git2_rs に example があって、init とか add とかはそこを見ればいいけれど、commitはそれがない。なぜ...。

で、commit は落とし穴があって

* 最初のコミットだと `try!(repo.head());` で head が見つからないで、落ちる。
* `git config [--global] user.name <ユーザ名>` と `git config [--global] user.email <メールアドレス>` をしていない環境だと、`try!(repo.signature());` がコケる。

この２つの対策が必要。




