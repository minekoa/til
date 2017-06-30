extern crate git2;

use git2::{Repository, Signature}; //, RepositoryInitOptions, RepositoryInitMode, Error};
use std::path::Path;
use std::fs::File;
use std::io::Write;

fn git_init(rep_path: &str) -> Result<(), git2::Error> {
    let path = Path::new(rep_path);
    try!(Repository::init(path));
    Ok(())
}

fn file_save(fpath: &str, s:&String) -> Result<(), std::io::Error> {
    let path = Path::new( fpath );
    try!(File::create(path).and_then( |mut f| f.write_all(s.as_bytes()) ));
    Ok(())
}

fn git_add(rep_path: &str, fname:&str) -> Result<(), git2::Error> {
    let rep_path = Path::new(rep_path);

    let repo      = try!(Repository::open(rep_path));
    let mut index = try!(repo.index());

    let mut files = Vec::new();
    files.push(fname.to_string());

    try!(index.add_all( files.iter(), git2::ADD_DEFAULT, None));
    try!(index.write());

    Ok(())
}

fn git_rm(rep_path: &str, fname: &str) -> Result<(), git2::Error> {
    let rep_path = Path::new(rep_path);

    let repo      = try!(Repository::open(rep_path));
    let mut index = try!(repo.index());

    let mut files = Vec::new();
    files.push(fname.to_string());

//    try!(index.remove_path( Path::new(fname) ); 
    try!(index.remove_all(files.iter(), None));
    try!(index.write());

    Ok(())
}

fn git_commit(rep_path: &str, message: &str) -> Result<(), git2::Error> {
    let rep_path = Path::new(rep_path);
    let repo     = try!(Repository::open(rep_path));

    let signature = try!(Signature::now("foo", "bar@bar.com")); // git config で設定された signature を使う場合は repo.signature() を使う

    let mut index = try!(repo.index());
    let tree_oid  = try!(index.write_tree());
    let tree_obj  = try!(repo.find_object(tree_oid, Some(git2::ObjectType::Tree)));
    let tree      = tree_obj.as_tree().unwrap();

    match repo.head() {
        Ok(head_ref) => {
            let head_obj    = try!(head_ref.peel(git2::ObjectType::Commit));
            let head_commit = head_obj.as_commit().unwrap();

            try!(repo.commit(Some("HEAD"),
                             &signature,           // auther
                             &signature,           // commiter
                             &message,             // message
                             &tree,                // tree
                             &[head_commit]));

        }
        Err(_) => {
            try!(repo.commit(Some("HEAD"),
                             &signature,           // auther
                             &signature,           // commiter
                             &message,             // message
                             &tree,                // tree
                             &[]));
        }

    }

    Ok(())
}


fn main() {

    print!("git init...");
    match git_init("./tmp") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git add (hoge)...");
    match file_save("./tmp/hoge.txt", &"abcdefg".to_string()) {
        Ok(_)    => print!("(file saved)..."),
        Err(err) => println!("Error: {:?}", err),
    }

    match git_add("./tmp", "hoge.txt") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git add(huga) ...");
    match file_save("./tmp/huga.txt", &"hijklmn".to_string()) {
        Ok(_)    => print!("(file saved)..."),
        Err(err) => println!("Error: {:?}", err),
    }

    match git_add("./tmp", "huga.txt") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }


    print!("git commit...");
    match git_commit("./tmp", "This is a commit message") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git rm (huga) ...");
    match git_rm("./tmp", "huga.txt") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }
    print!("git commit...");
    match git_commit("./tmp", "This is a commit message2") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }

    print!("git rm (huga2) ...");
    match git_rm("./tmp", "huga.txt") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }
    print!("git commit...");
    match git_commit("./tmp", "This is a commit message3") {
        Ok(_)    => println!("SUCCESS!"),
        Err(err) => println!("Error: {:?}", err),
    }
    
    

}

