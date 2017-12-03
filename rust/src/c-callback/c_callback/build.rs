extern crate gcc;

fn main {
    gcc::compile_library("libsub.a", &["src/sub.c"]);
}

