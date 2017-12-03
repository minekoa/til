mod ffi {
    extern crate libc;

    pub enum FooObj {}

    extern "C" {
        pub fn fooObj_new() -> *mut FooObj;
        pub fn fooObj_delete(obj: *mut FooObj) -> *mut FooObj;

        pub fn foo_StaticMethod1(a:i32) -> i32;
        pub fn foo_Method1(obj: *mut FooObj, a:i32) -> i32;
    }
}


impl FooObj {
    pub fn new() -> FooObj {
        unsafe {
            let mut myObj = FooObj {
                raw: ffi::fooObj_new(),

fn main() {
    println!("Hello, world!");
}
