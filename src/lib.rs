
#![feature(extern_types, libc)]
#![feature(ptr_wrapping_offset_from)]

// #![feature ( libc )]
extern crate libc;
mod geom_predicates_2;
//mod tinyexpr_2;
pub fn cleanup() {
	println!("I am cleanup the world!!")
}

pub fn svg_string_from_file(p: String) -> String {
	return "svg_string_from_file".to_owned();
}
