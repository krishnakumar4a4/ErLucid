use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::collections::VecDeque;

fn main() {
    println!("Hello, world!");
    let path = Path::new("test");
    let display = path.display();

    let file = match File::open(&path) {
    	Err(why) => panic!("could not open {}:{}", display,why.description()),
    	Ok(file) => file,
    };

    let f = BufReader::new(file);
    // for line in f.lines(){
    // 	println!("for ever line of: {}", line.unwrap());
    // }
//Using vectors
    // let mut vec = vec![1,2,3,4];
    // let vec2 = vec![5,6,7];
    // vec.extend(vec2);
    // for i in vec.iter() {
    // 	println!("{}\n", i);
    // }

//Using double ended vectors
	// let mut vecdeq = VecDeque::new();
	// for line in f.lines() {
	// 	println!("pushing into double ended vectorq");
	// 	vecdeq.push_front(line);
	// }	
	// let mut test = VecDeque::new();
	// test = vecdeq.iter().inspect(|x| println!("inspecting: {:?}", x)).collect();

//read call enties from file and push it to vector;return entries should be matched
//with top-to-bottom of call entries; if no return is matching, push it back of vector;
//once all the matching calls are exhausted, store the vector and continue.
	let mut vecdeq = VecDeque::new();
	for line in f.lines() {
		let instring = line.unwrap().to_string();
		let clonedstring = instring.clone();
		// println!("pushing into double ended vectorq:{:?}",instring);
		// println!("{:?}", instring.chars().nth(1));
		let call = instring.chars().nth(1).unwrap();

		//used to store processed call-return pairs <Topstack>
		let mut callReturnVec = Vec::new();
		//used to store return pairs <BottomStack>
		let mut ReturnVec = Vec::new();
		match call {
			'c' => vecdeq.push_front(clonedstring),
			'r' => append_return(&mut vecdeq,clonedstring, &mut callReturnVec,&mut ReturnVec),
			_ => println!("not pushing anything"),
		} 
		//vecdeq.push_front(line);
		for eachValue in callReturnVec {
	   		println!("callReturn vec{}", eachValue);
	   	}
	   	for eachValue in ReturnVec {
	   		println!("Return vec{}", eachValue);
	   	}
	}	
	// let mut test = VecDeque::new();
	// test = vecdeq.iter().inspect(|x| println!("inspecting: {:?}", x)).collect();

	// for i in vecdeq.iter() {
	// 	println!("{:?}", i);
	// }

	// let mut s = String::new();
    // match file.read_to_string(&mut s) {
    //     Err(why) => panic!("could not open {}:{}", display,why.description()),
    //     Ok(_) => print!("{} contains: \n {}", display,s),
    // };
}

fn append_return(vecdeq: &mut VecDeque<String>,r_string: String, callReturnVec: &mut Vec<String>, ReturnVec: &mut Vec<String>) {
	let clone_r_string = r_string.clone();
	let r_tokens = r_string.split(",");
	let r_uptokens = r_tokens.skip(2);

	if vecdeq.is_empty() {
		ReturnVec.push(clone_r_string);
	} else {
		let stack_front = vecdeq.pop_front().unwrap();
		let clone_stack_front = stack_front.clone();
		let tokens = stack_front.split(",");
		let uptokens = tokens.skip(2);

		if uptokens.eq(r_uptokens) {
			callReturnVec.push(clone_stack_front + &r_string);
		} else {
	     	vecdeq.push_front(clone_stack_front);
	     	ReturnVec.push(clone_r_string);
		}
	}
}
