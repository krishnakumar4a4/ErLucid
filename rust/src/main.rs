use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::collections::VecDeque;
use std::convert::From;

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
		//used to store processed call-return pairs <Topstack>
		let mut callReturnVec = Vec::new();
		//used to store return pairs <BottomStack>
		let mut ReturnVec = Vec::new();
	for line in f.lines() {
		println!("Each line is: {:?}", line);
		let instring = line.unwrap().to_string();
		let clonedstring = instring.clone();
		// println!("pushing into double ended vectorq:{:?}",instring);
		// println!("{:?}", instring.chars().nth(1));
		let call = instring.chars().nth(1).unwrap();
		match call {
			'c' => check_stack_complete(&mut vecdeq,clonedstring, &mut callReturnVec,&mut ReturnVec),
			'r' => append_return(&mut vecdeq,clonedstring, &mut callReturnVec,&mut ReturnVec),
			_ => println!("not pushing anything"),
		} 

	}	
			//vecdeq.push_front(line);
		println!("Before the print loop");
		for eachValue in callReturnVec {
	   		println!("callReturn vec{}", eachValue);
	   	}
	   	for eachValue in ReturnVec {
	   		println!("Return vec{}", eachValue);
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
			callReturnVec.push(clone_stack_front + ";" + &r_string);
		} else {
	     	vecdeq.push_front(clone_stack_front);
	     	ReturnVec.push(clone_r_string);
		}
	}
}

fn check_stack_complete(vecdeq: &mut VecDeque<String>,clonedstring: String, callReturnVec:&mut Vec<String>,ReturnVec: &mut Vec<String>){
	//For the first time vecdeq will be empty,should only try to collect/print/write stack
	//if callReturnVec holds something
	if !callReturnVec.is_empty(){
		//When one stack is complete, vecdeq will turn empty and starts afresh for new stack
		if vecdeq.is_empty(){
			println!("Printing callReturn vector");
			for i in callReturnVec.iter() {
				println!("{:?}", i);
			}
			println!("Printing return vector");
			for i in ReturnVec.iter() {
				println!("{:?}", i);
			}
			//create stack for flamegraph
			to_flame_graph(&mut callReturnVec.clone(),ReturnVec.clone());
			//After one stack is complete, clear old stack to start new one
			callReturnVec.clear();
			ReturnVec.clear();
		}
	}
	vecdeq.push_front(clonedstring);
}

fn to_flame_graph(callReturnVec: &mut Vec<String>,ReturnVec: Vec<String>){
	// let mut CallString;
	println!("printing to flamegraph");
	let mut flameStackVec: Vec<&str> = Vec::new();
	// let mut RefCounterVec = Vec::new();
	let ReturnVeclength = ReturnVec.len();
	for i in 0..ReturnVeclength{
		flameStackVec.push(&ReturnVec[ReturnVeclength-1-i]);
		for j in flameStackVec.iter(){
			print!("{};", j);
		}
		let mut timediff = ReturnVec[ReturnVeclength-1-i].split(',');
		match timediff.nth(1){
			Some(x) => print!(" {}", x),
			None => print!(" 0",)
		};
		print!("\n");
	}
	let callReturnVeclength = callReturnVec.len();
	// println!("callReturnVeclength: {:?}", callReturnVeclength);
	for i in 0..callReturnVeclength{
		
		// println!("index {:?}", i);
		//We need to reverse the vector stack
		// let CallReturnMapVector: Vec<&str> = callReturnVec[callReturnVeclength-1-i].split(';').collect();
		let mut CallReturnMapVectorTokens: Vec<&str> = callReturnVec[callReturnVeclength-1-i].split(';').collect();
		let CallMapVecToken = CallReturnMapVectorTokens[0];
		let ReturnMapVecToken = CallReturnMapVectorTokens[1];
		// println!("calltoken {:?}", CallMapVecToken);
		// println!("calltoken {:?}", ReturnMapVecToken);	
		//Getting the call tuple string
		let mut CallMapVec = CallMapVecToken.split(',');
		let StartTimeString = match CallMapVec.nth(1){
			Some(x) => x,
			None => "0"
		};
		let StartTime = StartTimeString.parse::<u32>().ok().unwrap();
		// println!("{}", StartTime);

		//Getting the return tuple string

		let mut ReturnMapVec = ReturnMapVecToken.split(',');
		let EndTimeString = match ReturnMapVec.nth(1){
			Some(x) => x,
			None => "0"
		};
		let EndTime = EndTimeString.parse::<u32>().ok().unwrap();
		// println!("{}", EndTime);
		//Getting the return tuple string
		// let ReturnMapVector: Vec<&str> = CallReturnMapVectorTokens.nth(1).unwrap().split(',').collect();
		// let StartTime = CallMapVector[1].parse::<u32>();
		// let EndTime = ReturnMapVector[1].parse::<u32>();
		let TimeDiff;
		//Time difference between call and its return
		TimeDiff = EndTime - StartTime;
		// println!("call start {},returns {}", CallMapVector[1],ReturnMapVector[1]);
		// println!("Time difference {:?}", TimeDiff);
		// let CallString;
		// // let UpdatedCallString;
		// CallString = CallReturnMapVectorTokens.nth(0).unwrap();
		// // UpdatedCallString = CallString.to_string().clone();
		// println!("{:?}", CallString);
		flameStackVec.push(&CallMapVecToken);
		// RefCounterVec.push(TimeDiff);
		// for j in CallReturnMapVector.iter(){
		// 	println!("Printing map{:?}", j);
		// }
		for i in flameStackVec.iter(){
			print!("{};", i);
		}
		print!(" {}", TimeDiff);
		println!("");
	}
}
