use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::io::BufWriter;
use std::collections::VecDeque;
use std::convert::From;
//File open options https://doc.rust-lang.org/std/fs/struct.OpenOptions.html#method.open
use std::fs::OpenOptions;

fn main() {
    let path = Path::new("test");
    let display = path.display();

    let file = match File::open(&path) {
    	Err(why) => panic!("could not open {}:{}", display,why.description()),
    	Ok(file) => file,
    };

    let f = BufReader::new(file);

    //Open a file if it exists,otherwise create and open with write access
	let outputFile = match OpenOptions::new().write(true).create(true).open("flameOut"){
		Ok(file) => file,
		Err(why) => panic!("could not open {}:{}", display,why.description()),
	};

	let mut writer = BufWriter::new(&outputFile);

//read call enties from file and push it to vector;return entries should be matched
//with top-to-bottom of call entries; if no return is matching, push it back of vector;
//once all the matching calls are exhausted, store the vector and continue.
	let mut vecdeq = VecDeque::new();
		//used to store processed call-return pairs <Topstack>
		let mut callReturnVec = Vec::new();
		//used to store return pairs <BottomStack>
		let mut ReturnVec = Vec::new();
	for line in f.lines() {
		// println!("Each line is: {:?}", line);
		let instring = line.unwrap().to_string();
		let clonedstring = instring.clone();
		let call = instring.chars().nth(1).unwrap();
		match call {
			'c' => check_stack_complete(&mut vecdeq,clonedstring, &mut callReturnVec,&mut ReturnVec,&mut writer),
			'r' => append_return(&mut vecdeq,clonedstring, &mut callReturnVec,&mut ReturnVec),
			_ => println!("not pushing anything"),
		} 

	}	
	//Below END variable doesnt signify anything,just for the sake of sending
	let END = "END".to_string();
		check_stack_complete(&mut vecdeq,END, &mut callReturnVec,&mut ReturnVec,&mut writer);

		// println!("Before the print loop");
		// for eachValue in callReturnVec {
	 //   		println!("callReturn vec{}", eachValue);
	 //   	}
	 //   	for eachValue in ReturnVec {
	 //   		println!("Return vec{}", eachValue);
	 //   	}

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

fn check_stack_complete(vecdeq: &mut VecDeque<String>,clonedstring: String, callReturnVec:&mut Vec<String>,ReturnVec: &mut Vec<String>,outputFile: &mut BufWriter<&std::fs::File>){
	//For the first time vecdeq will be empty,should only try to collect/print/write stack
	//if callReturnVec holds something
	// println!("check_stack_complete vecdeq{:?}", vecdeq);
	if !callReturnVec.is_empty(){
		//When one stack is complete, vecdeq will turn empty and starts afresh for new stack
		if vecdeq.is_empty(){
			//create stack for flamegraph
			to_flame_graph(&mut callReturnVec.clone(),ReturnVec.clone(),outputFile);
			//After one stack is complete, clear old stack to start new one
			callReturnVec.clear();
			ReturnVec.clear();
		}
	}
	vecdeq.push_front(clonedstring);
}

fn to_flame_graph(callReturnVec: &mut Vec<String>,ReturnVec: Vec<String>,outputFile: &mut BufWriter<&std::fs::File>){
	println!("printing to flamegraph");
	let mut flameStackVec: Vec<&str> = Vec::new();
	let ReturnVeclength = ReturnVec.len();
	for i in 0..ReturnVeclength{
		flameStackVec.push(&ReturnVec[ReturnVeclength-1-i]);
		for j in flameStackVec.iter(){
			// print!("{};", j);
			write!(outputFile,"{};",j);
		}
		let mut timediff = ReturnVec[ReturnVeclength-1-i].split(',');
		let Time = match timediff.nth(1){
			Some(x) => x,
			None => " 0",
		};
		write!(outputFile," {}",Time);
		// print!("\n");
		write!(outputFile,"\n");
	}
	let callReturnVeclength = callReturnVec.len();
	for i in 0..callReturnVeclength{
		//We need to reverse the vector stack
		let mut CallReturnMapVectorTokens: Vec<&str> = callReturnVec[callReturnVeclength-1-i].split(';').collect();
		let CallMapVecToken = CallReturnMapVectorTokens[0];
		let ReturnMapVecToken = CallReturnMapVectorTokens[1];

		//Getting the call tuple string
		let mut CallMapVec = CallMapVecToken.split(',');
		let StartTimeString = match CallMapVec.nth(1){
			Some(x) => x,
			None => "0"
		};
		let StartTime = StartTimeString.parse::<u32>().ok().unwrap();

		//Getting the return tuple string
		let mut ReturnMapVec = ReturnMapVecToken.split(',');
		let EndTimeString = match ReturnMapVec.nth(1){
			Some(x) => x,
			None => "0"
		};
		let EndTime = EndTimeString.parse::<u32>().ok().unwrap();

		let TimeDiff;
		//Time difference between call and its return
		TimeDiff = EndTime - StartTime;

		flameStackVec.push(&CallMapVecToken);
	
		for i in flameStackVec.iter(){
			// print!("{};", i);
			write!(outputFile,"{};",i);
		}
		// print!(" {}", TimeDiff);
		write!(outputFile," {}",TimeDiff);
		// println!("");
		write!(outputFile,"\n");
	}
}
