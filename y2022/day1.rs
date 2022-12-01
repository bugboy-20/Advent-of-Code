use std::io::{BufRead, BufReader};
use std::fs::File;

//#[derive(Clone)]
#[derive(Copy, Clone)]
struct Elf {
    calories : i32
}

impl Elf {
    fn new() -> Elf {
        Elf {
            calories : 0,
        }
    }

    fn add_food(&mut self, q : i32) {
        self.calories += q
    }
}

fn sum(a : [i32;3]) -> i32 {
    let mut s=0;
    for e in a {
        print!("{} ",e);
        s += e;
    }
    s
}
/*
fn swap<T>(a : &mut T, b : &mut T) where T : Copy {
    let tmp = a;
    a = b;
    b = tmp;

    ()
}*/
fn main() {
    //let elfS;
    let max;
    (max,_) = calories("./day1");

    print!("{}",sum(max));

}

fn calories(path : &str) -> ([i32;3],Vec<Elf>) {
    let mut max=[0,0,0];
    let mut e = Elf::new();
    let mut elves : Vec<Elf> = Vec::new();
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);

    for line in reader.lines() {
        match line.unwrap().as_str() {
            "" => {
                let mut d = e.calories;
                for i in 0..3 {
                    if d > max[i] {
                        let tmp = d;
                        d = max[i];
                        max[i] = tmp;
                    }
                }
                elves.push(e);
                e.calories = 0;

            }

            n => {
                let q : i32 = String::from(n).parse().expect("numero :(");
                e.add_food(q);
            }

        }
    }

    (max,elves)

}
