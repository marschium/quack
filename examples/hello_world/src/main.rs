extern crate quack;

fn main(){
    let script = r"
    struct vector { x: num, y: num }

    fn print_plus_one(b: num) {
        c = add(b, 1)
        dump(c)
    }    

    fn main() {
        print_plus_one(7)
        a = new vector { 7, 8 }
        dump(a.x)
        dump(add(a.x, a.y))

        d = 1
        if lt(d, 2) {
            dump(1)
        }
    }";
    let mut q = quack::Quack::new();
    q.load(script);
    q.run();
}