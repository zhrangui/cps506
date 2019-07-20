pub mod lab10 {
    use core::f64::consts::PI;
    pub struct Circle<T> {
        pub r: T
    }

    pub trait Shape<T> {
        fn area (&self) -> T;
    }

    impl Shape<f64> for Circle<f64> {
        fn area(&self) -> f64 {
            PI*self.r.powf(2.0)
        }
    }
}
use lab10::Shape;

fn main() {
    let c = lab10::Circle{
        r: 4.0
    };
    let a = c.area();
    println!("Circle area: {}", a);
}
