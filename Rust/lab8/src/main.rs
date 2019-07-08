use core::f32::consts::PI;

fn main() {
    let a = area(2.0);
    println!("Circle area: {}", a);
}

fn area (r: f32) -> f32 {
    return 4.0*PI*r*r;
}