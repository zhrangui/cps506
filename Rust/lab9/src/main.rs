fn main() {
    let rect = Rectangle{width: 5.0, length: 3.0};
    print!("Rectangle area: {}", rect.area());
}

struct Rectangle {
    width: f32,
    length: f32,
}

impl Rectangle {
    fn area(&self) -> f32 {
        self.width * self.length
    }
}
