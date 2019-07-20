#[cfg(test)]
mod tests {
    use core::f64::consts::PI;

    extern crate lab10;
    #[test]
    fn test_area() {
        let c = Circle {
            r: 2.0;
        }
        assert_eq(c.area(), PI*2.0.powf(2.0) )
    }
}