mod mod0 {
    fn hello_mod0() -> String {
        "hello mod0".to_string()
    }
}

mod mod1 {
    fn hello_mod1() -> String {
        "hello mod1".to_string()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
