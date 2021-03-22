use serde_json::Value;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub error: String,
    pub cause: String,
}

pub type Result = std::result::Result<Value, Error>;

pub trait MockResource {
    fn execute(&mut self, input: &Value) -> Result;
}

pub struct Identity {}
impl MockResource for Identity {
    fn execute(&mut self, input: &Value) -> Result {
        Ok(input.clone())
    }
}

pub fn identity() -> Box<dyn MockResource> {
    Box::new(Identity {})
}

pub struct Constant {
    value: Value,
}
impl MockResource for Constant {
    fn execute(&mut self, _: &Value) -> Result {
        Ok(self.value.clone())
    }
}
impl Constant {
    pub fn new(value: Value) -> Constant {
        Constant { value }
    }
}
pub fn constant(value: Value) -> Box<dyn MockResource> {
    Box::new(Constant::new(value))
}

struct Function<T> {
    func: T,
}

impl<T> MockResource for Function<T>
where
    T: FnMut(&Value) -> Result,
{
    fn execute(&mut self, input: &Value) -> Result {
        (self.func)(input)
    }
}

pub fn function<T>(func: T) -> Box<dyn MockResource>
where
    T: 'static + FnMut(&Value) -> Result,
{
    Box::new(Function { func })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Value};

    #[test]
    fn constant() {
        let inputs = vec![
            json!(null),
            json!(1234),
            json!({"key": {"inner_key": "value"}}),
            json!("string"),
            json!([1, 2, 3, 4]),
        ];
        for cons in inputs.iter() {
            let mut c = Constant::new(cons.clone());
            for input in inputs.iter() {
                let result = c.execute(input);
                assert_eq!(
                    result,
                    Ok(cons.clone()),
                    "executing Constant({}) on input {} work",
                    cons,
                    input
                )
            }
        }
    }

    #[test]
    fn identity() {
        let inputs = vec![
            json!(null),
            json!(1234),
            json!({"key": {"inner_key": "value"}}),
            json!("string"),
            json!([1, 2, 3, 4]),
        ];
        let mut c = Identity {};
        for input in inputs.iter() {
            let result = c.execute(input);
            assert_eq!(
                result,
                Ok(input.clone()),
                "executing Indentity on input {} work",
                input
            )
        }
    }

    #[test]
    fn function_nest() {
        let inputs = vec![
            json!(null),
            json!(1234),
            json!({"key": {"inner_key": "value"}}),
            json!("string"),
            json!([1, 2, 3, 4]),
        ];
        let mut func = Function {
            func: |v: &Value| Ok(json!({ "nested": v })),
        };
        for input in inputs {
            let result = func.execute(&input);
            assert_eq!(result, Ok(json!({ "nested": input })))
        }
    }

    #[test]
    fn function_mut() {
        let mut counter = 0;
        let func = |_: &Value| {
            counter += 1;
            Ok(json!(counter))
        };
        let mut func_resource = Function { func };
        assert_eq!(json!(1), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(2), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(3), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(4), func_resource.execute(&json!(null)).unwrap());
    }

    #[test]
    fn function_move() {
        let mut counter = 0;
        let func = move |_: &Value| {
            counter += 1;
            Ok(json!(counter))
        };
        let mut func_resource = Function { func };
        assert_eq!(json!(1), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(2), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(3), func_resource.execute(&json!(null)).unwrap());
        assert_eq!(json!(4), func_resource.execute(&json!(null)).unwrap());
    }
}
