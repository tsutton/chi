use choice_macro::{choice_match, gen_enum};
use serde_json::{json, Value};

gen_enum! {}

#[allow(unused)]
fn m() -> Result<bool, String> {
    let input = json!({});
    let input = &input;
    let variable = "$".to_string();
    let variable = &variable;
    let expr = DataTestExpression::StringEquals("asfd".to_string());
    choice_match!()
}

fn select_one<'a>(input: &'a Value, _: &str) -> Result<&'a Value, String> {
    Ok(input)
}

// Running cargo expand --lib and searching for this module (choice_with_rs), the output of the macro is:
mod output {
    pub enum DataTestExpression {
        StringEqualsPath(String),
        StringEquals(String),
        StringLessThanPath(String),
        StringLessThan(String),
        StringLessThanEqualsPath(String),
        StringLessThanEquals(String),
        StringGreaterThanPath(String),
        StringGreaterThan(String),
        StringGreaterThanEqualsPath(String),
        StringGreaterThanEquals(String),
        TimestampEqualsPath(String),
        TimestampEquals(String),
        TimestampLessThanPath(String),
        TimestampLessThan(String),
        TimestampLessThanEqualsPath(String),
        TimestampLessThanEquals(String),
        TimestampGreaterThanPath(String),
        TimestampGreaterThan(String),
        TimestampGreaterThanEqualsPath(String),
        TimestampGreaterThanEquals(String),
        NumericEqualsPath(String),
        NumericEquals(::serde_json::Value),
        NumericLessThanPath(String),
        NumericLessThan(::serde_json::Value),
        NumericLessThanEqualsPath(String),
        NumericLessThanEquals(::serde_json::Value),
        NumericGreaterThanPath(String),
        NumericGreaterThan(::serde_json::Value),
        NumericGreaterThanEqualsPath(String),
        NumericGreaterThanEquals(::serde_json::Value),
    }
    #[allow(unused)]
    fn m() -> Result<bool, String> {
        let input = ::serde_json::Value::Object(::serde_json::Map::new());
        let input = &input;
        let variable = "$".to_string();
        let variable = &variable;
        let expr = DataTestExpression::StringEquals("asfd".to_string());
        match expr {
            DataTestExpression::StringEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::StringEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::StringLessThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::StringLessThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::StringLessThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::StringLessThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::StringGreaterThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::StringGreaterThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::StringGreaterThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::StringGreaterThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::TimestampEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::TimestampEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::TimestampLessThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::TimestampLessThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::TimestampLessThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::TimestampLessThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::TimestampGreaterThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::TimestampGreaterThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::TimestampGreaterThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::TimestampGreaterThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::NumericEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_eq())
            }
            DataTestExpression::NumericEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_eq())
            }
            DataTestExpression::NumericLessThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_lt())
            }
            DataTestExpression::NumericLessThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_lt())
            }
            DataTestExpression::NumericLessThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_le())
            }
            DataTestExpression::NumericLessThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_le())
            }
            DataTestExpression::NumericGreaterThanPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_gt())
            }
            DataTestExpression::NumericGreaterThan(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_gt())
            }
            DataTestExpression::NumericGreaterThanEqualsPath(right_hand_side) => {
                let right_hand_side = select_one(input, &right_hand_side);
                let right_hand_side = match right_hand_side {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_ge())
            }
            DataTestExpression::NumericGreaterThanEquals(right_hand_side) => {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64() {
                        None => return Ok(false),
                        Some(f) => f,
                    },
                    _ => return Ok(false),
                };
                let order = match left_hand_side.partial_cmp(&right_hand_side) {
                    None => return Ok(false),
                    Some(o) => o,
                };
                Ok(order.is_ge())
            }
        }
    }
    fn select_one<'a>(
        input: &'a serde_json::Value,
        _: &str,
    ) -> Result<&'a serde_json::Value, String> {
        Ok(input)
    }
}
