use std::collections::HashMap;
use std::fmt;
use std::num::ParseFloatError;
use std::io::Read;

#[derive(Clone)]
enum Token {
    Op(String),
    Num(f64),
    List(Vec<Token>),
    Func(fn(&[Token]) -> Result<Token, TokenErr>),
}

#[derive(Debug)]
enum TokenErr {
    Reason(String),
}

#[derive(Clone)]
struct TokenEnv {
    data: HashMap<String, Token>,
}

fn tokenizer(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(Token, &'a [String]), TokenErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(TokenErr::Reason("could not get token".to_string()))?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(TokenErr::Reason("unexpected ')".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(Token, &'a [String]), TokenErr> {
    let mut res: Vec<Token> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(TokenErr::Reason("could not find closing ')'".to_string()))?;
        if next_token == ")" {
            return Ok((Token::List(res), rest));
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> Token {
    let potential_float: Result<f64, ParseFloatError> = token.parse();
    match potential_float {
        Ok(v) => Token::Num(v),
        Err(_) => Token::Op(token.to_string().clone()),
    }
}

fn default_env() -> TokenEnv {
    let mut data: HashMap<String, Token> = HashMap::new();
    data.insert(
        "+".to_string(),
        Token::Func(|args: &[Token]| -> Result<Token, TokenErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);

            Ok(Token::Num(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        Token::Func(|args: &[Token]| -> Result<Token, TokenErr> {
            let floats = parse_list_of_floats(args)?;
            let first = *floats
                .first()
                .ok_or(TokenErr::Reason("expected at least one number".to_string()))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(Token::Num(first - sum_of_rest))
        }),
    );

    TokenEnv { data }
}

fn parse_list_of_floats(args: &[Token]) -> Result<Vec<f64>, TokenErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &Token) -> Result<f64, TokenErr> {
    match exp {
        Token::Num(num) => Ok(*num),
        _ => Err(TokenErr::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &Token, env: &mut TokenEnv) -> Result<Token, TokenErr> {
    match exp {
        Token::Op(k) => env
            .data
            .get(k)
            .ok_or(TokenErr::Reason(format!("unexpected symbol k='{}'", k)))
            .map(|x| x.clone()),
        Token::Num(_a) => Ok(exp.clone()),
        Token::List(list) => {
            let first_form = list
                .first()
                .ok_or(TokenErr::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                Token::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<Token>, TokenErr>>();
                    f(&args_eval?)
                }
                _ => Err(TokenErr::Reason(
                    "first form must be a function".to_string(),
                )),
            }
        }
        Token::Func(_) => Err(TokenErr::Reason("unexpected form".to_string())),
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Token::Op(s) => s.clone(),
            Token::Num(n) => n.to_string(),
            Token::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            Token::Func(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut TokenEnv) -> Result<Token, TokenErr> {
    let (parsed_exp, _) = parse(&tokenizer(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    std::io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                TokenErr::Reason(msg) => println!("// 🙀 => {}", msg),
            },
        }
    }
}
