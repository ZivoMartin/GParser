use anyhow::{bail, ensure, Context, Result};
use std::collections::{HashMap, HashSet};
use std::io::Read;

#[derive(Debug)]
enum Rule {
    Atomic(String),
    Litteral(String),
    Union([Box<Rule>; 2]),
    Chain([Box<Rule>; 2]),
    EOW,
}

impl Rule {
    fn check_contains_valid_atomic(&self, name_set: &HashSet<String>) -> Result<()> {
        match self {
            Self::Atomic(content) if !name_set.contains(content) => {
                bail!("Invalid name: {content}");
            }
            Self::Union([r1, r2]) => {
                r1.check_contains_valid_atomic(name_set)?;
                r2.check_contains_valid_atomic(name_set)
            }
            Self::Chain([r1, r2]) => {
                r1.check_contains_valid_atomic(name_set)?;
                r2.check_contains_valid_atomic(name_set)
            }
            _ => Ok(()),
        }
    }
}

impl TryFrom<&str> for Box<Rule> {
    type Error = anyhow::Error;

    // ((digit + letter) * Ident2) + EOW

    fn try_from(expr: &str) -> Result<Self> {
        fn to_pf(expr: &str) -> Result<Vec<String>> {
            fn push_token(token: &mut String, output: &mut Vec<String>) {
                if !token.is_empty() {
                    output.push(token.drain(..).collect())
                }
            }
            let mut operator_stack: Vec<char> = Vec::new();
            let mut output = Vec::new();
            let mut current_token = String::new();
            'main_loop: for c in expr.chars() {
                if c == ' ' {
                    continue;
                }
                match c {
                    '(' => {
                        push_token(&mut current_token, &mut output);
                        operator_stack.push(c)
                    }
                    ')' => {
                        push_token(&mut current_token, &mut output);
                        while let Some(c) = operator_stack.pop() {
                            if c == '(' {
                                continue 'main_loop;
                            }
                            output.push(c.to_string())
                        }
                        bail!("Invalid parenthesis")
                    }
                    '+' | '*' => {
                        push_token(&mut current_token, &mut output);
                        if let Some(c) = operator_stack.last() {
                            if *c != '(' {
                                output.push(operator_stack.pop().unwrap().to_string())
                            }
                        }
                        operator_stack.push(c)
                    }
                    _ => current_token.push(c),
                }
            }

            push_token(&mut current_token, &mut output);
            while let Some(op) = operator_stack.pop() {
                if op == '(' {
                    bail!("Invalid parenthesis")
                }
                output.push(op.to_string())
            }
            Ok(output)
        }

        // digit letter + Ident2 * EOW +
        fn evaluate_pf(tokens: Vec<String>) -> Result<Box<Rule>> {
            let mut rule_stack = Vec::new();
            for token in tokens {
                let rule = match &token as &str {
                    "+" => {
                        let r2 = rule_stack.pop().context("Failed to pop r2")?;
                        let r1 = rule_stack.pop().context("Failed to pop r1")?;
                        Rule::Union([r1, r2])
                    }
                    "*" => {
                        let r2 = rule_stack.pop().context("Failed to pop r2")?;
                        let r1 = rule_stack.pop().context("Failed to pop r1")?;
                        Rule::Chain([r1, r2])
                    }
                    "EOW" => Rule::EOW,
                    _ => match token.strip_suffix("\"") {
                        Some(lit) => match lit.strip_prefix("\"") {
                            Some(lit) => Rule::Litteral(lit.to_string()),
                            None => bail!("Invalid name {token}"),
                        },
                        None => Rule::Atomic(token),
                    },
                };
                rule_stack.push(Box::new(rule))
            }
            Ok(rule_stack.pop().context("Unexpected error")?)
        }

        let pf = to_pf(expr)?;
        evaluate_pf(pf)
    }
}

#[derive(Debug)]
enum State {
    Nothing,
    Alphabets,
    Tokens,
}

impl State {
    fn next(&mut self, line: &str) -> Result<bool> {
        Ok(if let Some(line) = line.strip_prefix("#") {
            *self = match line {
                "Alphabets" => State::Alphabets,
                "Tokens" => State::Tokens,
                _ => bail!("Invalid state definition: {line}"),
            };
            true
        } else {
            false
        })
    }
}

#[derive(Debug)]
struct Alphabet {
    alphabet: HashSet<char>,
}

#[derive(Debug)]
struct ConfParser {
    state: State,
    alphabets: HashMap<String, Alphabet>,
    rules: HashMap<String, Box<Rule>>,
}

impl ConfParser {
    fn new() -> Self {
        Self {
            state: State::Nothing,
            alphabets: HashMap::new(),
            rules: HashMap::new(),
        }
    }

    fn read_alph_decl(&mut self, line: &str) -> Result<()> {
        let (name, quotes) = line
            .split_once("=")
            .context("Invalid alphabet declaration")?;
        let (name, quotes) = (name.trim(), quotes.trim());
        ensure!(
            quotes.starts_with("\"") && quotes.ends_with("\"") && quotes.len() > 1,
            "Invalid quotes for {name} declaration"
        );
        ensure!(
            !self.rules.contains_key(name),
            "The name {name} is already an rule"
        );

        if self
            .alphabets
            .insert(
                name.to_string(),
                Alphabet {
                    alphabet: quotes[1..quotes.len() - 1].chars().collect(),
                },
            )
            .is_some()
        {
            bail!("You declared the alphabet {name} twice")
        }
        Ok(())
    }

    fn read_token_decl(&mut self, line: &str) -> Result<()> {
        let (name, expr) = line
            .split_once("=")
            .context("Invalid alphabet declaration")?;
        let (name, expr) = (name.trim(), expr.trim());
        let rule = Box::<Rule>::try_from(expr)?;
        ensure!(
            !self.alphabets.contains_key(name),
            "The name {name} is already an alphabet"
        );
        if self.rules.insert(name.to_string(), rule).is_some() {
            bail!("You declared the rule {name} twice")
        }
        Ok(())
    }

    fn parse(&mut self, content: String) -> Result<()> {
        for line in content
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty())
        {
            if self.state.next(line)? {
                continue;
            }
            match self.state {
                State::Alphabets => self.read_alph_decl(line),
                State::Tokens => self.read_token_decl(line),
                State::Nothing => bail!("Please declare the state before starting anything"),
            }?
        }
        let name_set = self
            .rules
            .keys()
            .cloned()
            .chain(self.alphabets.keys().cloned())
            .collect::<HashSet<String>>();

        for rule in self.rules.values() {
            rule.check_contains_valid_atomic(&name_set)?;
        }
        Ok(())
    }
}

fn main() -> Result<()> {
    let path = std::env::args().nth(1).context("Please provide a path")?;
    let mut f = std::fs::File::open(path).context("Invalid path")?;
    let mut content = String::new();
    f.read_to_string(&mut content)
        .context("Failed to read the content of the file")?;
    let mut parser = ConfParser::new();
    parser.parse(content)?;

    Ok(())
}
