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
    fn id(&self) -> usize {
        (&*self as *const Rule) as usize
    }

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
            let mut quotes = false;
            'main_loop: for c in expr.chars() {
                if c == '\"' {
                    quotes = !quotes;
                }
                match c {
                    _ if quotes => current_token.push(c),
                    ' ' => (),
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

enum NextCharResponse<'a> {
    Reject,
    Keep,
    Out,
    Replace(Vec<Candidate<'a>>),
}

#[derive(Debug)]
struct Candidate<'a> {
    rule: &'a Rule,
    tokens: Vec<String>,
    current_token: String,
    path: Vec<Vec<&'a Rule>>,
}

impl<'a> Candidate<'a> {
    fn get_candidates(rules: &'a HashMap<String, Box<Rule>>, rule: &'a Rule) -> Vec<Self> {
        let mut res = HashMap::new();
        Self::get_candidates_rec(&mut res, Vec::new(), String::new(), Vec::new(), rules, rule);
        res.values().collect()
    }

    fn get_candidates_rec(
        mut candidates: &mut HashMap<usize, Candidate<'a>>,
        mut current_path: Vec<&'a Rule>,
        current_token: String,
        tokens: Vec<String>,
        rules: &'a HashMap<String, Box<Rule>>,
        rule: &'a Rule,
    ) {
        match rule {
            Rule::Atomic(name) => match rules.get(name) {
                Some(the_rule) => {
                    if let Some(candidate) = candidates.get_mut(&the_rule.id()) {
                        candidate.path.push(current_path)
                    } else {
                        Candidate::get_candidates_rec(
                            candidates,
                            current_path,
                            current_token,
                            tokens,
                            rules,
                            the_rule,
                        )
                    }
                }
                None => candidates.insert(
                    rule.id(),
                    Candidate {
                        path: vec![current_path],
                        rule,
                        current_token,
                        tokens,
                    },
                ),
            },
            Rule::Litteral(_) | Rule::EOW => candidates.insert(
                rule.id(),
                Candidate {
                    path: vec![current_path],
                    rule,
                    current_token,
                    tokens,
                },
            ),
            Rule::Union([r1, r2]) => {
                Candidate::get_candidates_rec(
                    candidates,
                    current_path.clone(),
                    current_token.clone(),
                    tokens.clone(),
                    rules,
                    r1,
                );
                Candidate::get_candidates_rec(
                    candidates,
                    current_path,
                    current_token,
                    tokens,
                    rules,
                    r2,
                );
            }
            Rule::Chain([r1, _]) => {
                current_path.push(rule);
                Candidate::get_candidates_rec(
                    candidates,
                    current_path,
                    current_token,
                    tokens,
                    rules,
                    r1,
                )
            }
        }
    }

    fn drain_current_token(&mut self) {
        if !self.current_token.is_empty() {
            self.tokens.push(self.current_token.drain(..).collect())
        }
    }

    fn pop_path(&mut self, rules: &'a HashMap<String, Box<Rule>>) -> NextCharResponse<'a> {
        let mut res = Vec::new();
        for path in self.path {
            match path.pop() {
                Some(Rule::Chain([_, r2])) => {
                    let mut map = HashMap::new();
                    Self::get_candidates_rec(
                        &mut map,
                        self.path.drain(..).collect(),
                        self.current_token.drain(..).collect(),
                        self.tokens.drain(..).collect(),
                        rules,
                        &r2,
                    );
                    res.append(&mut map.values().collect())
                }
                None => (),
                _ => unreachable!(),
            }
        }
    }

    fn get_next_candidates(
        candidates: Vec<Candidate<'a>>,
        c: char,
        alphabets: &HashMap<String, Alphabet>,
        rules: &'a HashMap<String, Box<Rule>>,
    ) -> Result<Vec<Candidate<'a>>> {
        let mut new_candidates = Vec::new();
        for mut candidate in candidates.into_iter() {
            match candidate.next_char(alphabets, rules, c)? {
                NextCharResponse::Keep => new_candidates.push(candidate),
                NextCharResponse::Replace(mut to_add) => new_candidates.append(&mut to_add),
                NextCharResponse::Out => new_candidates.push(Candidate {
                    rule: &Rule::EOW,
                    path: Vec::new(),
                    tokens: candidate.tokens.drain(..).collect(),
                    current_token: candidate.current_token.drain(..).collect(),
                }),
                NextCharResponse::Reject => (),
            }
        }
        Ok(new_candidates)
    }

    fn next_char(
        &mut self,
        alphabets: &HashMap<String, Alphabet>,
        rules: &'a HashMap<String, Box<Rule>>,
        c: char,
    ) -> Result<NextCharResponse<'a>> {
        Ok(match self.rule {
            Rule::Atomic(name) => {
                self.current_token.push(c);
                if alphabets
                    .get(name)
                    .context("Alphebet should be there")?
                    .alphabet
                    .contains(&c)
                {
                    self.pop_path(rules)
                } else {
                    NextCharResponse::Reject
                }
            }
            Rule::Litteral(lit) => {
                self.current_token.push(c);
                if *lit == self.current_token {
                    self.drain_current_token();
                    self.pop_path(rules)
                } else if lit.starts_with(&self.current_token) {
                    NextCharResponse::Keep
                } else {
                    NextCharResponse::Reject
                }
            }
            Rule::EOW => {
                if self.path.is_empty() && c != '\0' {
                    return Ok(NextCharResponse::Reject);
                }
                self.drain_current_token();
                let response = self.pop_path(rules);
                match response {
                    NextCharResponse::Replace(new_candidates) => NextCharResponse::Replace(
                        Self::get_next_candidates(new_candidates, c, alphabets, rules)?,
                    ),
                    _ => response,
                }
            }
            _ => unreachable!(),
        })
    }
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

    fn init(&mut self, content: String) -> Result<()> {
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
        ensure!(
            self.rules.contains_key("MAIN"),
            "You should provide an entry point for the grammar"
        );

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

    fn lex(&self, text: &str) -> Result<Vec<String>> {
        let entry = self.rules.get("MAIN").context("MAIN is missing")?;
        let mut candidates = Candidate::get_candidates(&self.rules, entry);
        for c in text.chars() {
            println!("{}", candidates.len());
            candidates =
                Candidate::get_next_candidates(candidates, c, &self.alphabets, &self.rules)?
        }
        // println!(
        //     "{:?}",
        //     candidates
        //         .iter()
        //         .map(|c| (c.rule, c.tokens.clone(), c.current_token.clone()))
        //         .collect::<Vec<_>>()
        // );

        candidates.retain_mut(|candidate| {
            matches!(
                candidate.next_char(&self.alphabets, &self.rules, '\0'),
                Ok(NextCharResponse::Out)
            )
        });
        if candidates.is_empty() {
            bail!("Syntax error")
        } else {
            Ok(candidates.pop().unwrap().tokens)
        }
    }
}

fn main() -> Result<()> {
    let path = std::env::args().nth(1).context("Please provide a path")?;
    let mut f = std::fs::File::open(path).context("Invalid path")?;
    let mut content = String::new();
    f.read_to_string(&mut content)
        .context("Failed to read the content of the file")?;
    let mut parser = ConfParser::new();
    parser.init(content)?;
    println!("{:?}", parser.lex("(((1+1)+100*zidjh))")?);
    Ok(())
}
