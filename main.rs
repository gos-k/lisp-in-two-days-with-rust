#[derive(Debug)]
enum TokeniseState {
    Start,
    LParen,
    RParen,
    Number,
    Symbol,
    WhiteSpace,
}
