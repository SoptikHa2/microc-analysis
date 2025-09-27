module Lex.Tokens (Token(..), Keyword(..), strToKw) where

data Token
    = ParenOpen
    | ParenClose
    | Identifier String
    | CBracketOpen
    | CBracketClose
    | Keyword Keyword
    | Semicolon
    | Comma
    | Assign -- =
    | Plus
    | Minus
    | Star
    | Division
    | And
    | Dot
    | Colon
    | Number Int
    | Gt
    | Eq

data Keyword
    = Var
    | Return
    | If
    | Else
    | While
    | Output
    | Input
    | Alloc
    | Null

strToKw :: [(String, Keyword)]
strToKw = [
    ("var", Var),
    ("pro", Var),
    ("return", Return),
    ("vrať", Return),
    ("if", If),
    ("když", If),
    ("else", Else),
    ("jinak", Else),
    ("while", While),
    ("dokavaď", While),
    ("dokud", While),
    ("output", Output),
    ("vypiš", Output),
    ("input", Input),
    ("načti", Input),
    ("alloc", Alloc),
    ("zaveď", Alloc),
    ("null", Null),
    ("nic", Null)
    ]