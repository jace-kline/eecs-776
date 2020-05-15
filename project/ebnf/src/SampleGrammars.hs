module SampleGrammars where

numberGrammarStr :: String
numberGrammarStr = "<number> ::= <digit> {<digit>} ['.' <digit> {<digit>}]\n<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'"

gpaGrammarStr :: String
gpaGrammarStr = "<gpa> ::= '4.0'['0'] | <first-dig> '.' <decimal> {<decimal>}\n<first-dig> ::= '0'|'1'|'2'|'3'\n<decimal> ::= <first-dig> |'4'|'5'|'6'|'7'|'8'|'9'"

--expressionGrammarStr :: String
--expressionGrammarStr = "<expression> ::= <expression> '+' <term> | <expression> '-' <term> | <term>\n<term> ::= <term> '*' <factor> | <term> '/' <factor> | <factor>\n<factor> ::= <primary> '^' <factor> | <primary>\n<primary> ::= <primary> | <element>\n<element> ::= ( <expression> ) | <variable> | <number>\n<variable> ::= 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'\n<number> ::= <digit> {<digit>} ['.' <digit> {<digit>}]\n<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'"

ebnfGrammarStr :: String
ebnfGrammarStr = "<ebnf> ::= <line> {'\\n' <line>}\n<line> ::= <ws><var-wrapper><ws> '::=' <ws><rhs><ws>\n<prod-exp> ::= <var-wrapper> | <terminals> | '('<ws><prod-exp><ws>')' | '['<ws><prod-exp><ws>']' | '{'<ws><prod-exp><ws>'}' | <rhs>\n<rhs> ::= <ws><prod-exp><ws>{'|'<ws><prod-exp><ws>}\n<var-wrapper> ::= '<'<ws><variable><ws>'>'\n<variable> ::= <first-var-sym> {<var-sym>}\n<first-var-sym> ::= 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '_'\n<var-sym> ::= <first-var-sym> | <digit> | '-'\n<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'\n<terminals> ::= '\'' <terminal> {<terminal} '\''\n<terminal> ::= ' ' | '!' | '\"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']' | '^' | '_' | '`' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '{' | '|' | '}' | '~'\n<ws> ::= {' '}"


-- terminals obtained by: intercalate "|" $ map (\x -> " '" ++ [chr x] ++ "' ") $ filter (\x -> isPrint (chr x)) [0..127]