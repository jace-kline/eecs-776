module SampleGrammars where

numberGrammarStr :: String
numberGrammarStr = "<number> ::= <digit> {<digit>} ['.' <digit> {<digit>}]\n<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'"

gpaGrammarStr :: String
gpaGrammarStr = "<gpa> ::= '4.0'['0'] | <first-dig> '.' <decimal> {<decimal>}\n<first-dig> ::= '0'|'1'|'2'|'3'\n<decimal> ::= <first-dig> |'4'|'5'|'6'|'7'|'8'|'9'"

expressionGrammarStr :: String
expressionGrammarStr = "<expression> ::= <expression> '+' <term> | <expression> '-' <term> | <term>\n<term> ::= <term> '*' <factor> | <term> '/' <factor> | <factor>\n<factor> ::= <primary> '^' <factor> | <primary>\n<primary> ::= <primary> | <element>\n<element> ::= ( <expression> ) | <variable> | <number>"

-- ebnfGrammarStr :: String
-- ebnfGrammarStr = ""