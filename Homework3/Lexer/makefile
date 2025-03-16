MAKE:
# Generate parser header
yacc -d gpp_interpreter.y

# Generate lexer
lex lex.l

# Compile the parser
gcc y.tab.c lex.yy.c -o gpp_interpreter