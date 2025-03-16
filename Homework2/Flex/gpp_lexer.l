%{
#include <stdio.h>
#include <string.h>
void handle_error(const char* text);
%}

/* Definitions */
DIGIT       [0-9]
LETTER      [a-zA-Z]
WHITESPACE  [ \t\n\r]
IDENTIFIER  [a-zA-Z_][a-zA-Z0-9_]*
INTEGER     {DIGIT}+
FRACTION    {INTEGER}[fF]{INTEGER}
COMMENT     ";;".* 

%%

"and"       { printf("KW_AND: %s\n", yytext); }
"or"        { printf("KW_OR: %s\n", yytext); }
"not"       { printf("KW_NOT: %s\n", yytext); }
"equal"     { printf("KW_EQUAL: %s\n", yytext); }
"less"      { printf("KW_LESS: %s\n", yytext); }
"nil"       { printf("KW_NIL: %s\n", yytext); }
"list"      { printf("KW_LIST: %s\n", yytext); }
"append"    { printf("KW_APPEND: %s\n", yytext); }
"concat"    { printf("KW_CONCAT: %s\n", yytext); }
"set"       { printf("KW_SET: %s\n", yytext); }
"deffun"    { printf("KW_DEFFUN: %s\n", yytext); }
"for"       { printf("KW_FOR: %s\n", yytext); }
"if"        { printf("KW_IF: %s\n", yytext); }
"exit"      { printf("KW_EXIT: %s\n", yytext); }
"load"      { printf("KW_LOAD: %s\n", yytext); }
"print"     { printf("KW_DISP: %s\n", yytext); }
"true"      { printf("KW_TRUE: %s\n", yytext); }
"false"     { printf("KW_FALSE: %s\n", yytext); }

"+"         { printf("OP_PLUS: %s\n", yytext); }
"-"         { printf("OP_MINUS: %s\n", yytext); }
"/"         { printf("OP_DIV: %s\n", yytext); }
"*"         { printf("OP_MULT: %s\n", yytext); }
"("         { printf("OP_OP: %s\n", yytext); }
")"         { printf("OP_CP: %s\n", yytext); }
","         { printf("OP_COMMA: %s\n", yytext); }

{COMMENT}   { printf("COMMENT: %s\n", yytext); }
{FRACTION}  { printf("VALUEF: %s\n", yytext); }
{INTEGER}   { printf("VALUEI: %s\n", yytext); }
{IDENTIFIER} { printf("IDENTIFIER: %s\n", yytext); }
{WHITESPACE} { /* Ignore whitespace */ }

{DIGIT}+[a-zA-Z0-9_]+  { handle_error(yytext); }
.           { handle_error(yytext); }

%%

void handle_error(const char* text) {
    printf("SYNTAX_ERROR: %s cannot be tokenized\n", text);
    exit(1);
}

int yywrap() {
    return 1;
}

int main() {
    printf("> ");
    fflush(stdout);
    yylex();
    return 0;
}