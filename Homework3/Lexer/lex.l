%{
#include "y.tab.h"
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
FRACTION    {INTEGER}[:]?{INTEGER}
COMMENT     ";;".*

%%

"and"       { return KW_AND; }
"or"        { return KW_OR; }
"not"       { return KW_NOT; }
"nil"       { return KW_NIL; }
"list"      { return KW_LIST; }
"append"    { return KW_APPEND; }
"concat"    { return KW_CONCAT; }
"set"       { return KW_SET; }
"deffun"    { return KW_DEFFUN; }
"for"       { return KW_FOR; }
"if"        { return KW_IF; }
"exit"      { return KW_EXIT; }
"load"      { return KW_LOAD; }
"print"     { return KW_PRINT; }
"true"      { return KW_TRUE; }
"false"     { return KW_FALSE; }

"="         { return OP_EQUAL; }
"<"         { return OP_LESS; }
">"         { return OP_MORE; }
"+"         { return OP_PLUS; }
"-"         { return OP_MINUS; }
"/"         { return OP_DIV; }
"*"         { return OP_MULT; }
"("         { return OP_OP; }
")"         { return OP_CP; }
","         { return OP_COMMA; }

\"[^\"]*\"  { 
    yylval.strval = strdup(yytext + 1); // Remove the leading quote
    yylval.strval[strlen(yylval.strval) - 1] = '\0'; // Remove the trailing quote
    return VALUEF; 
}

{COMMENT}   { /* Ignore comments */ }
{FRACTION}  { 
    yylval.strval = strdup(yytext); 
    return VALUEF; 
}
{INTEGER}   { 
    yylval.intval = atoi(yytext); 
    return VALUEI; 
}
{IDENTIFIER} { 
    yylval.strval = strdup(yytext); 
    return IDENTIFIER; 
}
{WHITESPACE} { /* Ignore whitespace */ }

{DIGIT}+[a-zA-Z0-9_]+  { handle_error(yytext); }
.           { handle_error(yytext); }

%%

void handle_error(const char* text) {
   fprintf(stderr, "SYNTAX_ERROR: %s cannot be tokenized\n", text);
   exit(1);
}

int yywrap() {
   return 1;
}
