%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External function from lexer
extern int yylex();
extern int yylineno;
extern char* yytext;

// Error reporting function
void yyerror(const char *s) {
    fprintf(stderr, "Syntax Error: %s at line %d near '%s'\n", s, yylineno, yytext);
}

// Struct to represent parse tree nodes
typedef struct ASTNode {
    char* type;
    char* value;
    struct ASTNode* left;
    struct ASTNode* right;
    struct ASTNode* next;  // Added for list-like structures
} ASTNode;

// Function to create a new AST node with more robust memory management
ASTNode* createNode(const char* type, const char* value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    if (node == NULL) {
        fprintf(stderr, "Memory allocation failed for AST node\n");
        exit(1);
    }
    node->type = strdup(type ? type : "UNKNOWN");
    node->value = strdup(value ? value : "");
    node->left = NULL;
    node->right = NULL;
    node->next = NULL;
    return node;
}

// Free the AST node and its children recursively
void freeASTNode(ASTNode* node) {
    if (node == NULL) return;
    
    // Recursively free children
    freeASTNode(node->left);
    freeASTNode(node->right);
    freeASTNode(node->next);
    
    // Free the node's strings
    free(node->type);
    free(node->value);
    free(node);
}

// Root of the parse tree
ASTNode* parseTree = NULL;
%}

// Union to handle different value types
%union {
    int intval;              // For integer values
    char* strval;            // For string values (used by VALUEF, IDENTIFIER, etc.)
    struct ASTNode* node;    // For parse tree nodes
}

// Token definitions (same as before)
%token OP_OP OP_CP OP_PLUS OP_MINUS OP_MULT OP_DIV OP_COMMA
%token KW_AND KW_OR KW_NOT OP_EQUAL OP_LESS OP_MORE
%token KW_NIL KW_LIST KW_APPEND KW_CONCAT
%token KW_SET KW_DEFFUN KW_FOR KW_IF KW_ELSE KW_EXIT KW_LOAD KW_PRINT
%token KW_TRUE KW_FALSE
%token <strval> IDENTIFIER
%token <intval> VALUEI
%token <strval> VALUEF

// Non-terminal types
%type <node> program 
%type <node> input 
%type <node> exp 
%type <node> explist 
%type <node> list 
%type <node> values 
%type <node> function_def 
%type <node> function_call
%type <node> id_list
%type <node> control_structure
%type <node> load_statement
%type <node> exit_statement
%type <node> print_statement
%type <node> assignment
%type <node> if_statement

// Resolve shift/reduce conflicts by specifying precedence
%left OP_PLUS OP_MINUS
%left OP_MULT OP_DIV

%%

// Top-level program
program:
    input { 
        parseTree = $1;
    }
    ;

// Input can be various G++ constructs
input: 
    exp             { $$ = $1; }
    | explist       { $$ = $1; }
    | function_def  { $$ = $1; }
    | list          { $$ = $1; }
    | control_structure { $$ = $1; }
    | load_statement { $$ = $1; }
    | exit_statement { $$ = $1; }
    | print_statement { $$ = $1; }
    | assignment    { $$ = $1; }
    | function_call { $$ = $1; }
    ;

/* Variable Assignment (SET) */
assignment:
    OP_OP KW_SET IDENTIFIER exp OP_CP {
        $$ = createNode("SET", "set");         // Create a SET node
        $$->left = createNode("IDENTIFIER", $3); // Left child is the variable name
        $$->right = $4;                         // Right child is the value/expression
    }
    ;

// Print statement handling
print_statement:
    OP_OP KW_PRINT exp OP_CP {
        $$ = createNode("PRINT", "print");
        $$->left = $3; // Attach the value or expression being printed as a child node
    }
    ;

// Exit statement handling
exit_statement:
    OP_OP KW_EXIT OP_CP { 
        $$ = createNode("EXIT", "exit"); 
    }
    ;

// Load statement
load_statement:
    OP_OP KW_LOAD VALUEF OP_CP {
        $$ = createNode("LOAD", $3); // Create a LOAD node with the filename
    }
    ;

// Expressions with more explicit handling
// Expressions with arithmetic operators
exp: 
    KW_NIL {
        $$ = createNode("NIL", "nil");
    }
    | IDENTIFIER { 
        $$ = createNode("IDENTIFIER", $1); 
    }
    | VALUEF { 
        $$ = createNode("FRACTION", $1); 
    }
    | VALUEI { 
        char buffer[32]; 
        sprintf(buffer, "%d", $1);
        $$ = createNode("INTEGER", buffer); 
    }
    | KW_TRUE { 
        $$ = createNode("BOOLEAN", "true"); 
    }
    | KW_FALSE { 
        $$ = createNode("BOOLEAN", "false"); 
    }
    | OP_OP exp OP_CP { 
        $$ = $2;  // Handle parenthesized expressions, just pass the expression inside
    }
    // Arithmetic operations
    | OP_OP OP_PLUS exp exp OP_CP { 
        $$ = createNode("PLUS", "+");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_MINUS exp exp OP_CP { 
        $$ = createNode("MINUS", "-");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_MULT exp exp OP_CP { 
        $$ = createNode("MULTIPLY", "*");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_DIV exp exp OP_CP { 
        $$ = createNode("DIVIDE", "/");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_AND exp exp OP_CP { 
        $$ = createNode("AND", "and");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_OR exp exp OP_CP { 
        $$ = createNode("OR", "or");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_NOT exp OP_CP { 
        $$ = createNode("NOT", "not");
        $$->left = $3;
    }
    | OP_OP OP_EQUAL exp exp OP_CP { 
        $$ = createNode("EQUAL", "==");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_LESS exp exp OP_CP { 
        $$ = createNode("LESS", "<");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_MORE exp exp OP_CP { 
        $$ = createNode("MORE", ">");
        $$->left = $3;
        $$->right = $4;
    }
    | list { 
        $$ = $1; 
    }
    | function_call { 
        $$ = $1; 
    }
    ;


// Expression List with more explicit chaining
explist: 
    OP_OP explist exp OP_CP {
        $$ = createNode("EXPLIST", "list");
        $$->left = $2;
        $$->right = $3;
    }
    | exp {
        $$ = $1;
    }
    ;

// List handling
list:
    OP_OP KW_LIST values OP_CP { 
        $$ = createNode("LIST", "list");
        $$->left = $3; 
    }
    | OP_OP KW_APPEND list exp OP_CP { 
        $$ = createNode("APPEND", "append");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_CONCAT list list OP_CP { 
        $$ = createNode("CONCAT", "concat");
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_LIST OP_CP { 
        $$ = createNode("LIST", "empty"); // Handle an empty list
    }
    ;

// Control structures
control_structure:
    if_statement { $$ = $1; }
    | OP_OP KW_FOR IDENTIFIER exp exp input OP_CP { 
        $$ = createNode("FOR", "for");
        $$->left = createNode("IDENTIFIER", $3);  // Loop variable
        $$->right = createNode("RANGE", "range");
        $$->right->left = $4;  // Start expression
        $$->right->right = createNode("LOOP_BODY", "body");
        $$->right->right->left = $5;  // End expression
        $$->right->right->right = $6; // Loop body
    }
    ;

// Enhanced IF statement parsing
// Full IF-ELSE structure with condition, true branch, and else branch
if_statement:
    // Full IF-ELSE structure with condition, true branch, and else branch
    OP_OP KW_IF exp input OP_OP KW_ELSE input OP_CP OP_CP { 
        $$ = createNode("IF-ELSE", "if-else");
        $$->left = $3;       // Condition
        $$->right = createNode("BRANCHES", "branches");
        $$->right->left = $4;   // True branch
        $$->right->right = $7;  // False branch (ELSE branch)
    }
    // Traditional IF with condition and true branch
    | OP_OP KW_IF exp input OP_CP { 
        $$ = createNode("IF", "if-single");
        $$->left = $3;  // Condition
        $$->right = createNode("BRANCHES", "branches");
        $$->right->left = $4;  // True branch
        $$->right->right = createNode("NIL", "nil"); // Implicit NIL for false branch
    }
    ;

// Values in a list with explicit chaining
values:
    values OP_COMMA exp { 
        $$ = createNode("VALUES", "multiple");
        $$->left = $1; // Previous values
        $$->right = $3; // Current value
    }
    | exp { 
        $$ = $1; // Single value
    }
    ;

// Identifier list for function parameters
id_list:
    id_list OP_COMMA IDENTIFIER {
        $$ = createNode("ID_LIST", "multiple");
        $$->left = $1;
        $$->right = createNode("IDENTIFIER", $3);
    }
    | IDENTIFIER { 
        $$ = createNode("IDENTIFIER", $1); 
    }
    ;

// Function Definition with more explicit parameter handling
function_def:
    OP_OP KW_DEFFUN IDENTIFIER OP_OP id_list OP_CP input OP_CP {
        $$ = createNode("FUNCTION_DEF", $3);
        $$->left = $5;   // Parameter list
        $$->right = $7;  // Function body
    }
    ;

// Function Call
function_call:
    OP_OP IDENTIFIER explist OP_CP {
        $$ = createNode("FUNCTION_CALL", $2);
        $$->left = $3; // Arguments
    }
    ;

%%

// Function to print the parse tree (simple recursive traversal)
void printParseTree(ASTNode* node, int depth) {
    if (node == NULL) return;

    // Indent based on depth
    for (int i = 0; i < depth; i++) printf("  ");

    // Print node information
    printf("%s: %s\n", node->type, node->value);

    // Recursively print children
    printParseTree(node->left, depth + 1);
    printParseTree(node->right, depth + 1);
}

// Main function for parsing
int main() {
    printf("G++ Syntax Analyzer\n");
    printf("Enter G++ code (Ctrl+D to finish):\n");
    
    if (yyparse() == 0) {
        printf("Syntax is valid.\n");
        printf("Parse Tree:\n");
        printParseTree(parseTree, 0);
    } else {
        printf("Syntax analysis failed.\n");
    }
    
    return 0;
}