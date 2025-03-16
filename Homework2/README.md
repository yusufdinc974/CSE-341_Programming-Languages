# 📌 CSE341 – Programming Languages  
## 📝 Homework 2  

### 🖥️ **Topics Covered:** Common Lisp, Lexical Analysis, Finite Automata, Flex  

---

## 📌 **Assignment Overview**  
You will implement a **G++ language lexer** that tokenizes valid expressions and statements based on the **G++ lexical syntax**.  
Your lexer must:  
✅ **Identify keywords, operators, identifiers, and literals.**  
✅ **Detect lexical errors and generate appropriate messages.**  
✅ **Be implemented in two different ways:**  
  - **Using Flex (35 points)**  
  - **Using Common Lisp (65 points)**  

🚨 **G++ Language Details:**  
- **Lisp-like syntax**, interpreted, static scope, and strongly typed.  
- **Keywords:** `and`, `or`, `not`, `equal`, `list`, `append`, `if`, `for`, `print`, etc.  
- **Operators:** `+`, `-`, `/`, `*`, `(`, `)`, `,`  
- **Literals:** **Unsigned integers & fractions (e.g., `123:12`)**  
- **Identifiers:** **Alphabetical characters, digits, and `_` (must start with a letter).**  
- **Comments:** **Lines starting with `;;`**  

---

## 📂 **Implementation Details**  
### **🔹 Part 1: G++ Lexer Using Flex (35 points)**  
- Implement a **Flex-based lexer** (`gpp_lexer.l`).  
- Compile to **C code (`gpp_lexer.c`)** using `flex`.  
- Lexer must:
  ✅ **Recognize keywords, identifiers, operators, and literals.**  
  ✅ **Detect lexical errors (`SYNTAX_ERROR`).**  
  ✅ **Tokenize a user-input G++ program line-by-line.**  

📌 **Example Tokens**  
```lisp
(deffun sumup (x)  
  (if (equal x 0)  
      1  
      (+ x (sumup (- x 1)))  
  )  
)
```
🔽 **Flex Output**
```
KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP  
OP_OP KW_IF OP_OP KW_EQUAL IDENTIFIER VALUEI OP_CP  
VALUEI  
OP_OP OP_PLUS IDENTIFIER OP_OP IDENTIFIER OP_OP IDENTIFIER VALUEI OP_CP OP_CP OP_CP  
```

---

### **🔹 Part 2: G++ Lexer in Common Lisp (65 points)**  
- Implement a **Lisp-based lexer** (`gpp_lexer.lisp`).  
- Must contain a function **`gppinterpreter`** that:  
  ✅ **Starts a REPL (Read-Eval-Print Loop)** for interactive tokenization.  
  ✅ **Accepts an optional input file argument.**  
  ✅ **Implements a recursive lexical analyzer (DFA or regex-based).**  
  ✅ **Handles all G++ token types.**  

📌 **Example Execution**  
```
> (gppinterpreter "hello.g++")  
KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP  
```
🚨 **Restrictions:**  
- **No built-in regex libraries allowed in Lisp.**  
- **Manually implement token recognition using recursion.**  

---

## 📌 **Implementation Requirements**  
✅ **Use Flex for Part 1 and Common Lisp for Part 2.**  
✅ **Correct DFA or regex-based lexical analysis.**  
✅ **Error messages for invalid tokens.**  
✅ **Interactive user input handling (REPL).**  

---

🚀 **Good luck!** Happy coding! 🎯  
