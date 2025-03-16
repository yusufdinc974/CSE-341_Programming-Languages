# 📌 CSE341 – Programming Languages  
## 📝 Homework 3  

### 🖥️ **Topics Covered:** Syntax Analysis, Context-Free Grammars, Yacc, Recursive Parsing  

---

## 📌 **Assignment Overview**  
You will implement a **G++ Syntax Analyzer** that determines whether a given program conforms to the **G++ syntax rules**.  
Your implementation must be done in **two different ways**:  
✅ **Using Yacc (35 points)**  
✅ **Using Common Lisp (65 points)**  

🚨 **G++ Language Overview:**  
- **Lisp-like syntax**, interpreted, imperative.  
- **Supports basic arithmetic operations, function definitions, control structures, and lists.**  
- **Static scope, static binding, strongly typed.**  

📌 **Refer to the provided [G++ Concrete Syntax](G__Syntax%20-%20Concrete.pdf) document for full syntax details.**  

---

## 📂 **Implementation Details**  
### **🔹 Part 1: G++ Syntax Analyzer Using Yacc (35 points)**  
- Implement a **Yacc-based syntax analyzer** (`gpp_interpreter.y`).  
- Use your **lexer from Homework 2** (`gpp_lexer.l`).  
- Must:  
  ✅ **Detect syntax errors** and provide informative messages.  
  ✅ **Generate a parse tree for expressions and if-statements.**  
  ✅ **Properly implement CFG for expressions & control structures.**  

📌 **Example Syntax Check (Yacc Output)**  
```
> (deffun sumup (x)
  (if (equal x 0)
      1
      (+ x (sumup (- x 1)))))
Syntax OK.
```

---

### **🔹 Part 2: G++ Syntax Analyzer in Common Lisp (65 points)**  
- Implement a **recursive descent parser** (`gpp_interpreter.lisp`).  
- Must:  
  ✅ **Start a REPL (Read-Eval-Print Loop) for syntax checking.**  
  ✅ **Accept an optional file input.**  
  ✅ **Validate G++ syntax recursively.**  
  ✅ **Generate a parse tree for expressions and control structures.**  

📌 **Example Execution**  
```
> (gppinterpreter "test.g++")
Syntax OK.
```

🚨 **Restrictions:**  
- **No external parsing libraries allowed in Lisp.**  
- **Proper CFG parsing algorithm must be implemented (20-point deduction otherwise).**  

---

## 📌 **Implementation Requirements**  
✅ **Use Yacc for Part 1 and Common Lisp for Part 2.**  
✅ **Proper context-free grammar parsing.**  
✅ **Error messages for invalid syntax.**  
✅ **Interactive REPL mode for testing.**  

---

🚀 **Good luck!** Happy coding! 🎯  
