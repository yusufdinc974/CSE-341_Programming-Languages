# 📌 CSE341 – Programming Languages  
## 📝 Homework 4  

### 🖥️ **Topics Covered:** Common Lisp, Logic Programming, Prolog Implementation, Theorem Proving  

---

## 📌 **Assignment Overview**  
You will implement a **simplified Prolog theorem prover** in Common Lisp.  
Your function will:  
✅ **Define axioms using Horn clauses.**  
✅ **Perform unification and resolution for theorem proving.**  
✅ **Traverse the search space using depth-first left-to-right strategy.**  
✅ **Return substitutions if a proof is found or nil if the query fails.**  

🚨 **Prolog-Like System Details:**  
- **Uses Horn clauses (simplified first-order predicate calculus).**  
- **Handles basic rule-based logical inferences.**  
- **Supports queries with at most one variable.**  

---

## 📂 **Implementation Details**  
### **🔹 Function to Implement: `prolog_prove`**  
```lisp
(defun prolog_prove (axioms query)
```
- **Arguments:**  
  - `axioms`: A list of Horn clauses representing facts and rules.  
  - `query`: A list of predicates to prove.  
- **Output:**  
  - If proof exists → Returns **substitutions list** (e.g., `(("X" "parent"))`).  
  - If proof fails → Returns **nil**.  

---

## 📌 **Horn Clause Representation**  
- **Facts** (single predicates in a list):  
  ```lisp
  (("father" "jim" "jill"))  
  ```
- **Rules** (head predicate, `" < "`, and conditions):  
  ```lisp
  (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))  
  ```

---

## 🎯 **Example Execution**  
### **🔹 Sample Axioms & Queries**
```lisp
(let ( 
        (axioms '(  
                    ( ("father" "jim" "jill") )  
                    ( ("mother" "mary" "jill") )  
                    ( ("father" "samm" "jim") )  
                    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )  
                    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )  
                    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )  
                    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )  

        (query1 '( ("ancestor" "X" "jill") ) )  
        (query2 '( ("ancestor" "X" "jill") ("mother" "X" "bob") ) ) )  

    (prolog_prove axioms query1)  ; Returns: (("X" "parent"))  
    (prolog_prove axioms query2)  ; Returns: NIL  
)
```

---

## 📌 **Implementation Requirements**  
✅ **Use proper unification and resolution techniques.**  
✅ **Perform depth-first traversal of the knowledge base.**  
✅ **Implement a clean recursive approach without side effects.**  
✅ **Ensure the output follows correct Prolog-style responses.**  

---

🚀 **Good luck!** Happy coding! 🎯  
