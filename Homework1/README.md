# 📌 CSE341 – Programming Languages  
## 📝 Assignment 1  

### 🖥️ **Topics Covered:** Common Lisp, Functional Programming, Recursion, File Processing  

---

## 📌 **Assignment Overview**  
You will implement a **functional recursive program** in Lisp that:  
✅ **Reads a C file line by line**.  
✅ **Determines the type of each line**.  
✅ **Converts C syntax to Lisp syntax recursively**.  
✅ **Writes the fully converted Lisp code to an output file**.  

🚨 **Important Requirements:**  
- **Strict functional programming principles** (No mutable assignments).  
- **Recursive processing** (Each line is handled recursively).  
- **Handles various C constructs** including loops, conditionals, and function calls.  

---

## 📂 **Functions to Implement**  
### **🔹 `line-type`**  
- **Input:** A line of C code.  
- **Output:** Returns the **type** of the line (`if-statement`, `loop`, `function-call`, etc.).  

### **🔹 `conversion-foo`**  
- **Input:** A line type.  
- **Output:** Returns the **corresponding conversion function**.  

### **🔹 `convert`**  
- **Input:** A C line and its corresponding conversion function.  
- **Output:** Returns the **converted Lisp equivalent**.  

### **🔹 Conversion Functions**  
- **Specific conversion functions for:**  
  ✅ `if` statements → `(if condition then-expression else-expression)`  
  ✅ `for` loops → `(loop for i from start to end do ...)`  
  ✅ `while` loops → `(loop while condition do ...)`  
  ✅ `function definitions` → `(defun name (args) body)`  
  ✅ `variable assignments` → `(setq var value)`  

### **🔹 `read_file`**  
- Reads the **input C file line by line**.  
- Can be an **iterator** returning **one line per call** or **indexed retrieval**.  

### **🔹 `write_file`**  
- Writes the **fully converted Lisp code** to an **output file**.  

---

## 📌 **Recursive Conversion Process**  
1️⃣ **Determine the type** of the current line (`line-type`).  
2️⃣ **Select the correct conversion function** (`conversion-foo`).  
3️⃣ **Convert the line** (`convert`).  
4️⃣ **Process the next line recursively**.  
5️⃣ **Combine results** and write to output.  

📌 **Example Conversion:**  
```c
// C Code
for (int i = 0; i < 10; i++) {
    printf("%d", i);
}
```
🔽  
```lisp
(loop for i from 0 to 9 do
    (print i))
```

---

## 📌 **Implementation Requirements**  
✅ **No mutable variables** (Strict functional style).  
✅ **No imperative loops** (Only recursion).  
✅ **Use external libraries** **only** for file I/O.  
✅ **Output must be valid Lisp syntax**.  

---

🚀 **Good luck!** Happy coding! 🎯  
