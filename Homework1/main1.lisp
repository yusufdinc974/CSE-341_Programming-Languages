(defun string-starts-with (str prefix)
  "Check if STR starts with PREFIX."
  (and (>= (length str) (length prefix))
       (string= (subseq str 0 (length prefix)) prefix)))

(defun is-alphabet-char (char)
  "Check if CHAR is an alphabet character (A-Z, a-z)."
  (or (and (char>= char #\A) (char<= char #\Z))
      (and (char>= char #\a) (char<= char #\z))
      (char= char #\_)))  ;; Also allow underscore

(defun split-string (string delimiter)
  "Splits STRING into a list of substrings separated by DELIMITER."
  (labels ((split-helper (string start)
             (let ((end (search delimiter string :start2 start)))
               (if end
                   (cons (subseq string start end)
                         (split-helper string (+ end (length delimiter))))
                   (list (subseq string start))))))
    (split-helper string 0)))


(defun string-join (string-list &optional (delimiter " "))
  "Joins a list of strings STRING-LIST into a single string, separated by DELIMITER."
  (reduce (lambda (a b) (concatenate 'string a delimiter b))
          string-list))


(defun convert-value (value)
  "Converts C values to their Lisp equivalent.
   Handles variables, numbers, strings, and other expressions."
  (let ((trimmed-value (string-trim " " value)))
    (format t "Debugging: Checking value: '~a'~%" trimmed-value) ;; Debugging line
    (cond
      ;; Check if the value is a variable (starts with a letter or underscore)
      ((and (not (string= trimmed-value ""))
            (is-alphabet-char (aref trimmed-value 0)))
       (progn
         (format t "Variable detected: ~a~%" trimmed-value)
         trimmed-value))

      ;; Check if the value is an integer (any sequence of digits)
      ((and (every #'digit-char-p trimmed-value)
            (> (length trimmed-value) 0))
       (progn
         (format t "Integer detected: ~a~%" trimmed-value)
         trimmed-value))

      ;; Check if the value is a floating-point number
      ((and (find #\. trimmed-value :test 'char=)
            (let ((parts (split-string trimmed-value ".")))
              (and (= (length parts) 2)
                   (every #'digit-char-p (first parts))
                   (every #'digit-char-p (second parts)))))
       (progn
         (format t "Floating-point detected: ~a~%" trimmed-value)
         trimmed-value))

      ;; Check if the value is a string literal
      ((and (>= (length trimmed-value) 2)
            (string= (subseq trimmed-value 0 1) "\"")
            (string= (subseq trimmed-value (- (length trimmed-value) 1)) "\""))
       (progn
         (format t "String detected: ~a~%" trimmed-value)
         (subseq trimmed-value 1 (- (length trimmed-value) 1))))  ;; Remove quotes

      ;; Check for boolean values or expressions like true, false
      ((string= (string-downcase trimmed-value) "true")
       (progn
         (format t "Boolean 'true' detected, converting to T~%")
         t))  ;; Convert C-style true to Lisp T
      ((string= (string-downcase trimmed-value) "false")
       (progn
         (format t "Boolean 'false' detected, converting to NIL~%")
         nil))  ;; Convert C-style false to Lisp NIL

      ;; Handle other cases as a fallback
      (t 
       (format t "~a" trimmed-value)))))

(defun test-convert-value ()
  (let ((test-values '("x" "0" "5.3" "\"Hello\"" "true" "false" "42" "var_name")))
    (dolist (value test-values)
      (let ((converted (convert-value value)))
        (format t "C Value: ~a -> Lisp Value: ~a~%"
                value
                (cond ((eq converted t) 'T)   ;; Convert Lisp boolean symbols
                      ((eq converted nil) 'NIL)
                      (t converted)))))))

(defun convert-variable-assignment (line)
  "Convert a C variable assignment line to Lisp format.
   Examples: 
   'x = 5;' becomes '(setf x 5)' and 
   'int x = 5;' becomes '(let ((x 5)))'."
  (let* ((trimmed-line (string-trim " " line))
         (equal-pos (position #\= trimmed-line))
         (semicolon-pos (position #\; trimmed-line))
         ;; Define a list of recognized data types for declarations
         (data-types '("int" "float" "char" "double" "long" "short" "bool")))
    (if (and equal-pos semicolon-pos)
        (let* ((before-equal (string-trim " " (subseq trimmed-line 0 equal-pos)))
               (after-equal (string-trim " " (subseq trimmed-line (1+ equal-pos) semicolon-pos)))
               (type-and-var (split-string before-equal " ")))
          ;; Check if the first word is a data type for a declaration
          (if (and (= (length type-and-var) 2)
                   (member (first type-and-var) data-types :test #'string=))
              ;; If declared with a recognized type, use let
              (let ((var-name (second type-and-var))
                    (value after-equal))
                (format nil "(let ((~a ~a)))" var-name value))
              ;; Otherwise, use setf for simple assignment
              (let ((var-name before-equal)
                    (value after-equal))
                (format nil "(setf ~a ~a)" var-name value))))
        (error "Invalid variable assignment: ~a" line))))

(defun convert-variable-declaration (line)
  "Convert a C variable declaration line to Lisp format.
   Example: 'int x;' becomes '(let ((x)))'."
  (let* ((trimmed-line (string-trim " " line))
         (semicolon-pos (position #\; trimmed-line)))
    (if semicolon-pos
        (let* ((parts (split-string (subseq trimmed-line 0 semicolon-pos) " "))
               (var-name (car (last parts))))  ; Get the variable name, which is the last element
          ;; Assuming we are declaring the variable without an initial value
          (format nil "(let ((~a)))" var-name))
        (error "Invalid variable declaration: ~a" line))))

(defun replace-regexp-in-string (pattern replacement string)
  "Replace all occurrences of PATTERN in STRING with REPLACEMENT."
  (labels ((replace-helper (string start)
             (let ((pos (search pattern string :start2 start)))
               (if pos
                   (concatenate 'string
                                (subseq string 0 pos)
                                replacement
                                (replace-helper (subseq string (+ pos (length pattern))) 0))
                   string))))
    (replace-helper string 0)))



(defun validate-declaim-string (declaim-string)
  "Validate and correct the declaim string by removing any extraneous characters."
  (let ((semicolon-pos (position #\; declaim-string)))
    (if (and semicolon-pos
             (char= (char declaim-string (1+ semicolon-pos)) #\)))
        ;; Remove the semicolon if followed by a closing parenthesis
        (concatenate 'string (subseq declaim-string 0 semicolon-pos)
                     (subseq declaim-string (1+ semicolon-pos)))
        ;; If no problem, return the original string
        declaim-string)))

(defun convert-function-definition (line)
  "Convert a C function definition line to Lisp format.
   Example: 'int add(int a, int b) {' becomes '(defun add (a b) ...)'"
  (let* ((trimmed-line (string-trim " " line))
         (open-brace-pos (position #\{ trimmed-line))
         ;; Find the function name
         (function-name-start (1+ (position #\  trimmed-line)))
         (function-name (subseq trimmed-line function-name-start
                                (position #\( trimmed-line :start function-name-start)))
         (params-string (if open-brace-pos
                            (subseq trimmed-line (1+ (position #\( trimmed-line)) open-brace-pos)
                            (subseq trimmed-line (1+ (position #\( trimmed-line)))))
         ;; Map C types to Lisp equivalents
         (type-mapping '(("int" . "integer")
                         ("float" . "float")
                         ("double" . "double")
                         ("char" . "character")
                         ("void" . "nil")
                         ("char*" . "string")))
         ;; Split parameters and identify types and names
         (params (split-string params-string ","))
         (param-types (mapcar (lambda (p)
                                (let* ((param-parts (split-string (string-trim " " p) " "))
                                       (param-type (first param-parts)))
                                  (or (cdr (assoc param-type type-mapping :test #'string=)) param-type)))
                              params))
         ;; Clean up parameter names for function definition
         (cleaned-params (mapcar (lambda (p)
                                   (let ((param-parts (split-string (string-trim " " p) " ")))
                                     (second param-parts)))  ;; Keep only parameter names
                                 params))
         ;; Convert return type using the type mapping
         (return-type (first (split-string trimmed-line " ")))
         (lisp-return-type (or (cdr (assoc return-type type-mapping :test #'string=)) return-type)))
    (if open-brace-pos
        ;; If function has a body THİS CAN BE CHANGED )
        (format nil "(defun ~a (~{~a~^ ~} " function-name cleaned-params)
        ;; If function is a declaration
        (let ((declaim-string (format nil "(declaim (ftype (function (~{~a~^ ~}) ~a) ~a))"
                                      (if (null params-string) '() param-types)
                                      lisp-return-type
                                      function-name)))
          (validate-declaim-string declaim-string)))))

(defun convert-return-statement (line)
  "Convert a C return statement to Lisp format.
   Example: 'return 5;' becomes '(return 5)'."
  (let* ((trimmed-line (string-trim " " line))
         (return-pos (position #\r trimmed-line :test #'char=)))
    (if (and return-pos (string= (subseq trimmed-line return-pos (+ return-pos 6)) "return"))
        (let* ((value (string-trim " " (subseq trimmed-line (+ return-pos 6) (position #\; trimmed-line)))))
          ;; Return the value in Lisp format
          (format nil "(return-from ~a)" value))
        (error "Invalid return statement: ~a" line))))

(defun replace-specifiers (format-string)
  "Replace C-style format specifiers with Lisp equivalents."
  (let ((specifiers '(("%d" "~d")
                      ("%f" "~f")
                      ("%s" "~a"))))
    (dolist (spec specifiers format-string)
      (let ((search (car spec))
            (replace (cadr spec)))
        ;; Use string replace
        (setf format-string (replace-regexp-in-string search replace format-string))))))

(defun convert-print-statement (line)
  "Convert a C printf statement to a Lisp format statement.
   Example: 'printf(\"Hello, %s!\", name);' becomes '(format t \"Hello, ~a!\" name)'"
  (let* ((trimmed-line (string-trim " " line))
         (printf-pos (position #\p trimmed-line :start 0))
         (format-string-start (and printf-pos 
                                    (position #\" trimmed-line :start (1+ printf-pos))))
         (format-string-end (and format-string-start 
                                  (position #\" trimmed-line :start (1+ format-string-start))))
         (format-string (if (and format-string-start format-string-end)
                            (subseq trimmed-line format-string-start (1+ format-string-end))
                            nil))
         (args-start (if format-string-end
                         (1+ format-string-end)
                         nil))
         ;; Extract arguments
         (args (if args-start
                   (let ((arg-string (subseq trimmed-line args-start (length trimmed-line))))
                     (split-string arg-string ",")) 
                   nil)))
    ;; Convert format specifiers to Lisp format
    (when format-string
      (let* ((lisp-format-string (replace-specifiers (string-trim "\"" format-string)))
             (cleaned-args (mapcar (lambda (arg) (string-trim " " arg)) args)))
        ;; Construct the final Lisp statement without `);`
        (format nil "(format t ~a ~{~a~^ ~})" 
                (format nil "\"~a\"" lisp-format-string) ;; Add quotation marks around the format string
                cleaned-args)))))

(defun string-prefix? (prefix str)
  "Check if PREFIX is the prefix of STR."
  (and (>= (length str) (length prefix))
       (string= (subseq str 0 (length prefix)) prefix)))

(defun convert-if-statement (line)
  "Convert a C if statement to Lisp format."
  (let* ((trimmed-line (string-trim " " line))
         (if-pos (position #\i trimmed-line :test #'char=))
         (open-paren-pos (position #\( trimmed-line :start (+ if-pos 2)))
         (close-paren-pos (position #\) trimmed-line :start (1+ open-paren-pos)))
         (open-brace-pos (position #\{ trimmed-line :start (1+ close-paren-pos)))
         (close-brace-pos (position #\} trimmed-line :start (1+ open-brace-pos)))
         (condition (subseq trimmed-line (1+ open-paren-pos) close-paren-pos)))
    
    ;; Find the body of the if statement between the braces
    (when open-brace-pos
      (let ((body (subseq trimmed-line (1+ open-brace-pos) close-brace-pos)))
        ;; Split the body into statements and trim each
        (let ((statements (remove "" (mapcar (lambda (s) (string-trim " " s))
                                              (split-string body ";")))))
          ;; Create the if statement with a formatted output
          (format nil "(if ~a (progn~%  ~{~a~^~%  ~})"
                  (replace-regexp-in-string "^" "(" (string-trim " " condition))
                  (mapcar (lambda (s) (format nil "    ~a" s)) statements)))))))


(defun convert-for-loop (line)
  "Convert a C for loop statement to Lisp format.
   Example: 'for (int i = 0; i < 10; i++) {' becomes '(loop for i from 0 below 10 do ...)'"
  (let* ((trimmed-line (string-trim " " line))
         (for-pos (position #\f trimmed-line :test #'char=))
         (open-paren-pos (position #\( trimmed-line :start (+ for-pos 2)))
         (close-paren-pos (position #\) trimmed-line :start (1+ open-paren-pos)))
         (open-brace-pos (position #\{ trimmed-line :start (1+ close-paren-pos)))
         (close-brace-pos (position #\} trimmed-line :start (1+ open-brace-pos)))
         (init-decl (subseq trimmed-line (1+ open-paren-pos) (position #\; trimmed-line)))
         (condition (subseq trimmed-line (1+ (position #\; trimmed-line)) (position #\; trimmed-line :start (1+ (position #\; trimmed-line)))))
         (increment (subseq trimmed-line (1+ (position #\; trimmed-line :start (1+ (position #\; trimmed-line)))) close-paren-pos))
         (body (when open-brace-pos
                 (subseq trimmed-line (1+ open-brace-pos) close-brace-pos)))
         (body-statements (when body
                            (remove "" (mapcar (lambda (s) (string-trim " " s))
                                               (split-string body ";")))))
         (loop-var (second (split-string (string-trim " " init-decl) "=")))
         (loop-start (second (split-string (string-trim " " init-decl) "=")))
         (loop-range (subseq (string-trim " " condition) 0 (position #\< (string-trim " " condition)))))

    ;; Constructing the final Lisp loop statement
    (format nil "(loop for ~a from ~a below ~a do~%  ~{~a~^~%  ~})"
            (string-trim " " loop-var)  ; The loop variable
            loop-start                    ; The initial value
            (string-trim " " (second (split-string (string-trim " " condition) "<")))  ; Limit value
            (mapcar (lambda (s) (format nil "    ~a" s)) body-statements))))  ; Body statements


(defun convert-while-loop (line)
  "Convert a C while loop statement to Lisp format.
   Example: 'while (i < 10) {' becomes '(loop while (< i 10) do ...)'"
  (let* ((trimmed-line (string-trim " " line))
         (while-pos (position #\w trimmed-line :test #'char=))
         (open-paren-pos (position #\( trimmed-line :start (+ while-pos 4)))
         (close-paren-pos (position #\) trimmed-line :start (1+ open-paren-pos)))
         (open-brace-pos (position #\{ trimmed-line :start (1+ close-paren-pos)))
         (close-brace-pos (position #\} trimmed-line :start (1+ open-brace-pos)))
         (condition (subseq trimmed-line (1+ open-paren-pos) close-paren-pos)))
    
    ;; Find the body of the while statement between the braces
    (when open-brace-pos
      (let ((body (subseq trimmed-line (1+ open-brace-pos) close-brace-pos)))
        ;; Split the body into statements and trim each
        (let ((statements (remove "" (mapcar (lambda (s) (string-trim " " s))
                                              (split-string body ";")))))
          ;; Create the while statement with a formatted output
          (format nil "(loop while ~a do~%  ~{~a~^~%  ~})"
                  (replace-regexp-in-string "^" "(" (string-trim " " condition))
                  (mapcar (lambda (s) (format nil "    ~a" s)) statements)))))))



(defun line-type (line)
  "Determine the type of a given line of C code.
   Returns a keyword indicating the type."
  (let ((trimmed-line (string-trim " " line)))  ; Trim spaces from the line
    ;(format t "Checking line type for: ~a~%" trimmed-line)  ; Debug output
    (cond
      ((string= line "") 'blank)  ; Handle blank lines
      ((string= trimmed-line "{") 'open-brace)  ; Handle opening brace
      ((and (string= trimmed-line "}") 
            (= (length trimmed-line) 1))  ; Handle closing brace
       :close-brace)
      ((string-prefix? "if" trimmed-line) :if-statement)  ; Handle if statement
      ((string-prefix? "for" trimmed-line) :for-loop)  ; Check for for-loop
      ;; Check for function definitions
      ((and (or (string-prefix? "int" trimmed-line)
                (string-prefix? "void" trimmed-line)
                (string-prefix? "float" trimmed-line)
                (string-prefix? "double" trimmed-line)
                (string-prefix? "char" trimmed-line))
            (search "(" trimmed-line)
            (search "{" trimmed-line))  ;; Check for function body
       :function-definition)
      ;; Check for function declarations (no body)
      ((and (or (string-prefix? "int" trimmed-line)
                (string-prefix? "void" trimmed-line)
                (string-prefix? "float" trimmed-line)
                (string-prefix? "double" trimmed-line)
                (string-prefix? "char" trimmed-line))
            (search "(" trimmed-line)
            (not (search "{" trimmed-line)))  ;; Ensure there's no body
       :function-declaration)
      ;; Check for variable declarations
      ((and (or (string-prefix? "int" trimmed-line)
                (string-prefix? "void" trimmed-line)
                (string-prefix? "float" trimmed-line)
                (string-prefix? "double" trimmed-line)
                (string-prefix? "char" trimmed-line)
                (string-prefix? "long" trimmed-line)
                (string-prefix? "short" trimmed-line))
            (search ";" trimmed-line))
       :variable-declaration)
      ;; Check for assignment statements
      ((and (search "=" trimmed-line) (search ";" trimmed-line))
       :variable-assignment)
      ;; Check for printf statements
      ((search "printf" trimmed-line)
       :print-statement)
      ;; Check for return statements
      ((and (search "return" trimmed-line) (search ";" trimmed-line))
       :return-statement)
      ;; Default case if no types match
      (t :unknown))))




;; Placeholder for convert-unknown function
(defun convert-unknown (line)
  "Handle unknown line types."
  (format nil "~a" line))


(defun convert-curly-brace (line)
  "Convert a closing curly brace '}' to a closing parenthesis ')'."
  (if (string= (string-trim " " line) "}")
      ")"
      (format nil "elma ~a" line)))  ; Return the line unchanged if it’s not a close brace


(defun conversion-foo (line)
  "Select and apply the appropriate conversion function based on the line type."
  ;(format t "Reading line: ~a~%" line)
  (let ((type (line-type line)))
    (case type
      (:close-brace (convert-curly-brace line))  ; Explicitly return ")"
      (:function-definition (convert-function-definition line))
      (:function-declaration (convert-function-definition line))
      (:variable-declaration (convert-variable-declaration line))
      (:variable-assignment (convert-variable-assignment line))
      (:print-statement (convert-print-statement line))
      (:return-statement (convert-return-statement line))
      (:if-statement (convert-if-statement line))
      (:for-loop (convert-for-loop line))  ; Add for-loop case
      (:unknown (convert-unknown line)))))



(defun string-blank-p (str)
  "Returns T if STR is empty or contains only whitespace."
  (every #'(lambda (char) (char= char #\Space)) str))


(defun convert (input-file output-file)
  "Converts each line of input-file from C to Lisp and writes to output-file."
  (with-open-file (in input-file :direction :input)
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      (labels ((process-lines ()
                 (let ((line (read-line in nil)))
                   (when line
                     (unless (string-blank-p line) ;; skip empty lines
                       (let ((converted (conversion-foo line)))
                         (format out "~a~%" converted)))
                     (process-lines))))) ;; Recursive call
        (process-lines))))) ;; Start processing lines


