;; Define global constants
(defparameter *keywords* '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat"
                           "set" "deffun" "for" "if" "exit" "load" "print" "true" "false"))
(defparameter *operators* '("+" "-" "*" "/" "(" ")" ","))

;; Tokenizer
(defun tokenize (input)
  "Tokenizes the input string into meaningful G++ tokens."
  (let ((tokens (remove-if #'string= 
                           (split-sequence:split-sequence-if #'char-whitespace-p input))))
    (loop for token in tokens
          collect
          (cond
            ((member token *keywords*) `(:keyword ,token))
            ((member token *operators*) `(:operator ,token))
            ((string-match-p "^[0-9]+(:[0-9]+)?$" token) `(:literal ,token)) ;; Fractional or integer literals
            ((string-match-p "^[a-zA-Z_][a-zA-Z0-9_]*$" token) `(:identifier ,token)) ;; Valid identifier
            (t (error "Unrecognized token: ~a" token))))))

;; Parsing Expressions ($EXP)
(defun parse-exp (tokens)
  "Parse $EXP from the token list and return the parse tree."
  (match-case tokens
    ;; Match (operator expression expression)
    (((:operator "(") (:operator "+") rest)
     (let* ((left-exp (parse-exp rest))
            (right-exp (parse-exp (cdr left-exp))))
       `(:exp :add ,(car left-exp) ,(car right-exp))))
    (((:operator "(") (:operator "-") rest)
     (let* ((left-exp (parse-exp rest))
            (right-exp (parse-exp (cdr left-exp))))
       `(:exp :sub ,(car left-exp) ,(car right-exp))))
    (((:operator "(") (:operator "*") rest)
     (let* ((left-exp (parse-exp rest))
            (right-exp (parse-exp (cdr left-exp))))
       `(:exp :mul ,(car left-exp) ,(car right-exp))))
    (((:operator "(") (:operator "/") rest)
     (let* ((left-exp (parse-exp rest))
            (right-exp (parse-exp (cdr left-exp))))
       `(:exp :div ,(car left-exp) ,(car right-exp))))
    ;; Match identifier or literal
    (((:identifier id) . rest) `(:identifier ,id . ,rest))
    (((:literal val) . rest) `(:literal ,val . ,rest))
    ;; Unrecognized
    (t (error "Syntax error in expression: ~a" tokens))))

;; Parsing Lists ($LIST)
(defun parse-list (tokens)
  "Parse a G++ list of values."
  (match-case tokens
    (((:operator "'") (:operator "(") (:operator ")") rest) `(:list nil . ,rest)) ;; '()
    (((:operator "'") (:operator "(") rest)
     (let ((values (parse-values rest)))
       (if (equal (car values) '(:operator ")"))
           `(:list ,(cdr values))
           (error "Expected closing parenthesis."))))
    (t (error "Invalid list syntax."))))

;; Parse Input ($INPUT)
(defun parse-start (tokens)
  "Parse $START from token list. Returns the parse tree and remaining tokens."
  (cond
    ;; $INPUT -> $EXP
    ((valid-exp? tokens) (parse-exp tokens))
    ;; $INPUT -> $EXPLIST
    ((valid-explist? tokens) (parse-explist tokens))
    ;; Invalid input
    (t (error "Invalid input: ~a" tokens))))

;; Valid Expression Check
(defun valid-exp? (tokens)
  "Check if tokens start with a valid $EXP."
  (or (and (consp tokens) (equal (car tokens) '(:operator "(")))
      (and (consp tokens) (equal (car tokens) '(:identifier)))
      (and (consp tokens) (equal (car tokens) '(:literal)))))

;; Valid Expression List Check
(defun valid-explist? (tokens)
  "Check if tokens start with a valid $EXPLIST."
  (and (consp tokens) (equal (car tokens) '(:operator "("))))

;; Parsing Expression Lists ($EXPLIST)
(defun parse-explist (tokens)
  "Parse $EXPLIST."
  (if (and tokens (equal (car tokens) '(:operator "(")))
      (let ((subtree (parse-exp (cdr tokens))))
        (if (and subtree (equal (car subtree) '(:operator ")")))
            `(:explist ,(cdr subtree))
            (error "Missing closing parenthesis for EXPLIST.")))
      (error "Invalid EXPLIST syntax.")))

;; REPL Function
(defun gpp-repl ()
  "Start the REPL for G++."
  (loop
    (format t "> ")
    (let ((input (read-line)))
      (unless (string= input "exit")
        (let* ((tokens (tokenize input))
               (parse-tree (parse-start tokens)))
          (print parse-tree))
        (gpp-repl)))))

;; Interpreter Main Function
(defun gppinterpreter (&optional (filename nil))
  "Load a G++ file or start the REPL."
  (if filename
      (with-open-file (stream filename)
        (loop for line = (read-line stream nil nil)
              while line
              do (let* ((tokens (tokenize line))
                        (parse-tree (parse-start tokens)))
                   (print parse-tree))))
      (gpp-repl)))
