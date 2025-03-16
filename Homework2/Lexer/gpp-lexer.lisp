;;;; G++ Lexer implementation in Common Lisp

;;; DFA implementation for identifiers and numbers
(defun letter-p (char)
  (alpha-char-p char))

(defun digit-p (char)
  (digit-char-p char))

(defun identifier-start-p (char)
  (or (letter-p char)
      (char= char #\_)))

(defun identifier-char-p (char)
  (or (letter-p char)
      (digit-p char)
      (char= char #\_)))

(defun read-identifier (stream first-char)
  (let ((result (make-string-output-stream)))
    (write-char first-char result)
    (loop for char = (peek-char nil stream nil nil)
          while (and char (identifier-char-p char))
          do (write-char (read-char stream) result)
          finally (return (get-output-stream-string result)))))

(defun read-number (stream first-char)
  (let ((result (make-string-output-stream)))
    (write-char first-char result)
    (loop for char = (peek-char nil stream nil nil)
          while (and char (digit-p char))
          do (write-char (read-char stream) result)
          finally (return (get-output-stream-string result)))))

;;; Keyword table
(defparameter *keywords*
  '(("and" . "KW_AND")
    ("or" . "KW_OR")
    ("not" . "KW_NOT")
    ("equal" . "KW_EQUAL")
    ("less" . "KW_LESS")
    ("nil" . "KW_NIL")
    ("list" . "KW_LIST")
    ("append" . "KW_APPEND")
    ("concat" . "KW_CONCAT")
    ("set" . "KW_SET")
    ("deffun" . "KW_DEFFUN")
    ("for" . "KW_FOR")
    ("if" . "KW_IF")
    ("exit" . "KW_EXIT")
    ("load" . "KW_LOAD")
    ("print" . "KW_DISP")
    ("true" . "KW_TRUE")
    ("false" . "KW_FALSE")))

;;; Operator table
(defparameter *operators*
  '((#\+ . "OP_PLUS")
    (#\- . "OP_MINUS")
    (#\/ . "OP_DIV")
    (#\* . "OP_MULT")
    (#\( . "OP_OP")
    (#\) . "OP_CP")
    (#\, . "OP_COMMA")))

(defun get-token (lexeme)
  (or (cdr (assoc lexeme *keywords* :test #'string=))
      "IDENTIFIER"))

(defun get-operator (char)
  (cdr (assoc char *operators*)))

(defun process-comment (stream line)
  (read-char stream) ; Read the second semicolon
  (cons "COMMENT" line))

(defun handle-syntax-error (char-or-string)
  (format t "SYNTAX_ERROR: ~A cannot be tokenized~%" char-or-string)
  (throw 'lexer-error nil))

(defun tokenize-line (line)
  (with-input-from-string (stream line)
    (catch 'lexer-error
      (loop with tokens = nil
            for char = (read-char stream nil nil)
            while char
            do (cond
                 ;; Skip whitespace
                 ((or (char= char #\Space)
                      (char= char #\Tab)
                      (char= char #\Newline))
                  nil)
                 
                 ;; Comments
                 ((and (char= char #\;)
                       (eql (peek-char nil stream nil nil) #\;))
                  (push (process-comment stream line) tokens)
                  (return tokens))
                 
                 ;; Operators
                 ((get-operator char)
                  (push (cons (get-operator char) (string char)) tokens))
                 
                 ;; Numbers and Fractions
                 ((digit-p char)
                  (let ((number (read-number stream char)))
                    (let ((next-char (peek-char nil stream nil nil)))
                      (cond
                        ;; Handle fraction format (e.g., 12f12)
                        ((and next-char (or (char-equal next-char #\f) (char-equal next-char #\F)))
                         (read-char stream) ; Skip 'f'
                         (if (and (peek-char nil stream nil nil)
                                 (digit-p (peek-char nil stream nil nil)))
                             (let* ((fraction (read-number stream (read-char stream)))
                                    (after-fraction (peek-char nil stream nil nil)))
                               (if (and after-fraction 
                                       (or (letter-p after-fraction) 
                                           (char= after-fraction #\_)))
                                   (handle-syntax-error 
                                    (format nil "~A~A~A~A" 
                                            number "f" fraction 
                                            (read-identifier stream after-fraction)))
                                   (push (cons "VALUEF" 
                                             (format nil "~Af~A" number fraction)) 
                                         tokens)))
                             (handle-syntax-error (format nil "~Af" number))))
                        
                        ;; Handle invalid identifiers starting with numbers
                        ((and next-char 
                              (or (letter-p next-char) 
                                  (char= next-char #\_)))
                         (handle-syntax-error 
                          (format nil "~A~A" 
                                  number 
                                  (read-identifier stream next-char))))
                        
                        ;; Regular integer
                        (t (push (cons "VALUEI" number) tokens))))))
                 
                 ;; Identifiers (now allowing underscore as first character)
                 ((identifier-start-p char)
                  (let* ((lexeme (read-identifier stream char))
                         (token-type (get-token lexeme)))
                    (push (cons token-type lexeme) tokens)))
                 
                 ;; Invalid characters
                 (t
                  (handle-syntax-error (string char))))
            finally (return (nreverse tokens))))))

(defun gppinterpreter (&optional filename)
  (if filename
      ;; Process file
      (with-open-file (stream filename :if-does-not-exist nil)
        (if stream
            (loop for line = (read-line stream nil nil)
                  while line
                  do (let ((tokens (tokenize-line line)))
                       (when tokens
                         (dolist (token tokens)
                           (format t "~A: ~A~%" (car token) (cdr token))))))
            (format t "Error: Could not open file ~A~%" filename)))
      ;; Interactive mode
      (loop
        (format t "> ")
        (force-output)
        (let* ((line (read-line nil nil))
               (tokens (and line (tokenize-line line))))
          (when (or (null line) (string= line "(exit)"))
            (return-from gppinterpreter nil))
          (when tokens
            (dolist (token tokens)
              (format t "~A: ~A~%" (car token) (cdr token))))))))