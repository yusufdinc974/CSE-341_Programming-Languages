;; Basic helper functions
(defun variable-p (str)
  (and (stringp str)
       (char<= #\A (char str 0) #\Z)))

(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (flatten (car lst))
                   (flatten (cdr lst))))))

;; Variable lookup helper function
(defun lookup (var bindings)
  (if (variable-p var)
      (let ((binding (assoc var bindings :test #'equal)))
        (if binding
            (lookup (cdr binding) bindings)
            var))
      var))

;; Occurs check helper function
(defun occurs-check (var term bindings)
  (let ((val (if (variable-p term)
                 (lookup term bindings)
                 term)))
    (cond ((equal var val) t)
          ((and (listp val)
                (some #'(lambda (x) (occurs-check var x bindings)) val)))
          (t nil))))

;; Forward declaration of unify function
(declaim (ftype (function (t t t) t) unify))

;; Helper function for list unification
(defun unify-lists (list1 list2 bindings)
  (cond 
    ((and (null list1) (null list2)) 
     bindings)
    ((or (null list1) (null list2)) 
     nil)
    (t 
     (let ((result (unify (car list1) (car list2) bindings)))
       (if (or (equal (car list1) (car list2)) result)
           (let ((next-bindings (or result bindings)))
             (unify-lists (cdr list1) (cdr list2) next-bindings))
           nil)))))

(defun unify (term1 term2 bindings)
  (let ((val1 (if (variable-p term1) 
                  (lookup term1 bindings)
                  term1))
        (val2 (if (variable-p term2)
                  (lookup term2 bindings)
                  term2)))
    (cond
      ((equal val1 val2) 
       bindings)
      ((variable-p val1)
       (if (occurs-check val1 val2 bindings)
           nil
           (cons (cons val1 val2) bindings)))
      ((variable-p val2)
       (if (occurs-check val2 val1 bindings)
           nil
           (cons (cons val2 val1) bindings)))
      ((and (listp val1) (listp val2))
       (if (not (= (length val1) (length val2)))
           nil
           (unify-lists val1 val2 bindings)))
      (t 
       nil))))

;; Apply bindings helper function
(defun apply-bindings (term bindings)
  (cond
    ((null term) nil)
    ((variable-p term)
     (let ((val (lookup term bindings)))
       (if (equal val term)
           term
           (apply-bindings val bindings))))
    ((listp term)
     (mapcar #'(lambda (x) (apply-bindings x bindings)) term))
    (t term)))

;; Find matching axioms helper function
(defun find-matching-axioms (pred axioms)
  (remove-if-not
   #'(lambda (axiom)
       (let ((head (car axiom)))
         (and (= (length head) (length pred))
              (string= (car head) (car pred)))))
   axioms))

;; Main proof function
(defun prove (goals axioms bindings)
  (if (null goals)
      (list bindings)
      (let* ((goal (car goals))
             (remaining-goals (cdr goals))
             (matching-axioms (find-matching-axioms goal axioms))
             (results nil))
        (dolist (axiom matching-axioms results)
          (let ((new-bindings (unify goal (car axiom) bindings)))
            (when new-bindings
              (let ((sub-results
                     (prove (apply-bindings remaining-goals new-bindings)
                            axioms new-bindings)))
                (setf results (append results sub-results)))))))))

;; Main entry point function
(defun prolog_prove (axioms query)
  (let ((results (prove query axioms nil)))
    (if results
        (let ((vars (remove-if-not #'variable-p (flatten query))))
          (if vars
              (mapcar #'(lambda (result)
                         (list (car vars)
                               (apply-bindings (car vars) result)))
                     results)
              t))
        nil)))


;; Test cases
;; Test 1: Simple Fact Query
(let ((axioms '((("mother" "mary" "jill"))))
      (query '(("mother" "mary" "jill"))))
  (format t "~%Test 1 (Simple Fact Query) result: ~A" (prolog_prove axioms query)))

;; Test 2: Non-Matching Query
(let ((axioms '((("father" "jim" "jill"))))
      (query '(("father" "samm" "jill"))))
  (format t "~%Test 2 (Non-Matching Query) result: ~A" (prolog_prove axioms query)))

;; Test 3: Query with Variables
(let ((axioms '((("father" "jim" "jill"))
                (("father" "samm" "jim"))))
      (query '(("father" "X" "jill"))))
  (format t "~%Test 3 (Query with Variables) result: ~A" (prolog_prove axioms query)))

;; Test 4: Multiple Matches for Variable
(let ((axioms '((("father" "jim" "jill"))
                (("father" "samm" "jill"))))
      (query '(("father" "X" "jill"))))
  (format t "~%Test 4 (Multiple Matches for Variable) result: ~A" (prolog_prove axioms query)))

;; Test 5: Rule-Based Query
(let ((axioms '((("father" "jim" "jill"))
                (("parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query '(("parent" "jim" "jill"))))
  (format t "~%Test 5 (Rule-Based Query) result: ~A" (prolog_prove axioms query)))

;; Test 6: Chained Rules
(let ((axioms '((("father" "jim" "jill"))
                (("parent" "X" "Y") "<" ("father" "X" "Y"))
                (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))))
      (query '(("ancestor" "jim" "jill"))))
  (format t "~%Test 6 (Chained Rules) result: ~A" (prolog_prove axioms query)))

