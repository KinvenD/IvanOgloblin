;gnu clisp 2.49

(DEFUN LENGTH1 (X)
       (COND (X (+ (LENGTH (CDR X)) 1))
             (T 0)))
(PRINT (LENGTH1 '(A B C)))