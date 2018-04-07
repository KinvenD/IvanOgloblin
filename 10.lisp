(DEFUN REPEAT-TR (F N X)
 (IF (= N 1)
     X
   (REPEAT-TR F (- N 1) (FUNCALL F X))))

(DEFUN LIST-NTH (N L) (CAR(REPEAT-TR 'CDR N L)))

(DEFUN LENGTH1 (LST) 
       (COND ((ZEROP (LENGTH LST))
              NIL)
             (T
              (LENGTH LST))))

(DEFUN PERMUT(LST &OPTIONAL (N (LENGTH1 LST)) Q) 
       (COND ((NULL N)
              Q)
             ((= N 0) 
              NIL) 
             (T 
              (CONS (PERMUT (CUTOUT LST N) (LENGTH1 (CUTOUT LST N)) (CONS (LIST-NTH N LST) Q)) 
                    (PERMUT LST (- N 1) Q)) ) ))
(DEFUN PERMUT1 (LST) (COND ( (NULL LST) NIL ) ( (LISTP (CAR LST) ) (APPEND (PERMUT1(CAR LST)) (PERMUT1(CDR LST)) ) ) ( (ATOM (CAR LST)) (CONS LST NIL) ) ) )

(DEFUN CUTOUT(LST N) (COND ( (> N 1) (CONS (CAR LST) (CUTOUT (CDR LST) (- N 1) ) ) ) (T (CDR LST)) ))

;(TRACE PERMUT)
(PRINT(PERMUT1( PERMUT'(1 2 3 4)) ))
