(DEFUN ATOMS (LST)(
                   COND((NULL LST) NIL)
                   ((ATOM (CAR LST)) (APPEND (CONS (CAR LST) NIL ) (ATOMS (CDR LST) ) ))
                   (T ( APPEND ( ATOMS ( CAR LST))( ATOMS ( CDR LST ) ) ) ))
       )

(PRINT (ATOMS '((A B) C NIL (D (E F G)))))