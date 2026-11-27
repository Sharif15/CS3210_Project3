;;; Load Project Functions
(LOAD "orion_project3.lisp")

;;; Test helper function
(DEFUN run-test (name input expected actual)
    (FORMAT t "~%Test: ~A~%" name)
    (FORMAT t "Input: ~S~%" input)
    (FORMAT t "Expected: ~S~%" expected)
    (FORMAT t "Actual: ~S~%" actual)
    (FORMAT t "Result: ~A~%"
            (IF (EQUAL expected actual)
                "PASS"
                "FAIL"))
)

;;; 4. BOOLEAN LOGIC

(FORMAT t "~%~a~%" "========== BOOLEAN LOGIC TESTS ==========")

;; 4.a: boolean-xor - Tests (4 total)

(run-test "boolean-xor - First Operand True"    ; Function and case
          '(T NIL)                              ; Input
          T                                     ; Expected
          (boolean-xor T NIL))                  ; Actual

(run-test "boolean-xor - Second Operand True"
          '(NIL T)
          T
          (boolean-xor NIL T))

(run-test "boolean-xor - Both Operands True"
          '(T T)
          NIL
          (boolean-xor T T))

(run-test "boolean-xor - Both Operands False"
          '(NIL NIL)
          NIL
          (boolean-xor NIL NIL))

;; 4.b: boolean-implies - Tests (4 total)
(run-test "boolean-implies - Both True"
          '(T T)
          T
          (boolean-implies T T))

(run-test "boolean-implies - True Implies False"
          '(T NIL)
          NIL
          (boolean-implies T NIL))

(run-test "boolean-implies - False Implies True"
          '(NIL T)
          T
          (boolean-implies NIL T))

(run-test "boolean-implies - Both False"
          '(NIL NIL)
          T
          (boolean-implies NIL NIL))

;; 4.c: boolean-iff - Tests (4 total)

(run-test "boolean-iff - Both True"
          '(T T)
          T
          (boolean-iff T T))

(run-test "boolean-iff - Both False"
          '(NIL NIL)
          T
          (boolean-iff NIL NIL))

(run-test "boolean-iff - First True, Second False"
          '(T NIL)
          NIL
          (boolean-iff T NIL))

(run-test "boolean-iff - First False, Second True"
          '(NIL T)
          NIL
          (boolean-iff NIL T))

;; 4.d: boolean-eval - Tests (5 total)

(run-test "boolean-eval - True Constant"
          T
          T
          (boolean-eval T))

(run-test "boolean-eval - False Constant"
          NIL
          NIL
          (boolean-eval NIL))

(run-test "boolean-eval - NOT and AND"
          '(NOT (AND T NIL))
          T
          (boolean-eval '(NOT (AND T NIL))))

(run-test "boolean-eval - XOR with Nested OR"
          '(XOR T (OR NIL T))
          NIL
          (boolean-eval '(XOR (OR NIL T) T)))

(run-test "boolean-eval - IMPLIES with Nested IFF"
          '(IMPLIES (IFF NIL NIL) (NOT T))
          NIL
          (boolean-eval '(IMPLIES (IFF NIL NIL) (NOT T))))

;; 4.e: boolean-simplify - Tests (8 total)
(run-test "boolean-simplify - Variable"
          'A
          'A
          (boolean-simplify 'A))

(run-test "boolean-simplify - Nested Identities"
          '(AND (OR A NIL) T)
          'A
          (boolean-simplify '(AND (OR A NIL) T)))

(run-test "boolean-simplify - Double NOT"
          '(NOT (NOT (AND T NIL)))
          'NIL
          (boolean-simplify '(NOT (NOT (AND T NIL)))))

(run-test "boolean-simplify - AND with Matching Operands"
          '(AND (NOT A) (NOT A))
          '(NOT A)
          (boolean-simplify '(AND (NOT A) (NOT A))))

(run-test "boolean-simplify - OR with Matching Operands"
          '(OR A A)
          'A
          (boolean-simplify '(OR A A)))

(run-test "boolean-simplify - OR with True"
          '(OR T A)
          T
          (boolean-simplify '(OR T A)))

(run-test "boolean-simplify - AND with False"
          '(AND A NIL)
          NIL
          (boolean-simplify '(AND A NIL)))

(run-test "boolean-simplify - Preserve Other Operands"
          '(IFF (XOR (AND T A) B) (IMPLIES C (OR NIL D)))
          '(IFF (XOR A B) (IMPLIES C D))
          (boolean-simplify '(IFF (XOR (AND T A) B) (IMPLIES C (OR NIL D)))))

          
;;; 5. RECURSIVE LIST PROCESSING (20 total tests)

(FORMAT t "~%~a~%" "========== RECURSIVE LIST PROCESSING TESTS ==========")

;; 5.a: list-reverse - Tests (5 total)

(run-test "list-reverse - Basic"        ; Name
          '(1 2 3 4)                    ; Input
          '(4 3 2 1)                    ; Expected
          (list-reverse '(1 2 3 4)))    ; Actual

(run-test "list-reverse - Empty List"
          NIL
          NIL
          (list-reverse NIL))

(run-test "list-reverse - Single Element"
          '(9)
          '(9)
          (list-reverse '(9)))

(run-test "list-reverse - Duplicates"
          '(1 3 2 2 1 3)
          '(3 1 2 2 3 1)
          (list-reverse '(1 3 2 2 1 3)))

(run-test "list-reverse - Nested Lists"
          '((1 2) 3 (4 5))
          '((4 5) 3 (1 2))
          (list-reverse '((1 2) 3 (4 5))))


;; 5.b: list-flatten - Tests (5 total)

(run-test "list-flatten - Basic"              ; Name
          '(1 (2 (3 4)) 5)                    ; Input
          '(1 2 3 4 5)                        ; Expected
          (list-flatten '(1 (2 (3 4)) 5)))    ; Actual

(run-test "list-flatten - Empty List"
          NIL
          NIL
          (list-flatten NIL))

(run-test "list-flatten - Empty Sublists"
          '(1 () (2 ()) 3)
          '(1 2 3)
          (list-flatten '(1 () (2 ()) 3)))

(run-test "list-flatten - Deep Nesting"
          '(((((1)))) ((2 (3))))
          '(1 2 3)
          (list-flatten '(((((1)))) ((2 (3))))))

(run-test "list-flatten - Mixed Elements"
          '(a (b 2) ((c c) 3))
          '(a b 2 c c 3)
          (list-flatten '(a (b 2) ((c c) 3))))


;; 5.c: list-remove-duplicates - Tests (5 total)

(run-test "list-remove-duplicates - Basic"            ; Name
          '(1 2 2 3 1 4)                              ; Input
          '(1 2 3 4)                                  ; Expected
          (list-remove-duplicates '(1 2 2 3 1 4)))    ; Actual

(run-test "list-remove-duplicates - Empty List"
          NIL
          NIL
          (list-remove-duplicates NIL))

(run-test "list-remove-duplicates - Single Element"
          '(9)
          '(9)
          (list-remove-duplicates '(9)))

(run-test "list-remove-duplicates - Oops! All duplicates"
          '(7 7 7 7 7 7)
          '(7)
          (list-remove-duplicates '(7 7 7 7 7 7)))

(run-test "list-remove-duplicates - Mixed and Nested"
          '(a (1 2) b a (1 2) 3 b)
          '(a (1 2) b 3)
          (list-remove-duplicates '(a (1 2) b a (1 2) 3 b)))


;; 5.d: list-palindrome - Tests (5 total)

(run-test "list-palindrome - Basic Palindrome"  ; Name
          '(1 2 3 2 1)                          ; Input
          T                                     ; Expected
          (list-palindrome '(1 2 3 2 1)))       ; Actual

(run-test "list-palindrome - Not a Palindrome"
          '(1 2 3 4 5)
          NIL
          (list-palindrome '(1 2 3 4 5)))

(run-test "list-palindrome - Empty List"
          NIL
          T
          (list-palindrome NIL))

(run-test "list-palindrome - Single Element"
          '(9)
          T
          (list-palindrome '(9)))

(run-test "list-palindrome - Nested Palindrome"
          '((1 2) a a (1 2))
          T
          (list-palindrome '((1 2) a a (1 2))))


;;; 6. MERGE SORT (15 total tests)

(FORMAT t "~%~a~%" "========== MERGE SORT TESTS ==========")

;; merge-sort - Tests (15 total)

(run-test "merge-sort - Basic (Ascending)"          ; Name
          '((2 1 5 0 4 3) <)                ; Input
          '(0 1 2 3 4 5)                    ; Expected
          (merge-sort '(2 1 5 0 4 3) #'<))  ; Actual

(run-test "merge-sort - Basic (Descending)"
          '((2 1 5 0 4 3) >)
          '(5 4 3 2 1 0)
          (merge-sort '(2 1 5 0 4 3) #'>))

(run-test "merge-sort - Odd Length List (Ascending)"
          '((4 5 1 3 2 7 9 8 6) <)
          '(1 2 3 4 5 6 7 8 9)
          (merge-sort '(4 5 1 3 2 7 9 8 6) #'<))

(run-test "merge-sort - Odd Length List (Descending)"
          '((4 5 1 3 2 7 9 8 6) >)
          '(9 8 7 6 5 4 3 2 1)
          (merge-sort '(4 5 1 3 2 7 9 8 6) #'>))

(run-test "merge-sort - Already Sorted List (Ascending)"
          '((1 2 3 4 5) <)
          '(1 2 3 4 5)
          (merge-sort '(1 2 3 4 5) #'<))

(run-test "merge-sort - Already Sorted List (Descending)"
          '((1 2 3 4 5) >)
          '(5 4 3 2 1)
          (merge-sort '(1 2 3 4 5) #'>))

(run-test "merge-sort - Duplicates (Ascending)"
          '((3 1 3 2 1 2) <)
          '(1 1 2 2 3 3)
          (merge-sort '(3 1 3 2 1 2) #'<))

(run-test "merge-sort - Duplicates (Descending)"
          '((3 1 3 2 1 2) >)
          '(3 3 2 2 1 1)
          (merge-sort '(3 1 3 2 1 2) #'>))

(run-test "merge-sort - Negatives (Ascending)"
          '((-2 0 3 2 -3 1 -1) <)
          '(-3 -2 -1 0 1 2 3)
          (merge-sort '(-2 0 3 2 -3 1 -1) #'<))

(run-test "merge-sort - Negatives (Descending)"
          '((-2 0 3 2 -3 1 -1) >)
          '(3 2 1 0 -1 -2 -3)
          (merge-sort '(-2 0 3 2 -3 1 -1) #'>))

(run-test "merge-sort - Non-Sequential Numbers (Ascending)"
          '((24 -9 32 0 7 2 19 -2 9 12 -5) <)
          '(-9 -5 -2 0 2 7 9 12 19 24 32)
          (merge-sort '(24 -9 32 0 7 2 19 -2 9 12 -5) #'<))

(run-test "merge-sort - Non-Sequential Numbers (Descending)"
          '((24 -9 32 0 7 2 19 -2 9 12 -5) >)
          '(32 24 19 12 9 7 2 0 -2 -5 -9)
          (merge-sort '(24 -9 32 0 7 2 19 -2 9 12 -5) #'>))

(run-test "merge-sort - Empty List"
          '(NIL <)
          NIL
          (merge-sort NIL #'<))

(run-test "merge-sort - Single Element"
          '((19) <)
          '(19)
          (merge-sort '(19) #'<))

; Test to verify that the original input is not modified:
(LET ((original '(4 1 3 2)))
  (merge-sort original #'<)
  (run-test "merge-sort - Input Unchanged"
            original
            '(4 1 3 2)
            original))