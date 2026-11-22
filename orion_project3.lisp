;;; 5. RECURSIVE LIST PROCESSING

;; 5.a: list-reverse
(DEFUN list-reverse (lst)
    ;; Returns a list containing the elements of the input list in reverse order.
    ; Uses a helper function and an accumulator variable, `acc`, which naturally 
    ; stores elements from the input list in reverse order using `CONS`
    (LABELS ((reverse-helper (remaining acc)
                ; Base Case: If input list is empty, return the accumulator list
                (IF (EQUAL remaining NIL)
                    acc
                    ; Otherwise, recursively append with `CAR` and iterate with `CDR`
                    (reverse-helper 
                      (CDR remaining) 
                      (CONS (CAR remaining) acc)))))
    (reverse-helper lst NIL))
)


;; 5.b: list-flatten
(DEFUN list-flatten (lst)
    ;; Converts a nested list into a single level list.
    ; Uses a helper function to combine lists as they are being flattened
    (LABELS ((join-lists (a b)
                ; Base Case: If a is empty, return b
                (IF (EQUAL a NIL)
                    b
                    ; Otherwise, append the first element of a to the recursive
                    ; result of joining the remainder of a with b, using `CONS`
                    (CONS (CAR a)
                          (join-lists (CDR a) b)))))
      (COND
        ; Base Case: If the input list is empty or exhausted, return `NIL`
        ((EQUAL lst NIL) NIL)
        ; If there's an empty sublist, skip it so it doesn't get appended
        ((EQUAL (CAR lst) NIL)
         (list-flatten (CDR lst)))
        ; If the first element is an atom, append it to our result
        ((ATOM (CAR lst))
         (CONS (CAR lst)
               (list-flatten (CDR lst))))
        ; Otherwise, the first element is a nested list, so we recusively
        ; flatten it, and everything that comes after it, then join them together.
        (T
         (join-lists
           (list-flatten (CAR lst))
           (list-flatten (CDR lst))))))
)


;; 5.c: list-remove-duplicates
(DEFUN list-remove-duplicates (lst)
    ;; Removes duplicate elements from a list.
    ; First helper function recursively checks whether an element has appeared before
    (LABELS ((seen-helper (element seen)
               (COND
                 ; If the 'seen' list is empty/exhausted, return `NIL`
                 ((EQUAL seen NIL) NIL)
                 ; If the element is in the 'seen' list, return true
                 ((EQUAL element (CAR seen)) T)
                 ; Otherwise, recusively check the remainder of the 'seen' list
                 (T
                  (seen-helper element (CDR seen)))))
             
             ; Second helper function recursively removes duplicate
             ; elements, and builds the final, resulting list
             (remove-helper (remaining seen)
               (COND
                 ; If the input list is empty/exhausted, return `NIL`
                 ((EQUAL remaining NIL) NIL)
                 ; If the current element has been seen before, skip it
                 ((seen-helper (CAR remaining) seen)
                  (remove-helper (CDR remaining) seen))
                 ; Otherwise, append the current element to the recursive result of
                 ; the next element; also append the current element to the 'seen' list
                 (T
                  (CONS (CAR remaining)
                        (remove-helper
                          (CDR remaining)
                          (CONS (CAR remaining) seen)))))))
    
    (remove-helper lst NIL))
)


;; 5.d: list-palindrome
(DEFUN list-palindrome (lst)
    ;; Returns `T` if a list reads the same forward and backward, or `NIL` otherwise
    (EQUAL lst (list-reverse lst)) ; Reverse the input list, compare, and return result
)


;;; 6. MERGE SORT

(DEFUN merge-sort (lst predicate)
    ;; Sorts a given list using divide-and-conquer technique.
    (LABELS
        ; First helper function recursively splits lists in half using two pointers
        ; The accumulator 'left' stores the values of the left split
        ((split-helper (slow fast left)
           ; If the list is exhausted, return a list with both halves as sublists
           (IF (OR (EQUAL fast NIL)
                   (EQUAL (CDR fast) NIL))
               (LIST (list-reverse left) slow)
               ; Otherwise, append to the the 'left' list and continue traversing
               (split-helper (CDR slow)
                             (CDR (CDR fast))
                             (CONS (CAR slow) left))))
                             
         ; Second helper function merges two already sorted lists based on the predicate
         (merge-helper (left right)
           (COND
             ; If the left list is exhausted, return the right
             ((EQUAL left NIL) right)
             ; If the right list is exhausted, return the left
             ((EQUAL right NIL) left)
             ; If the right element should come first (according to the predicate),
             ; attach it to the recursive result of the next element using `CONS`
             ((FUNCALL predicate (CAR right) (CAR left))
              (CONS (CAR right)
                    (merge-helper left (CDR right))))
             ; Otherwise, the left element should come first, so do the opposite 
             (T
              (CONS (CAR left)
                    (merge-helper (CDR left) right)))))
                    
         ; Third helper function coordinates splitting and merging to sort the input list
         (sort-helper (items)
           ; Base Case: If the input list is empty/exhausted, return it
           (IF (OR (EQUAL items NIL)
                   (EQUAL (CDR items) NIL))
               items
               ; Otherwise, recursively split, sort, and merge the input list
               (LET* ((halves (split-helper items items NIL))
                      (left (sort-helper (FIRST halves)))
                      (right (sort-helper (SECOND halves))))
                 (merge-helper left right)))))
                 
    (sort-helper lst))
)