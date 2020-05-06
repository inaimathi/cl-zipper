;;;; test/cl-zipper.lisp

(in-package #:cl-zipper-test)

(let ((zlist (make-zipper '(1 2 (3 4 (5 6) (7 8) ((9 10) 11 (12)))))))
  (tests
   (is (delete zlist) nil
       "Deleting the root fails")
   (is (root (delete (down zlist)))
       '(2 (3 4 (5 6) (7 8) ((9 10) 11 (12))))
       "Basic deletion works")
   (is (root (delete (down (right (right (down zlist))))))
       '(1 2 (4 (5 6) (7 8) ((9 10) 11 (12))))
       "Deleting deep leaves works")
   (is (root (delete (right (down (right (right (down zlist)))))))
       '(1 2 (3 (5 6) (7 8) ((9 10) 11 (12))))
       "Deleting deep right leaves works")
   (is (root (delete (right (right (down (right (right (down zlist))))))))
       '(1 2 (3 4 (7 8) ((9 10) 11 (12))))
       "Deleting deep parents works")))

;; (tests
;;  (is (+ 2 3) 5 "Addition works")
;;  (is (+ 2 3) 6 "Intentionally fails")

;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (+ a b) (+ b a))
;; 	  "Addition is commutative")
;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (- a b) (- b a))
;; 	  "Subtraction is not, so this should fail"))
