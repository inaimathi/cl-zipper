;;;; test/cl-zipper.lisp

(in-package #:cl-zipper-test)

(let ((zlist (make-zipper '(1 2 (3 4 (5 6) (7 8) ((9 10) 11 (12)))))))
  (tests
   (subtest "Navigation"

     (subtest "Basics"
       (is (-> zlist down node) 1
	   "Going down gets you the next level")
       (is (-> zlist down down) nil
	   "Going down from a leaf node gets you NIL")
       (is (-> zlist down right right down node) 3
	   "Deep navigation works"))

     (subtest "Navigation symmetry"
       (is t (equalp (-> zlist down up) zlist)
	   "up and down are structurally symmetrical")
       (is nil (eq (-> zlist down up) zlist)
	     "up and down are NOT memory location symmetrical")
       (is t (equalp (-> zlist down)
		     (-> zlist down right left))
	   "left and right are structurally symmetrical")
       (is nil (eq (-> zlist down)
		   (-> zlist down right left))
	   "left and right are NOT memory location symmetrical"))

     (subtest "Traversals"

       (subtest "Primitives"
	 (is nil (-> zlist (while #'next) next)
	     "Going next off the end of the tree fails")
	 (is nil (-> zlist prev)
	     "Going back from the root of the tree fails"))

       (subtest "Search"
	 (is 10 (-> zlist (find (lambda (node) (= node 10))) node)
	     "find takes a function and finds the specified node")
	 (is 6 (-> zlist (find (lambda (node) (> node 5))) node)
	     "find returns the first matching node if there are multiples")
	 (is nil (-> zlist (find (lambda (node) (> node 75))))
	     "find returns nil if it can't find a matching node"))))

   (subtest "Alteration"

     (subtest "Deletion"
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
	   "Deleting deep parents works")))))

;; (tests
;;  (is (+ 2 3) 5 "Addition works")
;;  (is (+ 2 3) 6 "Intentionally fails")

;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (+ a b) (+ b a))
;; 	  "Addition is commutative")
;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (- a b) (- b a))
;; 	  "Subtraction is not, so this should fail"))
