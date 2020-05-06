;;;; src/package.lisp
(defpackage #:cl-zipper
  (:use #:cl)
  (:shadow #:remove #:delete #:replace)
  (:export #:until
	   #:zipper #:make-zipper
	   #:make-node #:branch? #:children #:node #:path
	   #:down #:up #:left #:right #:root ;; #:next #:prev
	   #:leftmost #:lefts #:rightmost #:rights
	   #:replace #:edit #:delete ;; #:remove
	   #:append-child #:insert-child ;; #:append-children #:insert-children
	   #:insert-left #:splice-left #:insert-right #:splice-right))
