(in-package #:cl-zipper)

(defstruct path
  (left) (path) (right))

(defstruct api
  (fn-branch?)
  (fn-children)
  (fn-make-node))

(defstruct loc
  (node)
  (path)
  (api))

;;;;;;;;;; Constructors
(defun zipper (branch? children make-node root)
  (make-loc
   :node root
   :api (make-api :fn-branch? branch?
		  :fn-children children
		  :fn-make-node make-node)))

(defmethod make-zipper ((thing list))
  (zipper #'listp #'identity (lambda (node children) (declare (ignore node)) children) thing))

(defun make-node (zipper children)
  (funcall (api-fn-make-node (loc-api zipper)) zipper children))

;;;;;;;;;; Selectors
(defun branch? (zipper) (funcall (api-fn-branch? (loc-api zipper)) (loc-node zipper)))
(defun children (zipper)
  (when (branch? zipper)
    (funcall
     (api-fn-children (loc-api zipper))
     (loc-node zipper))))
(defun node (zipper) (loc-node zipper))
(defun path (zipper) (loc-path zipper))

(defun lefts (zipper)
  (when (loc-path zipper)
    (reverse (path-left (loc-path zipper)))))

(defun rights (zipper)
  (when (loc-path zipper)
    (path-right (loc-path zipper))))

;;;;;;;;;; Navigation
;;;;;;;;;;;;;;; Basic navigation
(defun down (zipper)
  (when (children zipper)
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (children zipper))
	    (loc-path fresh)
	    (make-path
	     :left nil
	     :path (loc-path zipper)
	     :right (rest (children zipper))))
      fresh)))

(defun up (zipper)
  (when (path zipper)
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh)
	    (make-node
	     zipper (append
		     (reverse (path-left (path zipper)))
		     (cons (loc-node zipper)
			   (path-right (path zipper)))))
	    (loc-path fresh) (path-path (path zipper)))
      fresh)))

(defun left (zipper)
  (when (and (path zipper) (path-left (path zipper)))
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (path-left (path zipper)))
	    (loc-path fresh)
	    (make-path
	     :left (rest (path-left (path zipper)))
	     :path (path-path (path zipper))
	     :right (cons (loc-node zipper) (path-right (path zipper)))))
      fresh)))

(defun right (zipper)
  (when (and (path zipper) (path-right (path zipper)))
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (path-right (path zipper)))
	    (loc-path fresh)
	    (make-path
	     :left (cons (loc-node zipper) (path-left (path zipper)))
	     :path (path-path (path zipper))
	     :right (rest (path-right (path zipper)))))
      fresh)))

;;;;;;;;;;;;;;; Compound navigation
(defun root (zipper)
  (if-let (z (while zipper #'up))
    (node z)))

(defun leftmost (zipper) (while zipper #'left))

(defun rightmost (zipper) (while zipper #'right))

;;;;;;;;;; Modification
(defun replace (zipper node)
  (let ((fresh (copy-loc zipper)))
    (setf (loc-node fresh) node)
    fresh))

(defun delete (zipper)
  (when (path zipper)
    (let ((fresh (copy-loc zipper))
	  (fresh-path (copy-path (loc-path zipper))))
      (cond ((rights zipper)
	     (setf (loc-node fresh) (pop (path-right fresh-path))
		   (loc-path fresh) fresh-path))
	    ((lefts zipper)
	     (setf (loc-node fresh) (pop (path-left fresh-path))
		   (loc-path fresh) fresh-path))
	    (t (setf (loc-path fresh) (path-path fresh-path))))
      fresh)))

(defun insert-child (zipper node)
  (replace
   zipper
   (make-node
    zipper
    (cond ((not (branch? zipper))
	   (list node (node zipper)))
	  ((children zipper)
	   (cons node (children zipper)))
	  (t (list node))))))

(defun append-child (zipper node)
  (replace
   zipper
   (make-node
    zipper
    (cond ((not (branch? zipper))
	   (list (node zipper) node))
	  ((children zipper)
	   (append (children zipper) (list node)))
	  (t (list node))))))

(defun insert-left (zipper node)
  (let ((fresh (copy-loc zipper))
	(fresh-path (copy-path (loc-path zipper))))
    (push node (path-left fresh-path))
    (setf (loc-path fresh) fresh-path)
    fresh))

(defun insert-right (zipper node)
  (let ((fresh (copy-loc zipper))
	(fresh-path (copy-path (loc-path zipper))))
    (push node (path-right fresh-path))
    (setf (loc-path fresh) fresh-path)
    fresh))

(defun edit (zipper f &rest args)
  (replace zipper (apply f (node zipper) args)))

(defun splice-left (zipper node-list)
  (reduce #'insert-left node-list :initial-value zipper))

(defun splice-right (zipper node-list)
  (reduce #'insert-right (reverse node-list) :initial-value zipper))


;;;;;;;;;; Traversal
;;;;;;;;;;;;;;; Depth-first interface adapted from clojure.zip
(defun bf-next (zipper)
  )

(defun next (zipper)
  (or (and (branch? zipper) (down zipper))
      (right zipper)
      (labels ((backtrack (z)
		 (when (up z)
		   (or (right (up z)) (backtrack (up z))))))
	(backtrack zipper))))

(defun prev (zipper)
  (if-let (lft (left zipper))
    (labels ((backtrack (z)
	       (if-let (child (and (branch? z) (down z)))
		 (backtrack (rightmost child))
		 z)))
      (backtrack lft))
    (up zipper)))

(defun remove (zipper) (prev (delete zipper)))

(defun find (zipper f)
  (if (and (not (branch? zipper)) (funcall f (node zipper)))
      zipper
      (let ((z zipper))
	(loop do (setf z (next z)) while z
	   if (and (not (branch? z))
		   (funcall f (node z)))
	   return z))))
