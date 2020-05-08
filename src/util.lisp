(in-package #:cl-zipper)

(defmacro if-let ((name test) then &optional else)
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,test))
       (if ,tmp
	   (let ((,name ,tmp)) ,then)
	   ,else))))

(defun while (zipper f)
  (let ((z zipper))
    (loop for next = (funcall f z) while next
       when next do (setf z next))
    z))
