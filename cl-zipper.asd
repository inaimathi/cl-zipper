;;;; cl-zipper.asd

(asdf:defsystem #:cl-zipper
  :description "Describe cl-zipper here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module
		src :components
		((:file "package")
		 (:file "util")
		 (:file "cl-zipper")))))

(asdf:defsystem #:cl-zipper-test
  :description "Test suite for :cl-zipper"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-zipper #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "cl-zipper"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
