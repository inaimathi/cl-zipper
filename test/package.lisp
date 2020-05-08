;;;; test/package.lisp

(defpackage #:cl-zipper-test
  (:use #:cl #:cl-zipper #:test-utils)
  (:shadowing-import-from #:cl-zipper #:delete #:replace #:find #:remove #:->))
