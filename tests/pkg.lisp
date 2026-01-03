;;; pkg.lisp
(defpackage #:xlib/tests
  (:use :cl :rt #:xft #:ttf)
  (:export #:font-window #:with-test-window))
(in-package :xlib/tests)
(defsuite :xlib)
