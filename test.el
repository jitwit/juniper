;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defun fake-test ()
  (j-mini "echo JVERSION")) ; wont work unless profile.ijs loads properly

(fake-test)
