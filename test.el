;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defvar ej (j-engine))

(defun fake-test ()
  (delete-file "blaha.txt")
  (j-do ej "(a.{~65+(?~26){i.26) (1!:2) < 'blaha.txt'"))

(fake-test)

