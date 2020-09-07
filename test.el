;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defvar ej (j-engine))

(defun fake-test ()
  (delete-file "blaha.txt")
  (j-smx ej "blaha.txt")
  (j-do ej "i. 10")
  ;; (j-do ej "(a.{~65+(?~26){i.26) (1!:2) 4")
  )
;; < 'blaha.txt'"))

(fake-test)

