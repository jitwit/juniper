;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defvar ej (new-j))
(j-smx ej "blaha.txt")

(defun fake-test ()
  (j-do ej "i. 10")
  (j-do ej "''[(a.{~65+(?~26){i.26) (1!:2) 4")
  (j-do ej "%. ? 3 3 $ 0")
  (j-do ej "BINPATH -: 1!:43 ''")
  (j-do ej "ARGV")
  (j-do ej "echo i. 10"))

(fake-test)


