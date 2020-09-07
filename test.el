;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defvar ej (new-j))

(defun fake-test ()
  (delete-file "juniper.txt")
  (j-smx ej "juniper.txt")
  (j-do ej "echo JVERSION")) ; wont work unless profile.ijs loads properly

(fake-test)
