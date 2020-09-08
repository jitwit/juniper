;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defun fake-test ()
  (j-mini "echo JVERSION")
  (j-do J "((,: 255&-) ? 3 $ 256) viewmat ? 1000 1000 $ 0 [ require 'viewmat'"))

(fake-test)
