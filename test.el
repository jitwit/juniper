;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/juniper")
(require 'juniper)

(defun fake-test ()
  (delete-file juniper-viewmat-png)
  (j-over-mini "echo JVERSION [ require 'viewmat'")
  (j-do J "f =: [: (,\"_1/)^:2 (3 3 $ 1 0)&(*\"0 _)")
  (j-do J "((,: 255&-) ? 3 $ 256) viewmat (f ^: 6) 1"))

(fake-test)
