;;; -*- lexical-binding: t; -*-

(require 'juniper-module)

(defcustom juniper-profile-ijs
  "/home/jrn/.guix-profile/bin/profile.ijs"
  "your J initialization script")

(defun new-j ()
  "create a J engine, including initialization"
  (let ((J (j-engine)))
    (j-do J "BINPATH_z_ =: 1!:43''")
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" juniper-profile-ijs "'"))
    (delete-file "juniper.txt") ; NB. need to decide how to do buffers/engines
    (j-smx J "juniper.txt")
    J))

(provide 'juniper)
