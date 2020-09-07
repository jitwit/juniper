;;; -*- lexical-binding: t; -*-

(require 'juniper-module)

(defcustom profile-ijs
  "/home/jrn/.guix-profile/bin/profile.ijs"
  "your J initialization script")

(defun new-j ()
  (let ((J (j-engine)))
    (j-do J "BINPATH_z_ =: 1!:43''")
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" profile-ijs "'"))
    J))

(provide 'juniper)

