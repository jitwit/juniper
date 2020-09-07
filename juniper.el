;;; -*- lexical-binding: t; -*-

(require 'juniper-module)

(defcustom juniper-profile-ijs
  "~/code/juniper/profile.ijs"
  "your J initialization script")

(defcustom juniper-buffer
  "*juniper*"
  "juniper buffer")

(defun new-j ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" (expand-file-name juniper-profile-ijs) "'"))
    (j-do J "BINPATH_z_ =: 1!:43''")
    J))

(defvar J
  (new-j)
  "quick and dirty world-wide J")

(defun j-reset ()
  (setq J (new-j)))

(defun j-eval (J sentence)
  (let ((j-out (make-temp-file "juniper")))
    (j-smx J j-out)
    (j-do J sentence)
    (insert-file-contents j-out)))

(defun j-mini (sentence)
  "execute J sentence from mini buffer"
  (interactive "sJ: ")
  (with-temp-buffer
    (j-eval J sentence)
    (display-message-or-buffer (buffer-string))))

(global-set-key (kbd "M-j") 'j-mini)

(provide 'juniper)
