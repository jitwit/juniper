;;; -*- lexical-binding: t; -*-
(require 'juniper-module)
(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)

;;;; jfe/dynamic module
(defcustom juniper-profile-ijs
  "~/code/juniper/profile.ijs"
  "your J initialization script")

; (defcustom juniper-buffer "*juniper*" "juniper buffer")

(defun new-j ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" (expand-file-name juniper-profile-ijs) "'"))
    (j-do J "BINPATH_z_ =: 1!:43''")
    ;; NB. todo: add hook to display viewmat in emacs buffer    
    (j-do J "VISIBLE_jviewmat_ =: 0 [ require 'viewmat plot'")
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


;;;; documentation
(defun the-NuVoc ()
  (read (format "(progn %s)"
                (with-temp-buffer
                  (insert-file-contents "data/j.sexp")
                  (buffer-string)))))

(defun j-find-thing (thing)
  "Find information about thing (exact match)"
  (interactive "sthing: ")
  (seq-find #'(lambda (jentity)
                (member thing (cdadr jentity)))
            j-nuvoc))

(defun j-urls (thing)
  "Look up urls related to a thing (exact match)"
  (let ((entity (j-find-thing thing)))
    (if entity
        (seq-map #'(lambda (info)
                     ;; guaranteed fields
                     (append (cdr (assoc 'description (cdr info)))
                             (cdr (assoc 'url (cdr info)))))
                 (seq-filter #'(lambda (kv)
                                 (equal (car kv) 'info))
                             (cdr entity)))
      nil)))

(defun j-names (thing)
  "Look up english names for thing"
  (seq-map #'car (j-urls thing)))

(defun joogle (thing)
  "Present a popup with links to information about thing"
  (interactive "sJOOGLE: ")
  (let ((urls (seq-map #'(lambda (url)
                           (popup-make-item (seq-elt url 0)
					    :value
					    (seq-elt url 1)))
                       (j-urls thing))))
    (when urls
      (browse-url (popup-menu* urls)))))

(defun jdocs ()
  "only works on my guix when j-docs-help addon is present"
  (interactive)
  (browse-url "~/.guix-profile/share/j/addons/docs/help/index.htm"))


;;;; convenience
(global-set-key (kbd "M-j") 'j-mini)
(global-set-key (kbd "C-c C-j") 'joogle)


(provide 'juniper)
