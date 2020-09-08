;;; -*- lexical-binding: t; -*-
(require 'juniper-module)
(require 'juniper-font-lock)
(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)
(require 'filenotify)

;;;; groups
(defgroup juniper-mode nil
  "A mode for J"
  :group 'languages
  :prefix "j-")

(defcustom juniper-mode-hook nil
  "`juniper-mode'"
  :type 'hook
  :group 'juniper)

;;;; jfe/dynamic module
(defcustom juniper-profile-ijs
  "~/code/juniper/profile.ijs"
  "your J initialization script")

(defcustom juniper-viewmat-png
  "~/j902-user/temp/viewmat.png"
  "viewmat file")

(defvar j-viewmat-buffer
  (get-buffer-create "viewmat"))

; (defcustom juniper-buffer "*juniper*" "juniper buffer")
(defun new-j ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" (expand-file-name juniper-profile-ijs) "'"))
    (j-do J "BINPATH_z_ =: 1!:43''")
    ;; NB. suppress viewmat from trying to open file
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

;; (defun j-clean-temp-outputs ()
;;   "erase all the juniper output files"
;;   (j-mini "*./ (1!:55 :: 1:) 1 dir '/tmp/juniper*'"))

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

;;;; viewmat

;; have viewmat buffer
;; use insert-image-file
(defun j-viewmat ()
  "open and view a viewmat image"
  (when (buffer-live-p j-viewmat-buffer)
    (kill-buffer j-viewmat-buffer))
  (setq j-viewmat-buffer (get-buffer-create "viewmat"))
  (with-current-buffer j-viewmat-buffer
    (insert-image-file juniper-viewmat-png)
    (newline))
  (view-buffer-other-window j-viewmat-buffer))

;; probably want `make-process' with argument `:command' as `nil'?
;;;; evaluation
;; (defun j-console ()
;;   "Ensures a running j-console-cmd session and switches focus to
;; the containing buffer"
;;   (interactive)
;;   (switch-to-buffer-other-window (process-buffer (j-console-ensure-session)))
;;   (inferior-juniper-mode))

;; (defun j-console-execute-region (start end)
;;   "Sends current region to the j-console-cmd session and exectues it"
;;   (interactive "r")
;;   (let ((region (buffer-substring-no-properties start end))
;;         (session (j-console-ensure-session)))
;;     (pop-to-buffer (process-buffer session))
;;     (goto-char (point-max))
;;     (insert region)
;;     (comint-send-input)
;;     (other-window 1)))

;; (defun j-console-execute-line ()
;;   "Sends current line to the j-console-cmd session and exectues it"
;;   (interactive)
;;   (j-console-execute-region (point-at-bol) (point-at-eol)))

;; (defun j-console-execute-buffer ()
;;   "Sends current buffer to the j-console-cmd session and exectues it"
;;   (interactive)
;;   (j-console-execute-region (point-min) (point-max)))

;;;; convenience
(defvar juniper-mode-keymap
  (let ((map (make-sparse-keymap)))
;;    (define-key map (kbd "C-c !")   'j-console)
;;    (define-key map (kbd "C-c C-c") 'j-console-execute-buffer)
;;    (define-key map (kbd "C-c C-r") 'j-console-execute-region)
    ;;    (define-key map (kbd "C-c C-l") 'j-console-execute-line)
    (global-set-key (kbd "C-c C-j") 'joogle)
    (define-key map (kbd "M-p")     'prettify-symbols-mode)
    map)
  "Keymap for J major mode")

(define-derived-mode juniper-mode prog-mode "J"
  "Major mode for writing J."
  :syntax-table j-syntax-table
  (setq ; one day: font-lock-multiline t
        font-lock-defaults j-font-locks
	prettify-symbols-alist j->apl)
  ;; (pretty-add-keywords nil j->apl)
  (use-local-map juniper-mode-keymap))

(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . juniper-mode))
(global-set-key (kbd "M-j") 'j-mini)
(file-notify-add-watch juniper-viewmat-png
		       '(change)
		       (lambda (e)
			 ;; (princ e)
			 (j-viewmat)))

(provide 'juniper)
