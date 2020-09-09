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

(defvar j-output-buffer
  (get-buffer-create "J"))

; (defcustom juniper-buffer "*juniper*" "juniper buffer")
(defun new-j ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "0!:0 < '" (expand-file-name juniper-profile-ijs) "'"))
    (j-do J "BINPATH_z_ =: 1!:43''")
    ;; NB. suppress viewmat from trying to open file itself
    (j-do J "VISIBLE_jviewmat_ =: 0 [ require 'viewmat plot'")
    J))

(defvar J
  (new-j)
  "quick and dirty world-wide J")

(defun j-reset ()
  (setq J (new-j)))

(defun j-eval (J sentence)
  "interpret a single sentence"
  (if (eq J nil) (error "error! J machine is nil")
    (let ((j-out (make-temp-file "juniper")))
      (j-smx J j-out)
      (j-do J sentence)
      (insert-file-contents j-out))))

(defun j-eval* (J sentences)
  "interpret several sentences"
  (if (eq J nil) (error "error! J machine is nil")
    (let ((j-in  (make-temp-file "juniper" nil nil sentences))
	  (j-out (make-temp-file "juniper")))
      (j-smx J j-out)
      (j-do J (concat "0!:0 < '" j-in "'"))
      (insert-file-contents j-out))))

(defun j-over-mini (sentence)
  "execute J sentence from mini buffer"
  (interactive "sJ: ")
  (with-temp-buffer
    (j-eval J sentence)
    (display-message-or-buffer (buffer-string))))

(defun j-over-region (a b)
  "Send region to J"
  (interactive "r")
  (unless (buffer-live-p j-output-buffer)
    (setq j-output-buffer (get-buffer-create "J")))
  (let ((sentence (buffer-substring-no-properties a b)))
    (pop-to-buffer j-output-buffer)
    (goto-char (point-max))
    (j-eval J sentence)
    (goto-char (point-max))
    (other-window 1)))

(defun j-over-region* (a b)
  "Send region to J"
  (interactive "r")
  (unless (buffer-live-p j-output-buffer)
    (setq j-output-buffer (get-buffer-create "J")))
  (let ((sentence (buffer-substring-no-properties a b)))
    (pop-to-buffer j-output-buffer)
    (goto-char (point-max))
    (j-eval* J sentence)
    (goto-char (point-max))
    (other-window 1)))

(defun j-over-line ()
  "Send line to J"
  (interactive)
  (j-over-region (point-at-bol) (point-at-eol)))

(defun j-over-buffer ()
  ;; plz fix me, spelling error, whereas j-over-line ok?  have J cd to
  ;; where file is... seems to happen when defining, =:. still works
  ;; from mini buffer...
  "Send buffer to J"
  (interactive)
  (j-over-region* (point-min) (point-max)))

;;;; documentation
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

(defun j-docs ()
  "only works on my guix when j-docs-help addon is present"
  (interactive)
  (browse-url "~/.guix-profile/share/j/addons/docs/help/index.htm"))

;;;; viewmat
(defun j-viewmat ()
  "open and view a viewmat image"
  (when (buffer-live-p j-viewmat-buffer)
    (kill-buffer j-viewmat-buffer))
  (setq j-viewmat-buffer (get-buffer-create "viewmat"))
  (with-current-buffer j-viewmat-buffer
    (insert-image-file juniper-viewmat-png))
  (view-buffer j-viewmat-buffer))

;; probably want `make-process' with argument `:command' as `nil'?
;;;; evaluation

;;;; mode
(defvar juniper-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'j-over-buffer)
    (define-key map (kbd "C-c l")   'j-over-line)
    (define-key map (kbd "C-c i")   'j-docs)
    (define-key map (kbd "C-c j")   'joogle)
    (define-key map (kbd "M-p")     'prettify-symbols-mode)
    map)
  "Keymap for J major mode")

(define-derived-mode juniper-mode prog-mode "J"
  "Major mode for wielding J."
  :syntax-table j-syntax-table
  (setq ; one day: font-lock-multiline t
        font-lock-defaults j-font-locks
	prettify-symbols-alist j->apl) ;; (pretty-add-keywords nil j->apl)
  (use-local-map juniper-mode-keymap))

(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . juniper-mode))
(global-set-key (kbd "M-j") 'j-over-mini)
(file-notify-add-watch juniper-viewmat-png
		       '(change)
		       (lambda (e)
			 ;; (princ e)
			 (j-viewmat)))

(provide 'juniper)
