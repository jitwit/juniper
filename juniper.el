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
  "your J initialization script"
  :group 'juniper)

(defcustom juniper-binpath
  "~/.guix-profile/bin"
  "the directory J expects to find libj.so, jconsole, and such"
  :group 'juniper)

(defcustom juniper-viewmat-png
  "~/j902-user/temp/viewmat.png"
  "viewmat file"
  :group 'juniper)

(defvar j-viewmat-buffer
  (get-buffer-create "viewmat"))

(defun j-new ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J "ARGV_z_ =: 'emacs'")
    (j-do J (concat "BINPATH_z_ =: '" (expand-file-name juniper-binpath) "'"))
    (j-do J (concat "0!:0 < '" (expand-file-name juniper-profile-ijs) "'"))
    ;; NB. suppress viewmat from trying to open file itself
    (j-do J "VISIBLE_jviewmat_ =: 0 [ require 'viewmat plot'")
    J))

(defvar juniper-place->j
  (make-hash-table :test 'equal)
  "Table mapping files? to J instances")

;; j instances should have J engine, home directory, optionally:
;; project main, project test... maybe should just learn projectile
;; (or similar)?
(defun j-create-instance (where)
  "associate a location with a J, unless already associated"
  (unless (gethash where juniper-place->j)
    (let ((J (j-new))
	  (out (get-buffer-create (concat "J <" where ">"))))
      (j-do J (concat "1!:44 '" (expand-file-name where) "'"))
      (puthash where
	       `((engine . ,J)
		 (where . ,where)
		 (out . ,out))
	       juniper-place->j))))

(defun j-eval (J sentence)
  "have `J' interpret a single `sentence'"
  (let ((j-out (make-temp-file "juniper/")))
    (j-smx J j-out)
    (j-do J sentence)
    (insert-file-contents j-out)))

(defun j-eval* (J sentences)
  "have `J' interpret several `sentences' through 0!:0"
  (let ((j-in  (make-temp-file "juniper/" nil nil sentences))
	(j-out (make-temp-file "juniper/")))
    (j-smx J j-out)
    (j-do J (concat "0!:0 < '" j-in "'"))
    (insert-file-contents j-out)))

(defun j-over-mini (sentence)
  "execute J sentence from mini buffer"
  (interactive "sJ: ")
  (let ((J (gethash "~" juniper-place->j)))
    (with-temp-buffer
      (j-eval (cdr (assq 'engine J)) sentence)
      (display-message-or-buffer (buffer-string)))))

(defun j-over-region (J a b)
  "Send region to J"
  (interactive "r")
  (let ((engine (cdr (assq 'engine J)))
	(out (cdr (assq 'out J))))
    ;; todo get-buffer-create doesn't make a new one
    ;; (unless (buffer-live-p out) (setq out (get-buffer out)))
    (let ((sentences (buffer-substring-no-properties a b)))
      (j-do engine (concat "1!:44 '" default-directory "'"))
      (pop-to-buffer out)
      (goto-char (point-max))
      (j-eval engine sentences)
      (goto-char (point-max))
      (other-window 1))))

(defun j-over-region* (J a b)
  "Send region to J"
  (interactive "r")
  (let ((engine (cdr (assq 'engine J)))
	(out (cdr (assq 'out J))))
    ;; todo get-buffer-create doesn't make a new one
    ;; (unless (buffer-live-p out) (setq out (get-buffer out)))
    (let ((sentences (buffer-substring-no-properties a b)))
      (j-do engine (concat "1!:44 '" default-directory "'"))
      (pop-to-buffer out)
      (goto-char (point-max))
      (j-eval* engine sentences)
      (goto-char (point-max))
      (other-window 1))))

(defun j-over-line ()
  "Send line to J"
  (interactive)
  (let* ((where (buffer-file-name))
	 (J (gethash where juniper-place->j)))
    (cond (J (j-over-region J (point-at-bol) (point-at-eol)))
	  (t (j-create-instance where) (j-over-line)))))

(defun j-over-buffer ()
  "Send buffer to J"
  (interactive)
  (let* ((where (buffer-file-name))
	 (J (gethash where juniper-place->j)))
    (cond (J (j-over-region* J (point-min) (point-max)))
	  (t (j-create-instance where) (j-over-buffer)))))

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
    (define-key map (kbd "C-c c")   'j-over-buffer)
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

(let ((/tmp/juniper (concat (temporary-file-directory) "juniper")))
  (unless (file-exists-p /tmp/juniper)
    (mkdir /tmp/juniper))
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . juniper-mode))
  (global-set-key (kbd "M-j") 'j-over-mini)
  (j-create-instance "~"))
  ;; too shoddy for now
  ;; (file-notify-add-watch juniper-viewmat-png
  ;; 		       '(change)
  ;; 		       (lambda (e)
  ;; 			 ;; (princ e)
  ;; 			 (j-viewmat)))


(provide 'juniper)
