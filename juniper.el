;;; -*- lexical-binding: t; -*-
(require 'juniper-module)
(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)
(require 'filenotify)

;;;; groups
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

;; probably want `make-process' with argument `:command' as `nil'
;;;; evaluation
(defgroup j-console nil
  "REPL integration extention for `j-mode'"
  :group 'applications
  :group 'j
  :prefix "j-console-")

(defcustom j-console-cmd "ijconsole"
  "Name of the executable used for the J REPL session"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-args '() ;; '("-prompt")
  "Arguments to be passed to the j-console-cmd on start"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-init-file nil
  "Full path to the file who's contents are sent to the
  j-console-cmd on start

Should be NIL if there is no file not the empty string"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-buffer-name "J"
  "Name of the buffer which contains the j-console-cmd session"
  :type 'string
  :group 'j-console)

(defvar j-console-comint-input-filter-function nil
  "J mode specific mask for comint input filter function")

(defvar j-console-comint-output-filter-function nil
  "J mode specific mask for comint output filter function")

;; before output, there's n*3 spaces in buffer from prompts...
;; can avoid possibly, maybe.. actually this is quite horrible.
(defvar j-console-comint-preoutput-filter-function
  (lambda (out)
    (replace-regexp-in-string (rx line-start (+ "   "))
			      ""
			      out)
    out)
  "J mode specific mask for comint preoutput filter function")

(defun j-console-create-session ()
  "Starts a comint session wrapped around the j-console-cmd"

  (apply 'make-comint j-console-cmd-buffer-name
         j-console-cmd j-console-cmd-init-file j-console-cmd-args)
  (mapc
   (lambda (comint-hook-sym)
     (let ((local-comint-hook-fn-sym
            (intern
             (replace-regexp-in-string
              "s$" "" (concat "j-console-" (symbol-name comint-hook-sym))))))
       (when (symbol-value local-comint-hook-fn-sym)
         (add-hook comint-hook-sym (symbol-value local-comint-hook-fn-sym)))))
   '(comint-input-filter-functions
     comint-output-filter-functions
     comint-preoutput-filter-functions)))

(defun j-console-ensure-session ()
  "Checks for a running j-console-cmd comint session and either
  returns it or starts a new session and returns that"
  (or (get-process j-console-cmd-buffer-name)
      (progn
        (j-console-create-session)
        (get-process j-console-cmd-buffer-name))))

(define-derived-mode inferior-j-mode comint-mode "Inferior J"
  "Major mode for J inferior process.")

;;;###autoload
(defun j-console ()
  "Ensures a running j-console-cmd session and switches focus to
the containing buffer"
  (interactive)
  (switch-to-buffer-other-window (process-buffer (j-console-ensure-session)))
  (inferior-j-mode))

(defun j-console-execute-region (start end)
  "Sends current region to the j-console-cmd session and exectues it"
  (interactive "r")
  (let ((region (buffer-substring-no-properties start end))
        (session (j-console-ensure-session)))
    (pop-to-buffer (process-buffer session))
    (goto-char (point-max))
    (insert region)
    (comint-send-input)
    (insert "\n   ") ;; how to still get a prompt in "jconsole" buffer?
;; vvv currently preoutput filter. buggy on examples such as '3 3
;;    3 $ 0 1000', but i'd rather have that than this horrible current
;;    mess (replace-regexp-in-string (rx line-start (+ " ")) "" out)
    (other-window 1)))

(defun j-console-execute-line ()
  "Sends current line to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (point-at-bol) (point-at-eol)))

(defun j-console-execute-buffer ()
  "Sends current buffer to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (point-min) (point-max)))

;;;; convenience
(global-set-key (kbd "M-j") 'j-mini)
(global-set-key (kbd "C-c C-j") 'joogle)
(file-notify-add-watch juniper-viewmat-png
		       '(change)
		       (lambda (e)
			 (princ e)
			 (j-viewmat)))
;; minibuffer-inactive-mode <= a major-mode
(provide 'juniper)
