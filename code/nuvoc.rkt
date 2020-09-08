#lang racket

(require html-parsing
         sxml/sxpath
         sxml)

;; to protect against suspect changes such as the most recent (07-08-20)
(define save-NuVoc-url
  "https://web.archive.org/web/20200524060459/https://code.jsoftware.com/wiki/NuVoc")
(define jsoftware.com "https://code.jsoftware.com")
(define NuVoc.html "NuVoc.html")
(define NuVoc.el "NuVoc.el")

(define nuvoc
  (with-input-from-file NuVoc.html
    (lambda ()
      (html->xexp (current-input-port)))))

;;;; Parse they key table
;;; Look for the color codes to use to tag J entities later
(define parse-key
  (compose (sxpath '(// td))
           (node-pos 2)
           (sxpath '(// table))))

(define jdoc-key
  `(("#ffffff" . Comment) ; not in table
    ,@(filter-map (lambda (x)
                    (match x
                      (`(td (@ (style ,css)) ,blah)
                       (let ((speech-type (string-trim blah))
                             (item-color (string-trim (last (string-split css ":")) ";")))
                         (match speech-type
                           ("KEY:" #f)
                           ("" #f)
                           (_  (cons item-color (string->symbol speech-type))))))
                      (_ #f)))
                  (parse-key nuvoc))))

;;;; Parse the main table. Pass 1
;;; groups td's together that are identified as something J-relvant
;;; and tags them with tag J1.
;; todo: would feel nicer to use sxt over explicit named let...
(define (jdoc-entity? node) ;; doesn't catch end of row?
  (match (assoc 'style (sxml:attr-list node))
    (`(style ,css) (and (string-contains? css "border-right:none")
                        (not (string-contains? css "border-left:none"))))
    (_ #f)))

(define (group nodes)
  (let loop ((nodes nodes) (xs '()) (xss '()))
    (match nodes
      ('() (cons '*TOP* (reverse xss)))
      (`(,x ,nodes ...)
       (if (jdoc-entity? x)
         (loop nodes (list x) (cons (cons 'J1 (reverse xs)) xss))
         (loop nodes (cons x xs) xss))))))

(define parse1
  (compose group
           (sxpath '(// td))
           (node-pos 3)
           (sxpath '(// table))))

;;;; Parse main table. Pass 2
;;; Classify J1 nodes according to style sheet in first td. Retag as
;;; J2.
(define (css->speech css)
  (let ((speech (assf (lambda (color)
                        (string-contains? css color))
                      jdoc-key)))
    (and speech (cdr speech))))

(define (J-speech td)
  (match td
    (`(td (@ (style (*text* ,css))) . ,rem)
     (cons (css->speech css) rem))
    (`(td (@ ,row-span (style (*text* ,css))) . ,rem)
     (cons (css->speech css) rem))
    (_ '(bug-jspeech))))

(define (clean-td td)
  (match td
    ('(td
       (@ (style (*text* "border-left:none;border-right:none")))
       (*text* "•\n"))
     #f)
    ('(td (*text* "•\n"))
     #f)
    (`(td (@ . ,atts) .
          ,blah)
     `(td ,@(filter (lambda (b)
                      (not (member b '((*text* "  ")
                                       (*text* "\n")
                                       (*text* ", ")
                                       (*text* " ")
                                       (*text* "•\n")))))
                    blah)))
    (_ td)))

(define (J1->speech j1 . tds)
  (match tds
    ('() #f)
    (`(,td . ,tds)
     `(J ,(J-speech td) ,@(filter-map clean-td tds)))
    (_ `(bug-j1speech))))

(define (parse-a a as . bod)
  (match as
    (`(@ . ,as)
     `(info ;; mild danger in assuming cdr
       (url ,@(cdr (assq 'href as)))
       (description ,@bod)))
    (_ '(bug-parse-a))))

(define (parse2 nodes)
  (pre-post-order
   ((compose (sxml:modify '("//br" delete))
             (sxml:modify '("//p" delete-undeep)))
    nodes)
   `((*text* . ,(lambda x x))
     (*default* . ,(lambda x x))
     (a . ,parse-a)
     (J1 . ,J1->speech))))

;;;; Pass 3. Clean up more random stuff and organize
(define (clean-identifier speech . ids)
  (cons speech (filter-map (lambda (id)
                             (and (not (member id '("\n" " " " to " " \n")))
                                  (match id
                                    (`(tt ,id) `(j-token ,id))
                                    (_ id))))
                           ids)))

(define (parse3 nodes)
  (pre-post-order
   nodes
   `((*text* . ,(lambda (_ x) x))
     (*default* . ,(lambda x x))
     ,@(map (lambda (speech)
              (cons (cdr speech) clean-identifier))
            jdoc-key))))

;;;; Pass 4. Clean up more random stuff and organize
;; char " " determins if tt thing describes rank
;; space . seeems to imply a name
;; (define (tt->rank tt . ))
(define (monad-rank-desc? desc)
  ;; danger/todo: mv
  (member desc '("0" "1" "2" "_" "mu")))

(define (dyad-rank-desc? desc)
  (string-contains? desc " "))

(define (tt->rank . t)
  (match t
    (`(tt ,(? monad-rank-desc? xs))
     `(monad-rank ,xs))
    (`(tt ,(? dyad-rank-desc? xs))
     `(dyad-rank ,xs))
    (_ t)))

(define (parse4 nodes)
  ((sxml:modify '("//td" delete-undeep))
   (pre-post-order
    (filter identity nodes)
    `((*text* . ,(lambda (_ x) x))
      (*default* . ,(lambda x x))
      (tt . ,tt->rank)))))

;;;; Pass 5. Join rank descriptions and info items
;; besides weird objects like rank ("), it seems 1 and 2 about tags
;; can determine how to group a given J speech entity.
(define (join-info J speech . about)
  (match (car speech) ;; tag of speech
    ('Verb `(J ,speech ,@(join-info* about)))
    ('Conjunction `(J ,speech ,@(join-info* about)))
    ('Adverb `(J ,speech ,@(join-info* about)))
    (_ `(J ,speech ,@about))))

;; issues '"(' <= ?. and ^:
;; issues '" (' <= " and :
(define (join-info* about)
  (match about
    ;; these catch all but ?. ^: " : ^ |. and control
    (`((monad-rank ,mr) (info . ,mi) (info . ,di) (dyad-rank ,dr))
     `((info (valence monad) (rank ,mr) ,@mi)
       (info (valence dyad) (rank ,dr) ,@di)))
    (`((monad-rank ,mr) (info . ,i) (dyad-rank ,dr))
     `((info (valence monad) (rank ,mr) ,@i)
       (info (valence dyad) (rank ,dr) ,@i)))
    (`((tt ,mr) (info . ,i) (dyad-rank ,dr))
     `((info (valence dyad) (rank ,mr ,dr) ,@i)))
    (`((monad-rank ,mr) (info . ,i))
     `((info (valence monad) (rank ,mr) ,@i)))
    (`((info . ,i) (dyad-rank ,dr))
     `((info (valence dyad) (rank ,dr) ,@i)))
    ;; & (bug in mapping to monad-rank / dyad-rank tags, i believe)
    (`((monad-rank ,mr) (info . ,i1) (dyad-rank ,dr) (tt ,mv) (info . ,i2) (dyad-rank ,mvmv))
     `((info (valence monad) (rank ,mr) ,@i1)
       (info (valence dyad) (rank ,dr) ,@i1)
       (info (valence dyad) (rank ,mv ,mvmv) ,@i2)))
    ;; "
    (`(,info ,op (tt ,un) (info . ,i1) (tt ,mn) (info . ,i2) " , " (tt ,uv) (info . ,i3) ,cp)
     `(,info
       (info (valence dyad) (rank ,un) ,@i1)
       (info (valence dyad) (rank ,mn) ,@i2)
       (info (valence dyad) (rank ,uv) ,@i3)))
    ;; ^:
    (`((monad-rank ,mr) (info . ,info) (dyad-rank ,dr) ,op (info . ,i1) (info . ,i2) (info . ,i3) ,cp)
     `((info (valence monad) (rank ,mr) ,@info)
       (info (valence dyad) (rank ,dr) ,@i1)
       (info (valence dyad) (rank ,dr) ,@i2)
       (info (valence dyad) (rank ,dr) ,@i3)))
    ;; ?.
    (`((monad-rank ,mr) (info ,url1 (description ,d1)) (info ,url2 (description ,d2)) (dyad-rank ,dr) ,seed)
     `((info (valence monad) (rank ,mr) ,url1 (description ,(string-trim (string-append d1 seed))))
       (info (valence dyad)  (rank ,dr) ,url2 (description ,(string-trim (string-append d2 seed))))))
    ;; ^
    (`((monad-rank ,mr) (info . ,mi) ,nl (info . ,i1) (dyad-rank ,dr1) (info . ,i2) (dyad-rank ,dr2))
     `((info (valence monad) (rank ,mr) ,@mi)
       (info (valence dyad) (rank ,dr1) ,@i1)
       (info (valence dyad) (rank ,dr2) ,@i2)))
    ;; |.
    (`((monad-rank ,mr1) (info . ,i1) ,sp (monad-rank ,mr2) (info . ,i2) (info . ,i3) (dyad-rank ,dr1) ,sp (info . ,i4) (dyad-rank ,dr2))
     `((info (valence monad) (rank ,mr1) ,@i1)
       (info (valence monad) (rank ,mr2) ,@i2)
       (info (valence dyad) (rank ,dr1) ,@i3)
       (info (valence dyad) (rank ,dr2) ,@i4)))
    ;; b. todo sort how better how to describe valence/bivalence
    (`((monad-rank ,mr1) (info . ,i1) (info . ,i2) (info . ,i3) (dyad-rank ,dr) (monad-rank ,mr2) (info . ,i4))
     `((info (valence dyad) (rank ,mr1 ,dr) ,@i1)
       (info (valence dyad) (rank ,mr1 ,dr) ,@i2)
       (info (valence dyad) (rank ,mr1 ,dr) ,@i3)
       (info (valence dyad) (rank ,mr1 ,dr) ,@i4)))
    ;; :
    (`(,i1 ,op ,i2 ,i3 ,i4 ,cp)
     `(,i1 ,i2 ,i3 ,i4))
    ;; todo  C. d.
    (_ about)))

(define (parse5 nodes)
  (pre-post-order
   nodes
   `((*text* . ,(lambda (_ x) x))
     (*default* . ,(lambda x x))
     (J . ,join-info))))

;;;; Pass 6. fill urls, delete-undeep some tags
(define (fill-url a url)
  `(,a ,(string-append jsoftware.com url)))

(define (parse6 nodes)
  (pre-post-order
   ((sxml:modify '("//j-token" delete-undeep))
    nodes)
   `((*text* . ,(lambda (_ x) x))
     (*default* . ,(lambda x x))
     (url . ,fill-url))))

;;;; Parse the nuvoc
(define parse
  (compose parse6 parse5 parse4 parse3 parse2 parse1))

;;;; Dump information gleaned from nuvoc in s expression
(define (dump-jdoc)
  (when (file-exists? NuVoc.el)
    (delete-file NuVoc.el))
  (with-output-to-file NuVoc.el
    (lambda ()
      (write
       `(defvar j-nuvoc
          ',(cdr (parse nuvoc))
          "The J NuVoc"))
      (newline)
      (write '(provide 'NuVoc)))))


(dump-jdoc)
