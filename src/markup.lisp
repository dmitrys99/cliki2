(in-package #:cliki2)
(in-readtable cliki2)

(defvar *cliki2-rules* (alexandria:copy-hash-table *rules*))

(defmacro define-rule (symbol expression &body options)
  `(let ((*rules* *cliki2-rules*))
     (defrule ,symbol ,expression ,@options)))

(let ((cliki2-grammar (esrap:compile-grammar '3bmd-grammar::block)))
  (defun parse-cliki2-doc (markup)
    (let ((*rules* *cliki2-rules*)
          (curpos 0))
      (iter (multiple-value-bind (block pos)
                (parse cliki2-grammar markup :start curpos :junk-allowed t)
              (while block) (collect block)
              (while pos)   (setf curpos pos))))))

(defun generate-html-from-markup (markup &optional
                                  (sanitize-mode sanitize:+relaxed+))
  (sanitize:clean
   (with-output-to-string (s)
     (3bmd:print-doc-to-stream
      (parse-cliki2-doc (3bmd::expand-tabs markup :add-newlines t)) s))
   sanitize-mode))

;;; cliki2 markup extensions

;;;; article-link

(define-rule article-link (and (and (? #\\) "_(") (+ (and (! #\)) character)) #\))
  (:destructure (start article end)
    (declare (ignore start end))
    (cons :article-link (cut-whitespace (text article)))))

(defmethod 3bmd:print-tagged-element ((tag (eql :article-link)) *html-stream* title)
  (pprint-article-link title))

;;;; person-link

(define-rule person-link (and "_P(" (+ (and (! #\)) character)) #\))
  (:destructure (start name end)
    (declare (ignore start end))
    (cons :person-link (cut-whitespace (text name)))))

(defmethod 3bmd:print-tagged-element ((tag (eql :person-link)) *html-stream* name)
  #H[<a href="$(#/site/account?name={name})" class="person">${name}</a>])

;;;; hyperspec-link

(define-rule hyperspec-link (and "_H(" (+ (and (! #\)) character)) #\))
  (:destructure (start symbol end)
    (declare (ignore start end))
    (cons :hyperspec-link (text symbol))))

(defmethod 3bmd:print-tagged-element ((tag (eql :hyperspec-link)) *html-stream* symbol)
  #H[<a href="${(clhs-lookup:spec-lookup symbol)}" class="hyperspec">${symbol}</a>]) ;; where does this come from?

;;;; category-link

(define-rule category-link (and (and (? #\\) "*(") (+ (and (! #\)) character)) #\))
  (:destructure (start category end)
    (declare (ignore start end))
    (cons :article-link (cut-whitespace (text category)))))

;;;; code-block

(define-rule empty-lines
    (* (and (* (or #\Space #\Tab)) (? #\Return) #\Newline)))

(define-rule code-block (and "<code>"
                             empty-lines
                             (+ (and (! (and empty-lines "</code>")) character))
                             empty-lines
                             "</code>")
  (:destructure (start w1 code w2 end)
    (declare (ignore start w1 w2 end))
    (cons :lisp-code-block (text code))))

(defmethod 3bmd:print-tagged-element ((tag (eql :lisp-code-block)) *html-stream* code)
  #H[<div class="code">${(colorize::html-colorization :common-lisp code)}</div>])

;;;; category-list

(defun category-char-p (character)
  (not (member character '(#\: #\" #\)))))

(define-rule category-name (and (? #\") (+ (category-char-p character)) (? #\"))
  (:lambda (list)
    (text (second list))))

(define-rule category-list (and (and (? #\\) "_/(")
                                category-name
                                (* (and (! #\)) character))
                                ")")
  (:lambda (list)
    (cons :cliki2-category-list (category-keyword (second list)))))

(sanitize:define-sanitize-mode +links-only+
    :elements ("a")
    :attributes (("a" . ("href" "class")))
    :protocols  (("a" . (("href" . (:ftp :http :https :mailto :relative))))))

(defun pprint-article-summary-li (article separator)
  #H[<li>] (pprint-article-link (title article))
  #H[ ${separator}
  ${(generate-html-from-markup (article-description article) +links-only+)}
  </li>])

(defmethod 3bmd:print-tagged-element ((tag (eql :cliki2-category-list)) *html-stream* category)
  #H[<ul>] (dolist (article (sort (copy-list (articles-with-category category))
                                  #'string< :key 'canonical-title))
             (pprint-article-summary-li article "-"))
  #H[</ul>])

;;;; package-link

(define-rule package-link (and ":(package" (+ (or #\Tab #\Space #\Newline #\Return)) "\"" (+ (and (! #\") character)) "\")")
  (:destructure (start w1 quote link end)
    (declare (ignore start w1 quote end))
    (cons :package-link (text link))))

(defmethod 3bmd:print-tagged-element ((tag (eql :package-link)) *html-stream* link)
  #H[<a href="${link}" class="download">Download ASDF package from ${link}</a>])

;;;; cliki2 markup extensions

(define-rule 3bmd-grammar::inline-extensions
    (or article-link
        person-link
        hyperspec-link
        category-link
        code-block
        category-list
        package-link))
