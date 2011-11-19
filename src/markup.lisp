(in-package #:cliki2)
(in-readtable cliki2)

(defvar *cliki2-rules* nil)

(defun parse-cliki2-doc (markup)
  (let ((*cliki2-rules* t)
        (curpos 0))
    (iter (multiple-value-bind (block pos)
              (esrap:parse '3bmd-grammar::block markup
                           :start curpos :junk-allowed t)
            (while block) (collect block)
            (while pos)   (setf curpos pos)))))

(defun generate-html-from-markup (markup)
  (3bmd:print-doc-to-stream
   (parse-cliki2-doc
    (sanitize:clean (3bmd::expand-tabs markup :add-newlines t) sanitize:+relaxed+))
   *html-stream*))

;;; cliki2 markup extensions

;;;; article-link

(defrule article-link (and (and (? #\\) "_(") (+ (and (! #\)) character)) #\))
  (:destructure (start article end)
    (declare (ignore start end))
    (cons :article-link (cut-whitespace (text article)))))

(defmethod print-tagged-element ((tag (eql :article-link)) *html-stream* title)
  (pprint-article-link title))

;;;; person-link

(defrule person-link (and "_P(" (+ (and (! #\)) character)) #\))
  (:destructure (start name end)
    (declare (ignore start end))
    (cons :person-link (cut-whitespace (text name)))))

(defmethod print-tagged-element ((tag (eql :person-link)) *html-stream* name)
  #H[<a href="$(#/site/account?name={name})" class="person">${name}</a>])

;;;; hyperspec-link

(defrule hyperspec-link (and "_H(" (+ (and (! #\)) character)) #\))
  (:destructure (start symbol end)
    (declare (ignore start end))
    (cons :hyperspec-link (text symbol))))

(defmethod print-tagged-element ((tag (eql :hyperspec-link)) *html-stream* symbol)
  #H[<a href="${(clhs-lookup:spec-lookup symbol)}" class="hyperspec">${symbol}</a>]) ;; where does this come from?

;;;; category-link

(defrule category-link (and (and (? #\\) "*(") (+ (and (! #\)) character)) #\))
  (:destructure (start category end)
    (declare (ignore start end))
    (cons :article-link (cut-whitespace (text category)))))

;;;; code-block

(defrule empty-lines
    (* (and (* (or #\Space #\Tab)) (? #\Return) #\Newline)))

(defrule code-block (and "<code>"
                             empty-lines
                             (+ (and (! (and empty-lines "</code>")) character))
                             empty-lines
                             "</code>")
  (:destructure (start w1 code w2 end)
    (declare (ignore start w1 w2 end))
    (cons :lisp-code-block (text code))))

(defmethod print-tagged-element ((tag (eql :lisp-code-block)) *html-stream* code)
  #H[<div class="code">${(colorize::html-colorization :common-lisp code)}</div>])

;;;; category-list

(defun category-char-p (character)
  (not (member character '(#\: #\" #\)))))

(defrule category-name (and (? #\") (+ (category-char-p character)) (? #\"))
  (:lambda (list)
    (text (second list))))

(defrule category-list (and (and (? #\\) "_/(")
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
  #H[<li>] (pprint-article-link (title article)) #H[ ${separator}
  ${(sanitize:clean (with-output-to-string (*html-stream*)
                      (generate-html-from-markup (article-description article)))
                    +links-only+)}
  </li>])

(defmethod print-tagged-element ((tag (eql :cliki2-category-list)) *html-stream* category)
  #H[<ul>] (dolist (article (sort (copy-list (articles-with-category category))
                                  #'string< :key 'canonical-title))
             (pprint-article-summary-li article "-"))
  #H[</ul>])

;;;; package-link

(defrule package-link (and ":(package" (+ (or #\Tab #\Space #\Newline #\Return)) "\"" (+ (and (! #\") character)) "\")")
  (:destructure (start w1 quote link end)
    (declare (ignore start w1 quote end))
    (cons :package-link (text link))))

(defmethod print-tagged-element ((tag (eql :package-link)) *html-stream* link)
  #H[<a href="${link}" class="download">Download ASDF package from ${link}</a>])

(define-extension-inline *cliki2-rules* cliki-rules
    (or article-link
        person-link
        hyperspec-link
        category-link
        code-block
        category-list
        package-link))
