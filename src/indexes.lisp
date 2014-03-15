(in-package #:cliki2)
(in-readtable cliki2)

;;; topics and backlinks

(defun canonicalize (title)
  (string-downcase (cut-whitespace title)))

(defun collect-links (link-type content)
  (let (names)
    (ppcre:do-register-groups (name) (#?/${link-type}\(([^\)]*)\)/ content)
      (pushnew (canonicalize name) names :test #'string=))
    names))

(defun topics (content)
  (collect-links "\\*" content))

(defun page-links (content)
  (collect-links "\\_" content))

;;; full-text search

(defparameter *common-words*
  '("" "a" "also" "an" "and" "are" "as" "at" "be" "but" "by" "can" "for" "from"
    "has" "have" "here" "i" "if" "in" "is" "it" "not" "of" "on" "or" "s" "see"
    "so" "that" "the" "there" "this" "to" "us" "which" "with" "you"))

(defun words (content)
  (let (words)
    (dolist (word (ppcre:split "<.*?>|_|\\s|[^\\w]" (string-downcase content)))
      (unless (member word *common-words* :test #'string=)
        (pushnew (stem:stem word) words :test #'string=)))
    words))

(defun search-articles (phrase)
  (let ((words (words phrase)))
    (when words
      (sort (copy-list (reduce (lambda (&optional a b)
                                 (intersection a b :test #'string=))
                               (mapcar #'articles-by-search-word words)))
            #'< :key (lambda (title)
                       (let ((title (canonicalize title)))
                        (loop for word in words
                              for weight from 0 by 100
                              thereis (awhen (search word title)
                                        (+ (* it 100) weight (length title)))
                              finally (return most-positive-fixnum))))))))

(defun paginate-article-summaries (start articles &optional (next-page-uri "?"))
  (let ((page-size 10)
        (start (or (parse-integer (or start "0") :junk-allowed t) 0)))
    (flet ((page-uri (page# label)
             #H[<span><a href="${next-page-uri}&start=${(* page# page-size)}">${label}</a></span>]))

      #H[<ol start="${(1+ start)}">]
      (loop for i from start below (min (+ start page-size) (length articles))
            do (pprint-article-summary-li (elt articles i) "<br />"))
      #H[</ol>
      <div id="paginator">
      <span>Результаты:</span>]
      (unless (= 0 start)
        (page-uri (ceiling (- start page-size) page-size) "&lt;"))
      (dotimes (page# (ceiling (length articles) page-size))
        (if (= start (* page# page-size))
            #H[<span>${(1+ page#)}</span>]
            (page-uri page# (1+ page#))))
      (unless (>= (+ start page-size) (length articles))
        (page-uri (ceiling (+ start page-size) page-size) "&gt;"))
      #H[</div>])))

(defpage /site/search "Результат поиска" (query start)
  #H[<h1>Результат поиска</h1>]
  (aif (search-articles query)
       (paginate-article-summaries start it #U?query={query})
       #H[Ничего не найдено]))
