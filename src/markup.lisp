(in-package #:cliki2)
(in-readtable cliki2)

(sanitize:define-sanitize-mode +links-only+
    :elements ("a")
    :attributes (("a" . ("href" "class")))
    :protocols  (("a" . (("href" . (:ftp :http :https :mailto :relative))))))

(sanitize:define-sanitize-mode +cliki-tags+
    :elements ("a" "blockquote"
               "dd" "dl" "dt"
               "h1" "h2" "h3" "h4" "h5" "h6" "hgroup"
               "pre" "code"
               "img"
               "table" "tbody" "td" "tfoot" "th" "thead" "tr" "col" "colgroup"
               "ul" "ol" "li"
               "b" "em" "i" "small" "strike" "strong"
               ;; what are these?
               "ins" "kbd"  "mark"
               "caption" "cite"
               "figcaption" "figure" "del"
               "sub" "sup" "bdo" "dfn"
               "q" "rp" "rt" "ruby" "s" "samp"
               "time" "var" "wbr" "u" "abbr")
    :attributes ((:all         . ("dir" "lang" "title" "class"))
                 ("a"          . ("href"))
                 ("blockquote" . ("cite"))
                 ("col"        . ("span" "width"))
                 ("colgroup"   . ("span" "width"))
                 ("del"        . ("cite" "datetime"))
                 ("img"        . ("align" "alt" "height" "src" "width"))
                 ("ins"        . ("cite" "datetime"))
                 ("ol"         . ("start" "reversed" "type"))
                 ("ul"         . ("type"))
                 ("code"       . ("lang"))
                 ("q"          . ("cite"))
                 ("table"      . ("summary" "width"))
                 ("td"         . ("abbr" "axis" "colspan" "rowspan" "width"))
                 ("th"         . ("abbr" "axis" "colspan" "rowspan" "scope" "width")))

    :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
                ("blockquote"  . (("cite" . (:http :https :relative))))
                ("del"         . (("cite" . (:http :https :relative))))
                ("img"         . (("src"  . (:http :https :relative))))
                ("ins"         . (("cite" . (:http :https :relative))))
                ("q"           . (("cite" . (:http :https :relative))))))

(defun generate-html-from-markup (markup)
  (princ
   (parse-cliki-markup (sanitize:clean markup +cliki-tags+))
   *html-stream*))

(defun parse-cliki-markup (markup)
  (loop for prefix in '("_" "_H" "\\*" "\\/" "_P")
     for formatter in '(pprint-article-link format-hyperspec-link pprint-category-link format-category-list format-package-link)
     do (setf markup (process-cliki-rule markup prefix formatter)))
  (ppcre:regex-replace-all "\\n\\n" (colorize-code markup) "<p>"))

(defun process-cliki-rule (markup prefix formatter)
  (ppcre:regex-replace-all #?/${prefix}\((.*?)\)/
                           markup
                           (lambda (match r1)
                             (declare (ignore match))
                             (with-output-to-string (*html-stream*)
                              (funcall formatter r1)))
                           :simple-calls t))

(defmethod pprint-article-summary-li ((article article) separator)
  #H[<li>] (pprint-article-link (title article)) #H[ ${separator}
  ${(sanitize:clean (with-output-to-string (*html-stream*)
                      (generate-html-from-markup (article-description article)))
                    +links-only+)}
  </li>])

(defun format-category-list (category) ;; /(
  #H[<ul>] (dolist (article (sort (copy-list
                                   (articles-with-category
                                    (category-keyword category)))
                                  #'string< :key 'canonical-title))
             (pprint-article-summary-li article "-"))
  #H[</ul>])

(defun format-hyperspec-link (symbol) ;; _H(
  #H[<a href="${(clhs-lookup:spec-lookup symbol)}" class="hyperspec">${symbol}</a>])

(defun format-package-link (link) ;; _P(
  #H[<a href="${link}" class="download">Download ASDF package from ${link}</a>])

;;;; do something with code-block

(let ((supported-langs (sort (mapcar (lambda (x)
                                       (symbol-name (car x)))
                                     colorize::*coloring-types*)
                             #'> :key #'length)))
  (defun colorize-code (markup)
    (ppcre:regex-replace-all
     "<code(.*?)?>(.*?)</code>" markup
     (lambda (match maybe-lang code)
       (declare (ignore match))
       (let ((lang (loop for lang in supported-langs
                         when (search lang maybe-lang :test #'char-equal)
                         return (find-symbol lang :keyword))))
         (if lang
             #?[<div class="code">${(colorize::html-colorization lang code)}</div>]
             #?[<code>${code}</code>])))
     :simple-calls t)))
