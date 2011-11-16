(in-package #:cliki2)
(in-readtable cliki2)

(defvar *search-index* nil)

(defun close-search-index ()
  (when *search-index*
    (montezuma:close *search-index*)
    (setf *search-index* nil)))

(defun open-search-index ()
  (let ((dir (merge-pathnames "index/" *datadir*)))
    (close-search-index)
    (ensure-directories-exist dir)
    (setf *search-index* (make-instance 'montezuma:index :path dir))))

(defun index-document (id content)
  (let* ((id #?"${id}")
         (index-data `(("id" . ,id) ("content" . ,content)))
         (doc (montezuma:get-document *search-index* id)))
    (if doc
        (montezuma:update *search-index* id index-data)
        (montezuma:add-document-to-index *search-index* index-data))))

(defun search-articles (query start page-size)
  (let ((revisionables ())
        (documents (montezuma:search
                    *search-index*
                    (format nil "content:\"~A\"" (remove #\" query))
                    :num-docs page-size
                    :first-doc start)))
    (montezuma:each ;; montezuma sucks goat dick
     documents
     (lambda (doc)
       (push (store-object-with-id
              (parse-integer
               (montezuma:document-value
                (montezuma:get-document *search-index* (montezuma:doc doc))
                "id")))
             revisionables)))
    (values (reverse revisionables) (montezuma::total-hits documents))))

(defpage /site/search "CLiki: Search results" (query start)
  (let ((page-size 10)
        (start (or (parse-integer (or start "0") :junk-allowed t) 0)))
    (multiple-value-bind (articles total)
        (search-articles query start page-size)
      #H[<h1>Search results</h1>]
      (if articles
          (progn
            #H[<ol start="${(1+ start)}">]
            (dolist (article articles)
              (pprint-article-summary-li article "<br />"))
            #H[</ol>
            <div id="paginator">
            <span>Result page:</span>
            <ul>]
            (dotimes (p (ceiling total page-size))
              #H[<li>]
              (if (= start (* p page-size))
                  #H[${(1+ p)}]
                  #H[<a href="$(#U?query={query}&start={(* p page-size)})">${(1+ p)}</a></li>]))
            #H[</ul></div>])
          #H[No results found]))))

