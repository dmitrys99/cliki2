(in-package #:cliki2)
(in-readtable cliki2)

(defparameter *search-index* nil)

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
  (let ((index-data `(("id" . ,id) ("content" . ,content)))
        (doc (montezuma:get-document *search-index* id)))
    (if doc
        (montezuma:update *search-index* id index-data)
        (montezuma:add-document-to-index *search-index* index-data))))

(defun search-articles (query start &key (page-size 10))
  (let ((revisionables ()))
    (montezuma:each
     (montezuma:search *search-index*
                       (format nil "content:\"~A\"" (remove #\" query))
                       :num-docs page-size
                       :first-doc start)
     (lambda (doc)
       (push (store-object-with-id
              (parse-integer
               (montezuma:document-value
                (montezuma:get-document *search-index* (montezuma:doc doc))
                "id")))
             revisionables)))
    (reverse revisionables)))

(defpage /site/search "CLiki2: Search results" (query index)
  (let* ((start (or (parse-integer index :junk-allowed t) 0))
         (articles (search-articles query start)))
    #H[<h1>Search results</h1>]
    (if articles
        (progn
          #H[<ol start="${(1+ start)}">]
          (dolist (article articles)
            #H[<li><a href="/${(title article)}">${(title article)}</a>
            <div>${(article-description article)}</div>
            </li>])
          #H[</ol>

          <div id="paginator">
          <span>Result page:</span>
          <ul>
          {for $p in range( ceiling( $total / $pageSize))}
          <li>
          {if $p*$pageSize == $start}
          {$p + 1}
          {else}
          <a href="?query={$query |escapeUri}&start={$p*$pageSize |escapeUri}">{$p + 1}</a>
          {/if}
          </li>
          {/for}
          </ul>
          <div class="clear"></div>
          </div>])
        #H[No results found])))

