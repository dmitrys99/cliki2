(in-package #:cliki2)
(in-readtable cliki2)

(defclass wiki-diff (diff:diff) ()
  (:default-initargs
   :window-class 'wiki-diff-window))

(defclass wiki-diff-window (diff:diff-window) ())

(defun choose-chunks (chunks a b c)
  (loop for chunk in chunks appending
       (let ((kind (diff:chunk-kind chunk))
             (lines (diff:chunk-lines chunk)))
         (cond ((or (eq kind :common) (eq kind a)) lines)
               ((eq kind b) (list (format nil "~{~&~A~}" lines)))
               ((eq kind c) (make-list (length lines)))))))

(defun compare-strings (original modified)
  (labels ((str2arr (str)
             (map 'simple-vector #'char-code str))
           (wrt (str out start end)
             (loop for i from start below end
                   for ch = (char str i) do
                   (if (char= ch #\Newline)
                       (write-line "<br />" out)
                       (write-char ch out))))
           (fmt (regions str offset-fun length-fun)
             (with-output-to-string (out)
               (loop for reg in regions
                     for modified-p = (typep reg 'diff:modified-diff-region)
                     for start = (funcall offset-fun reg)
                     for end = (+ start (funcall length-fun reg)) do
                     (progn (when modified-p
                              (princ "<span style=\"color:red;\">" out))
                            (wrt str out start end)
                            (when modified-p (princ "</span>" out)))))))
    (let ((rawdiff (diff:compute-raw-diff (str2arr original)
                                          (str2arr modified))))
      (values (fmt rawdiff original #'diff:original-start #'diff:original-length)
              (fmt rawdiff modified #'diff:modified-start #'diff:modified-length)))))

(defmethod diff:render-diff-window :before ((window wiki-diff-window) *html-stream*)
  #H[<tr>
  <td /><td class="diff-line-number">Строка ${(diff:original-start-line window)}:</td>
  <td /><td class="diff-line-number">Строка ${(diff:modified-start-line window)}:</td>
</tr>])

(defmethod diff:render-diff-window ((window wiki-diff-window) *html-stream*)
  (labels ((escape (x) (when x (escape-for-html x)))
           (td (line dash class style)
             (if line
                 #H[<td class="diff-marker">${dash}</td>
                    <td class="${class}" style="${style}">${line}</td>]
                 #H[<td class="diff-marker" /><td />]))
           (diff-line (original modified)
             (td original "-" "diff-deleteline" "background-color: #FFA;")
             (td modified "+" "diff-addline" "background-color: #CFC;")))
    (loop for original in (choose-chunks (diff:window-chunks window) :delete :replace :create)
          for modified in (choose-chunks (diff:window-chunks window) :create :insert :delete) do
         (let ((original (escape original))
               (modified (escape modified)))
           #H[<tr>]
           (if (and original modified)
               (if (string= original modified)
                   #H[<td class="diff-marker" style="height:4px;"/>
                      <td class="diff-context" style="background-color: #EEE;">${original}</td>
                      <td class="diff-marker" />
                      <td class="diff-context" style="background-color: #EEE;">${original}</td>]
                   (multiple-value-call #'diff-line
                     (compare-strings original modified)))
               (diff-line original modified))
           #H[</tr>]))))

(defun path-or-blank (revision)
  (if revision
      (revision-path revision)
      (wiki-path "empty_file")))

(defun unified-diff-body (oldr newr)
  (let ((diff (diff:format-diff-string 'diff:unified-diff
                                       (path-or-blank oldr)
                                       (revision-path newr))))
    (subseq diff (nth-value 1 (ppcre:scan ".*\\n.*?\\n" diff)))))

(defun revision-version-info-links (r)
  #H[Версия ] (pprint-revision-link r) #H[ (${ (edit-link r "править") })])

(defun render-unified-revision-diff (oldr newr)
  #H[<div style="font-family:monospace;"><br />--- ]
  (when oldr (revision-version-info-links oldr))
  #H[<br />+++ ] (revision-version-info-links newr)
  #H[<br /><pre>${(escape-for-html (unified-diff-body oldr newr))}</pre></div>])

(defun render-diff-table (oldr diffr maybe-undo-button?)
  #H[<div style="display:none;"><br />
  Unified format diff:] (render-unified-revision-diff oldr diffr)
  #H[Table format diff:
  </div>
  <table class="diff">
  <colgroup>
    <col class="diff-marker"> <col class="diff-content">
    <col class="diff-marker"> <col class="diff-content">
  </colgroup>
  <tbody>
    <tr>
      <th colspan="2">] (when oldr (revision-version-info-links oldr)) #H[</th>
      <th colspan="2">] (revision-version-info-links diffr)
      (when (and maybe-undo-button?
                 (eq diffr (latest-revision (find-article (parent-title diffr)))))
        #H[<form method="post" action="$(#/site/undo)">
        <input type="hidden" name="article" value="${ (parent-title oldr) }" />]
        (output-undo-button diffr)
        #H[</form>])
      #H[</th>
    </tr>
    ${(diff:format-diff-string 'wiki-diff
                               (path-or-blank oldr)
                               (revision-path diffr))}
  </tbody>
  </table>])

(defpage /site/compare-revisions () (article old diff)
  (let ((oldr  (find-revision article old))
        (diffr (find-revision article diff)))
    (unless (and oldr diffr)
      (error "Не могу найти указанную ревизию"))
    (when (> (revision-date oldr) (revision-date diffr))
      (rotatef oldr diffr))
    (setf *title*  #?"${ article } разница между ревизиями"
          *footer* (article-footer oldr))
    #H[<div class="centered"><h1><a class="internal" href="${ (article-link article) }">${article}</a></h1></div>]
    (render-diff-table oldr diffr t)))
