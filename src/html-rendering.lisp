(in-package #:cliki2)
(in-readtable cliki2)

(defvar *title*)
(defvar *footer* "")
(defvar *header* "")

(defun render-header ()
  #H[<html>
  <head>
    <title>${(format nil "CLiki~@[: ~A~]" *title*)}</title>
    ${*header*}
    <link  rel="stylesheet" href="/static/css/style.css">
    <link  rel="stylesheet" href="/static/css/colorize.css">
  </head>

  <body>
    <div id="pageheader">
      <div id="header">
        <span id="logo">CL<span>iki</span></span>
        <span id="slogan">the common lisp wiki</span>
        <div id="login">]
          (if *account*
              #H[<div id="logout">
                   <a href="${(link-to *account*)}">${(name *account*)}</a>
                   <div id="logout_button"><a href="$(#/site/logout)">Log out</a></div>
                 </div>]
              #H[<form method="post" action="$(#/site/login)">
                   <input type="text" name="name" title="login" class="login_input" />
                   <input type="password" name="password" class="login_input" />
                   <input type="submit" name="login" value="login" id="login_submit"/><br />
                   <input type="submit" name="reset-pw" value="reset password" id="reset_pw" />
                   <div id="register"><a href="$(#/site/register)">register</a></div>
                 </form>])
          #H[
        </div>
      </div>
    </div>

    <div class="buttonbar">
      <ul>
        <li><a href="/">Home</a></li>
        <li><a href="$(#/site/recent-changes)">Recent Changes</a></li>
        <li><a href="/CLiki">About CLiki</a></li>
        <li><a href="/Text%20Formating">Text Formatting</a></li>
      </ul>
      <div id="search">
        <form action="$(#/site/search)">
          <input type="text" name="query" value="${(or (get-parameter "query") "")}" />
          <input type="submit" value="search" />
        </form>
      </div>
    </div>
    <div id="content">])

(defun render-footer ()
  #H[</div><div id="footer" class="buttonbar"><ul>${*footer*}</ul></div></body></html>])

(defmacro render-page (title &body body)
  `(let* ((*title* ,title)
          (*footer* "")
          (body (with-output-to-string (*html-stream*)
                  ,@body)))
     (with-output-to-string (*html-stream*)
       (render-header)
       (princ body *html-stream*)
       (render-footer))))

(defmethod acceptor-status-message :around ((acceptor easy-acceptor) status-code &key &allow-other-keys) ;; blah, hunchentoot 1.2 is annoying
  (if (equal status-code 404)
      (with-account
        (render-page "Article not found"
          #H[<h1>Cliki2 does not have an article with this exact name</h1>
         <a href="$(#/site/edit-article?title={(guess-article-name)})">Create</a>]))
      (call-next-method)))
