(in-package #:cliki2)
(in-readtable cliki2)

(defvar *title*)

(defun render-header ()
  #H[<html>
  <head>
    <title>${(format nil "CLiki~@[: ~A~]" *title*)}</title>
    <link  rel="alternate" type="application/rss+xml" title="recent changes" href="$(#/site/feed/rss.xml)">
    <link  rel="stylesheet" href="/static/css/style.css">
    <link  rel="stylesheet" href="/static/css/colorize.css">
  </head>

  <body>
    <div id="pageheader">
      <div id="header">
        <span id="logo">CL<span>iki</span></span>
        <span id="slogan">the common lisp wiki</span>
        <div id="login">]
          (if (session-value 'account)
              #H[<a href="${(link-to account)}">${(name account)}</a> <a href="$(#/logout)">Log out</a>]
              #H[<form method="post" action="$(#/site/do-login)">
                   <input type="text" name="name" title="login" class="login_input" />
                   <input type="password" name="password" class="login_input" />
                   <input type="submit" value="login" />
                   <a id="reset_password" href="$(#/site/reset-password)">Reset password</a>
                   <a id="register" href="$(#/site/register)">Register</a>
                 </form>])
          #H[
        </div>
      </div>
    </div>

    <div id="navbar">
      <ul>
        <li><a href="/">Home</a></li>
        <li><a href="$(#/site/recent-changes)">Recent Changes</a></li>
        <li><a href="/CLiki">About CLiki</a></li>
        <li><a href="/Text%20Formating">Text Formatting</a></li>
      </ul>
      <div id="search">
        <form action="$(#/site/search)">
          <input type="text" name="query" value="${(or (hunchentoot:get-parameter "query") "")}" />
          <input type="submit" value="search" />
        </form>
      </div>
    </div>
    <div id="content">])

(defun render-footer ()
  #H[</div></body></html>])

(defmacro render-page (title &body body)
  `(let* ((*title* ,title)
          (body (with-output-to-string (*html-stream*)
                  ,@body)))
     (with-output-to-string (*html-stream*)
       (render-header)
       (princ body *html-stream*)
       (render-footer))))
