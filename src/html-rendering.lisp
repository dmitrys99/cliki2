(in-package #:cliki2)
(in-readtable cliki2)

(defvar *title*)
(defvar *footer* "")
(defvar *header* "")

(defun render-header ()
  #H[<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
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
                   <span>${(format-account-link *account*)}</span>
                   <div id="logout_button"><a href="$(#/site/logout)">Log out</a></div>
                 </div>]
              #H[<form method="post" action="$(#/site/login)">
                   <label for="name" class="hidden">Account name</label>
                   <input type="text" name="name" class="login_input" />
                   <label for= "password" class="hidden">Password</label>
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
        <li><a href="/Text%20Formatting">Text Formatting</a></li>
        <li><a href="$(#/site/tools)">Tools</a></li>
      </ul>
      <div id="search">
        <form action="$(#/site/search)">
          <label for="query" class="hidden">Search CLiki</label>
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
