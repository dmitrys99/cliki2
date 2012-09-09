(in-package #:cliki2)
(in-readtable cliki2)

(defvar *title*  "")
(defvar *footer* "")
(defvar *header* "")

(defun render-header ()
  #H[<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>${(format nil "~A~@[: ~A~]" (wiki-name *wiki*) *title*)}</title>
    ${*header*}
    <link  rel="stylesheet" href="/static/css/style.css">
    <link  rel="stylesheet" href="/static/css/colorize.css">
  </head>

  <body>
    <span class="hidden">${ (wiki-name *wiki*) } - ${*title*}</span>
    <div id="content"><div id="content-area">])

(defun render-footer ()
  #H[</div>
  <div id="footer" class="buttonbar"><ul>${*footer*}</ul></div>
  </div>
  <div id="header-buttons" class="buttonbar">
    <ul>
      <li><a href="/">Home</a></li>
      <li><a href="$(#/site/recent-changes)">Recent Changes</a></li>
      <li><a href="/${(wiki-name *wiki*)}">About</a></li>
      <li><a href="/Text%20Formatting">Text Formatting</a></li>
      <li><a href="$(#/site/tools)">Tools</a></li>
    </ul>
    <div id="search">
      <form action="$(#/site/search)">
        <label for="search_query" class="hidden">Search ${(wiki-name *wiki*)}</label>
        <input type="text" name="query" id="search_query" value="${(or (get-parameter "query") "")}" />
        <input type="submit" value="search" />
      </form>
    </div>
  </div>
  <div id="pageheader">
    <div id="header">
      <span id="logo">${(wiki-name *wiki*)}</span>
      <span id="slogan">${(description *wiki*)}</span>
      <div id="login">]
        (if *account*
            #H[<div id="logout">
                 <span>${(account-link (account-name *account*))}</span>
                 <div id="logout_button"><a href="$(#/site/logout)">Log out</a></div>
               </div>]
            #H[<form method="post" action="$(#/site/login)">
                 <label for="login_name" class="hidden">Account name</label>
                 <input type="text" name="name" id="login_name" class="login_input" />
                 <label for= "login_password" class="hidden">Password</label>
                 <input type="password" name="password" id="login_password" class="login_input" />
                 <input type="submit" name="login" value="login" id="login_submit" /><br />
                 <div id="register"><a href="$(#/site/register)">register</a></div>
                 <input type="submit" name="reset-pw" value="reset password" id="reset_pw" />
               </form>])
        #H[
      </div>
    </div>
  </div>
  </body></html>])

(defmacro render-page (title &body body)
  `(let* ((*header* *header*)
          (*footer* *footer*)
          (*title*  *title*) ;; in case ,title does setf
          (*title*  ,title)
          (body (with-output-to-string (*html-stream*)
                  ,@body)))
     (with-output-to-string (*html-stream*)
       (render-header)
       (princ body *html-stream*)
       (render-footer))))
