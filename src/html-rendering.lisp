(in-package #:cliki2)
(in-readtable cliki2)

(defvar *title*)

(defun render-header ()
  #H[<html>
        <head>
            <title>${(format nil "CLiki~@[: ~A~]" *title*)}</title>
            <link  rel="alternate" type="application/rss+xml" title="recent changes" href="$(#/feed/rss.xml)">
            <link  rel="stylesheet" href="/static/css/style.css">
            <link  rel="stylesheet" href="/static/css/colorize.css">
        </head>

        <body>
            <div id="pageheader">
                <div id="header">
                    <a title="CLiki: the common lisp wiki" id="logo" href="/">CL<span>iki</span></a>
                    <span id="slogan">the common lisp wiki</span>
                    <div id="login">
]
  (if (session-value 'account)
      #H[<a href="${(link-to account)}">${(name account)}</a> <a href="$(#/logout)">Log out</a>]
      #H[<form method="post" action="$(#/site/do-login)">
           <input type="text" name="name" title="login" />
           <input type="password" name="password" />
           <input type="submit" value="enter" />
           <a id="register" href="$(#/site/register)">Register</a>
           <a id="forgot_password" href="$(#/site/forgot-password)">Forgot password</a>
         </form>])
  #H[</div> </div> </div>
            <div id="navbar">
                <ul>
                    <li class="active"><a href="/">Home</a></li>
                    <li><a href="$(#/site/recent-changes)">Recent Changes</a></li>
                    <li><a href="/CLiki">About CLiki</a></li>
                    <li><a href="/Text%20Formating">Text Formatting</a></li>
                </ul>
                <div id="search">
                    <form action="$(#/site/search)">
                        <input type="text" name="query" value="${(hunchentoot:get-parameter "query")}" />
                        <input type="submit" value="search" />
                    </form>
                </div>
            </div>
])

(defun render-footer ())

(defmacro render-page (title &body body)
  `(let* ((*title* ,title)
          (body (with-output-to-string (*html-stream*)
                  ,@body)))
     (with-output-to-string (*html-stream*)
       (render-header)
       (princ body *html-stream*)
       (render-footer))))
