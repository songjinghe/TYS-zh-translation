第十七章 CGI脚本
================

<font color="red">（警告：缺乏适当安全防护措施的CGI脚本可能会让您的网站陷入危险状态。本文中的脚本只是简单的样例而不保证在真实网站使用是安全的。）</font>

CGI脚本是驻留在Web服务器上的脚本，而且可以被客户端（浏览器）运行。客户端通过脚本的URL来访问脚本，就像访问普通页面一样。服务器识别出请求的URL是一个脚本，于是就运行该脚本。服务器如何识别特定的URL为脚本取决于服务器的管理员。在本文中我们假设脚本都存放在一个单独的文件夹，名为cgi-bin。因此，www.foo.org网站上的`testcgi.scm`脚本可以通过 http://www.foo.org/cgi-bin/testcgi.scm 来访问。

服务器以`nobody`用户的身份来运行脚本，不应当期望这个用户有`PATH`的环境变量或者该变量正确设置（这太主观了）。因此用Scheme编写的脚本的“引导行”会比我们在一般Scheme脚本中更加清楚才行。也就是说，下面这行代码：
```shell
":";exec mzscheme -r $0 "$@"
```
隐式的假设有一个特定的shell（如bash），而且设置好了`PATH`变量，而mzscheme程序在PATH的路径里。对于CGI脚本，我们需要多写一些：

```shell
#!/bin/sh
":";exec /usr/local/bin/mzscheme -r $0 "$@"
```

这样指定了shell和Scheme可执行文件的绝对路径。控制从shell交接给Scheme的过程和普通脚本一致。

## 17.1 例：显示环境变量

下面是一个Scheme编写的CGI脚本的示例，`testcgi.scm`。该文件会输出一些常用CGI环境变量的设置。这些信息作为一个新的，刚刚创建的页面返回给浏览器。返回的页面就是该CGI脚本向标准输出里写入的任何东西。这就是CGI脚本如何回应对它们的调用——通过返回给它们（客户端）一个新页面。

注意脚本首先输出下面这行：
```
content-type: text/plain 
```

后面跟一个空行。这是Web服务器提供页面服务的标准方式。这两行不会在页面上显示出来。它们只是提醒浏览器下面将发送的页面是纯文本（也就是非标记）文字。这样浏览器就会恰当的显示这个页面了。如果我们要发送的页面是用HTML标记的，`content-type`就是`text/html`。

下面是脚本`testcgi.scm`：
```scheme
#!/bin/sh
":";exec /usr/local/bin/mzscheme -r $0 "$@"

;Identify content-type as plain text.

(display "content-type: text/plain") (newline)
(newline)

;Generate a page with the requested info.  This is
;done by simply writing to standard output.

(for-each
 (lambda (env-var)
   (display env-var)
   (display " = ")
   (display (or (getenv env-var) ""))
   (newline))
 '("AUTH_TYPE"
   "CONTENT_LENGTH"
   "CONTENT_TYPE"
   "DOCUMENT_ROOT"
   "GATEWAY_INTERFACE"
   "HTTP_ACCEPT"
   "HTTP_REFERER" ; [sic]
   "HTTP_USER_AGENT"
   "PATH_INFO"
   "PATH_TRANSLATED"
   "QUERY_STRING"
   "REMOTE_ADDR"
   "REMOTE_HOST"
   "REMOTE_IDENT"
   "REMOTE_USER"
   "REQUEST_METHOD"
   "SCRIPT_NAME"
   "SERVER_NAME"
   "SERVER_PORT"
   "SERVER_PROTOCOL"
   "SERVER_SOFTWARE"))
```

`testcgi.scm`可以直接从浏览器上打开，URL是：

http://www.foo.org/cgi-bin/testcgi.scm

此外，`testcgi.scm`也可以放在HTML文件的链接中，这样可以直接点击，如：
```html
... To view some common CGI environment variables, click 
<a href="http://www.foo.org/cgi-bin/testcgi.scm">here</a>. 
... 
```

而一旦触发了`testcg.scm`，它就会生成一个包括环境变量设置的纯文本页面。下面是一个示例输出：
```
AUTH_TYPE = 
CONTENT_LENGTH = 
CONTENT_TYPE = 
DOCUMENT_ROOT = /home/httpd/html 
GATEWAY_INTERFACE = CGI/1.1 
HTTP_ACCEPT = image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */* 
HTTP_REFERER = 
HTTP_USER_AGENT = Mozilla/3.01Gold (X11; I; Linux 2.0.32 i586) 
PATH_INFO = 
PATH_TRANSLATED = 
QUERY_STRING = 
REMOTE_HOST = 127.0.0.1 
REMOTE_ADDR = 127.0.0.1 
REMOTE_IDENT = 
REMOTE_USER = 
REQUEST_METHOD = GET 
SCRIPT_NAME = /cgi-bin/testcgi.scm 
SERVER_NAME = localhost.localdomain 
SERVER_PORT = 80 
SERVER_PROTOCOL = HTTP/1.0 
SERVER_SOFTWARE = Apache/1.2.4
```

## 17.2 示例：显示选择的环境变量

`testcgi.scm`没有从用户获得任何输入。一个更专注的脚本会从用户那里获得一个环境变量，然后输出这个变量的设置，此外不返回任何东西。为了做这个，我们需要一个机制把参数传递给CGI脚本。HTML的表单提供了这种功能。下面是完成这个目标的一个简单的HTML页面：
```html
<html> 
<head> 
<title>Form for checking environment variables</title> 
</head> 
<body> 
 
<form method=get  
      action="http://www.foo.org/cgi-bin/testcgi2.scm"> 
Enter environment variable: <input type=text name=envvar size=30> 
<p> 
 
<input type=submit> 
</form> 
 
</body> 
</html>
```

用户在文本框中输入希望的环境变量（如`GATEWAY_INTERFACE`）并点击提交按钮。这会把所有表单里的信息——这里，参数`envvar`的值是`GATEWAY_INTERFACE`——收集并发送到该表单对应的CGI脚本即`testcgi2.scm`。这些信息可以用两种方法来发送：
1. 如果表单的`method`属性是`GET`（默认），那么这些信息通过环境变量`QUERY_STRING`来传递给脚本
2. 如果表单的`method`属性是`POST`，那么这些信息会在稍后发送到CGI脚本的标准输入中。

我们的表单使用`QUERY_STRING`的方式。

把信息从`QUERY_STRING`中提取出来并输出相应的页面是`testcgi2.scm`脚本的事情。

发给CGI脚本的信息，不论通过环境变量还是通过标准输入，都被格式化为一串“参数/值”的键值对。键值对之间用`&`字符分隔开。每个键值对中参数的名字在前面而且与参数值之间用`=`分开。这种情况下，只有一个键值对，即`envvar=GATEWAY_INTERFACE`。

下面是`testcgi2.scm`脚本：
```scheme
#!/bin/sh
":";exec /usr/local/bin/mzscheme -r $0 "$@"

(display "content-type: text/plain") (newline)
(newline)

;string-index returns the leftmost index in string s
;that has character c

(define string-index
  (lambda (s c)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond ((>= i n) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (+ i 1))))))))

;split breaks string s into substrings separated by character c

(define split
  (lambda (c s)
    (let loop ((s s))
      (if (string=? s "") '()
          (let ((i (string-index s c)))
            (if i (cons (substring s 0 i)
                        (loop (substring s (+ i 1)
                                         (string-length s))))
                (list s)))))))

(define args
  (map (lambda (par-arg)
         (split #\= par-arg))
       (split #\& (getenv "QUERY_STRING"))))

(define envvar (cadr (assoc "envvar" args)))

(display envvar)
(display " = ")
(display (getenv envvar))

(newline)
```

注意辅助过程`split`把`QUERY_STRING`用`&`分隔为键值对并进一步用`=`把参数名和参数值分开。（如果我们是用`POST`方法，我们需要把参数名和参数值从标准输入中提取出来。）

`<input type=text>`和`<input type=submit>`是HTML表单的两个不同的输入标签。参考文献27来查看全部。

## 17.3 CGI脚本相关问题（utilities）

在上面的例子中，参数名和参数值都假设没有包含`=`或`&`字符。通常情况他们会包含。为了适应这种字符，而不会不小心把他们当成分割符，CGI参数传递机制要求所有除了字母、数字和下划线以外的“特殊”字符都要编码进行传输。空格被编码为`+`，其他的特殊字符被编码为3个字符的序列，包括一个`%`字符紧跟着这个字符的16进制码。因此，`20% + 30% = 50%, &c.`会被编码为：
```
20%25+%2b+30%25+%3d+50%25%2c+%26c%2e
```
（空格变成`+`；`%`变为`%25`；`+`变为`%2b`；`=`变为`%3d`；`,`变为`%2c`；`&`变为`%26`；`.`变为`%2e`）

除了把获得和解码表单的代码写在每个CGI脚本中，把这些函数放在一个库文件`cgi.scm`中。这样`testcgi2.scm`的代码写起来更紧凑：

```scheme
#!/bin/sh
":";exec /usr/local/bin/mzscheme -r $0 "$@"

;Load the cgi utilities

(load-relatve "cgi.scm")

(display "content-type: text/plain") (newline)
(newline)

;Read the data input via the form

(parse-form-data)

;Get the envvar parameter

(define envvar (form-data-get/1 "envvar"))

;Display the value of the envvar

(display envvar)
(display " = ")
(display (getenv envvar))
(newline)
```

这个简短一些的CGI脚本用了两个定义在`cgi.scm`中的通用过程。`parse-form-data`过程读取用户通过表单提交的数据，包括参数和值。

`form-data-get/1`找到与特定参数关联的值。

`cgi.scm`定义了一个全局表叫`*form-data-table*`来存放表单数据。

```scheme
;Load our table definitions

(load-relative "table.scm")

;Define the *form-data-table*

(define *form-data-table* (make-table 'equ string=?))
```

使用诸如`parse-form-data`等通用过程的一个好处是我们可以不用管用户是用那种方式（get或post）提交的数据。
```scheme
(define parse-form-data
  (lambda ()
    ((if (string-ci=? (or (getenv "REQUEST_METHOD") "GET") "GET")
         parse-form-data-using-query-string
         parse-form-data-using-stdin))))
```

环境变量`REQUEST_METHOD`表示使用那种方式传送表单数据。如果方法是`GET`，那么表单数据被作为字符串通过另一个环境变量`QUERY_STRING`传输。辅助过程`parse-form-data-using-query-string`用来拆散`QUERY_STRING`：
```scheme
(define parse-form-data-using-query-string
  (lambda ()
    (let ((query-string (or (getenv "QUERY_STRING") "")))
      (for-each
       (lambda (par=arg)
         (let ((par/arg (split #\= par=arg)))
           (let ((par (url-decode (car par/arg)))
                 (arg (url-decode (cadr par/arg))))
             (table-put! 
              *form-data-table* par
              (cons arg 
                    (table-get *form-data-table* par '()))))))
       (split #\& query-string)))))
```

辅助过程`split`，和它的辅助过程`string-index`，在第二节中定义过了。正如之前提到的，输入的表单数据是一串用`&`分割的键值对。每个键值对中先是参数名，然后是一个`=`号，后面是值。每个键值对都放到一个全局的表`*form-data-table*`里。

每个参数名和参数值都被编码了，所以我们需要用`url-decode`过程来解码得到它们的真实表示。
```scheme
(define url-decode
  (lambda (s)
    (let ((s (string->list s)))
      (list->string
       (let loop ((s s))
         (if (null? s) '()
             (let ((a (car s)) (d (cdr s)))
               (case a
                 ((#\+) (cons #\space (loop d)))
                 ((#\%) (cons (hex->char (car d) (cadr d))
                              (loop (cddr d))))
                 (else (cons a (loop d)))))))))))
```

`+`被转换为空格，通过过程`hex->char`,`%xy`这种形式的词也被转换为其ascii编码是十六进制数`xy`的字符。
```scheme
(define hex->char
  (lambda (x y)
    (integer->char
     (string->number (string x y) 16))))
```

我们还需要一个处理POST方法传输数据的程序。辅助过程`parse-form-data-using-stdin`就是做这个的。

```scheme
(define parse-form-data-using-stdin
  (lambda ()
    (let* ((content-length (getenv "CONTENT_LENGTH"))
           (content-length
             (if content-length
                 (string->number content-length) 0))
           (i 0))
      (let par-loop ((par '()))
        (let ((c (read-char)))
          (set! i (+ i 1))
          (if (or (> i content-length)
                  (eof-object? c) (char=? c #\=))
              (let arg-loop ((arg '()))
                (let ((c (read-char)))
                  (set! i (+ i 1))
                  (if (or (> i content-length)
                          (eof-object? c) (char=? c #\&))
                      (let ((par (url-decode
                                   (list->string
                                     (reverse! par))))
                            (arg (url-decode
                                   (list->string
                                     (reverse! arg)))))
                        (table-put! *form-data-table* par
                          (cons arg (table-get *form-data-table*
                                      par '())))
                        (unless (or (> i content-length)
                                    (eof-object? c))
                          (par-loop '())))
                      (arg-loop (cons c arg)))))
              (par-loop (cons c par))))))))
```

POST方法通过脚本的标准输入传输表单数据。传输的字符数放在环境变量`CONTENT_LENGTH`里。`parse-form-data-using-stdin`从标准输入读取对应的字符，也像之前那样设置`*form-data-table*`，保证参数名和值被解码。

剩下就是从`*form-data-table*`取回特定参数的值。主要这个这个表中每个参数都关联着一个列表，这是为了适应一个参数多个值的情况。`form-data-get`取回一个参数对应的所有值。如果只有一个值，就返回这个值。
```scheme
(define form-data-get
  (lambda (k)
    (table-get *form-data-table* k '())))
```

`form-data-get/1`返回一个参数的第一个（或最重要的）值。
```scheme
(define form-data-get/1
  (lambda (k . default)
    (let ((vv (form-data-get k)))
      (cond ((pair? vv) (car vv))
            ((pair? default) (car default))
            (else "")))))
```

在我们目前的例子当中，CGI脚本都是生成纯文本，通常我们希望生成一个HTML页面。把CGI脚本和HTML表单结合起来生成一系列带有表单的HTML页面是很常见的。把不同方法的响应代码放在一个CGI脚本里也是很常见的。不论任何情况，有一些辅助过程把字符串输出为HTML格式（即，把HTML特殊字符进行编码））都是很有帮助的：

```scheme
(define display-html
  (lambda (s . o)
    (let ((o (if (null? o) (current-output-port)
                 (car o))))
      (let ((n (string-length s)))
        (let loop ((i 0))
          (unless (>= i n)
            (let ((c (string-ref s i)))
              (display
               (case c
                 ((#\<) "&lt;")
                 ((#\>) "&gt;")
                 ((#\") "&quot;")
                 ((#\&) "&amp;")
                 (else c)) o)
              (loop (+ i 1)))))))))
```

## 17.4 一个CGI做的计算器


下面是一个CGI计算器的脚本，`cgicalc.scm`，使用了Scheme任意精度的算术功能。

```scheme
#!/bin/sh
":";exec /usr/local/bin/mzscheme -r $0

;Load the CGI utilities
(load-relative "cgi.scm")

(define uhoh #f)

(define calc-eval
  (lambda (e)
    (if (pair? e)
        (apply (ensure-operator (car e))
               (map calc-eval (cdr e)))
        (ensure-number e))))

(define ensure-operator
  (lambda (e)
    (case e
      ((+) +)
      ((-) -)
      ((*) *)
      ((/) /)
      ((**) expt)
      (else (uhoh "unpermitted operator")))))

(define ensure-number
  (lambda (e)
    (if (number? e) e
        (uhoh "non-number"))))

(define print-form
  (lambda ()
    (display "<form action=\"")
    (display (getenv "SCRIPT_NAME"))
    (display "\">
  Enter arithmetic expression:<br>
  <input type=textarea name=arithexp><p>
  <input type=submit value=\"Evaluate\">
  <input type=reset value=\"Clear\">
</form>")))

(define print-page-begin
  (lambda ()
    (display "content-type: text/html

<html>
  <head>
    <title>A Scheme Calculator</title>
  </head>
  <body>")))

(define print-page-end
  (lambda ()
    (display "</body>
</html>")))

(parse-form-data)

(print-page-begin)

(let ((e (form-data-get "arithexp")))
  (unless (null? e)
    (let ((e1 (car e)))
      (display-html e1)
      (display "<p>
  =&gt;&nbsp;&nbsp;")
      (display-html
       (call/cc
        (lambda (k)
          (set! uhoh
                (lambda (s)
                  (k (string-append "Error: " s))))
          (number->string
           (calc-eval (read (open-input-string (car e))))))))
      (display "<p>"))))

(print-form)
(print-page-end)
```
