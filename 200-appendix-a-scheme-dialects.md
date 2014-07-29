附录 A Scheme方言
====================

所有主要的Scheme方言都实现了R5RS规范。如果只使用R5RS中规定的功能，我们就能写出在这些方言中都能正常运行的代码。然而R5RS可能是为了更好的统一性，或是由于不可避免的系统依赖，在一些通用编程中无法忽略的重要问题上没有给出标准。因此这些Scheme方言不得不用一种特殊的非标准手段来解决这些问题。

本书使用了Scheme的MzScheme方言，因此也使用了一些非标准的特性。以下是本书中所有非标准的、依赖于MzScheme提供的特性：

- 命令行（包括打开一个侦听会话以及Shell脚本）
- `define-macro`
- `delete-file`
- `file-exists?`
- `file‑or‑directory‑modify‑seconds`
- `fluid‑let`
- `gensym`
- `getenv`
- `get‑output‑string`
- `load‑relative`
- `open‑input‑string`
- `open‑output‑string`
- `read‑line`
- `reverse!`
- `system`
- `unless`
- `when`

以上这些命令中除了`define-macro`和`system`外都是在MzScheme的默认环境中就有的。而这两个缺少的则可以在MzScheme的标准库中找到，通过以下方式显式地加载。

```scheme
(require (lib "defmacro.ss")) ;provides define-macro
(require (lib "process.ss"))  ;provides system
```

另外还可以把这两段代码放在MzScheme的初始化文件中（在Unix系统下是用户家目录下的`.mzschemerc`文件）。

一些非标准的特性（如`file-exists?`和`delete-file`）事实上在很多Scheme实现中已经是“标准”的特性了。另一些特性（如`when`和`unless`）或多或少有种“插件”式的定义（在本书中给出），因此可以在任何不具备这些过程的Scheme中加载。其他的需要针对每种方言来定义（如`load-relative`）。

本章描述了如何给你使用的Scheme方言加上本书中用到的这些非标准特性。想要了解更多关于你使用的Scheme方言，请参考其实现者提供的文档（附录E）。

## A.1 调用和初始化文件

很多Scheme方言就像MzScheme一样都会从用户的家目录中载入初始化文件。我们可以把非标准功能的定义都放到这个初始化文件中，这样非常方便。比如，非标准过程`file-or-directory-modify-seconds`可以添加到Guile语言中，只要把下面的代码放到Guile的初始化文件（`~/.guile`）中即可：

```scheme
(define file-or-directory-modify-seconds
  (lambda (f)
    (vector-ref (stat f) 9)))
```

另外，不同的Scheme方言有他们自己的不同的命令来启动对应的侦听器。下面的表格列出了不同Scheme方言对应的启动命令和初始化文件位置：

<table><tr><td><span style="margin-left: 2em"> </span><em>Dialect name</em>  </td><td>   <em>Command</em>  </td><td>   <em>Init&nbsp;file</em>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Bigloo  </td><td>   <code class=verbatim>bigloo</code>  </td><td>   <code class=verbatim>~/.bigloorc</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Chicken  </td><td>   <code class=verbatim>csi</code>  </td><td>   <code class=verbatim>~/.csirc</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Gambit  </td><td>   <code class=verbatim>gsi</code>  </td><td>   <code class=verbatim>~/gambc.scm</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Gauche  </td><td>   <code class=verbatim>gosh</code>  </td><td>   <code class=verbatim>~/.gaucherc</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Guile  </td><td>   <code class=verbatim>guile</code>  </td><td>   <code class=verbatim>~/.guile</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>Kawa  </td><td>   <code class=verbatim>kawa</code>  </td><td>   <code class=verbatim>~/.kawarc.scm</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>MIT Scheme (Unix)  </td><td>   <code class=verbatim>scheme</code>  </td><td>   <code class=verbatim>~/.scheme.init</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>MIT Scheme (Win)  </td><td>   <code class=verbatim>scheme</code>  </td><td>   <code class=verbatim>~/scheme.ini</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>MzScheme (Unix, Mac OS X)  </td><td>   <code class=verbatim>mzscheme</code>  </td><td>    <code class=verbatim>~/.mzschemerc</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>MzScheme (Win, Mac OS Classic)  </td><td>   <code class=verbatim>mzscheme</code>  </td><td>   <code class=verbatim>~/mzschemerc.ss</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>SCM  </td><td>   <code class=verbatim>scm</code>  </td><td>   <code class=verbatim>~/ScmInit.scm</code>  </td></tr>
<tr><td><span style="margin-left: 2em"> </span>STk  </td><td>   <code class=verbatim>snow</code>  </td><td>   <code class=verbatim>~/.stkrc</code>  </td></tr>
</table>

## A.2 Shell脚本

使用Guile编写的Shell脚本的初始行差不多应该是：

```shell
":";exec guile -s $0 "$@"
```

在Guile脚本中，调用过程`(command-line)`会以列表的形式返回脚本的名称和参数。如果只需要参数，只需要获得列表的`cdr`部分即可。

用Gauche编写的Shell脚本以：

```shell
":"; exec gosh -- $0 "$@"
```

开头。在脚本中变量`*argv*`中保存着脚本的参数列表。

用SCM编写的Shell脚本以：

```shell
":";exec scm -l $0 "$@"
```

开头。脚本中变量`*argv*`保存着一个列表，列表中包括Scheme可执行文件的名称，脚本的名称，`-l`这个选项，还有脚本的参数。如果只需要参数，对列表执行`cdddr`即可。

STk的Shell脚本以：

```shell
":";exec snow -f $0 "$@"
```

开头。在脚本中变量`*argv*`中保存着脚本的参数列表。

## A.3 `define-macro`

本文中使用的`define-macro`宏在Scheme的很多方言如Bigloo，Chicken，Gambit，Gauche，Guile，MzScheme和Pocket中都有定义。在其他Scheme方言中定义宏的方式基本上是相同的。本节将指出其他Scheme方言是如何表示如下一段代码片段的：

```scheme
(define-macro MACRO-NAME
  (lambda MACRO-ARGS
    MACRO-BODY ...))
```

在MIT Scheme第7.7.1或更高版本中，上述代码被写为：

```scheme
(define-syntax MACRO-NAME
  (rsc-macro-transformer
    (let ((xfmr (lambda MACRO-ARGS MACRO-BODY ...)))
      (lambda (e r)
        (apply xfmr (cdr e))))))
```

在老版本的MIT Scheme中：
```scheme
(syntax-table-define system-global-syntax-table 'MACRO-NAME
  (macro MACRO-ARGS
    MACRO-BODY ...))
```

在SCM和Kawa中：
```scheme
(defmacro MACRO-NAME MACRO-ARGS
  MACRO-BODY ...)
```

在STk中：
```scheme
(define-macro (MACRO-NAME . MACRO-ARGS)
  MACRO-BODY ...)
```


## A.4 `load-relative`

过程`load-relative`可以在Guile中如下定义：
```scheme
(define load-relative
  (lambda (f)
    (let* ((n (string-length f))
           (full-pathname?
             (and (> n 0)
                  (let ((c0 (string-ref f 0)))
                    (or (char=? c0 #\/)
                        (char=? c0 #\~))))))
      (basic-load
        (if full-pathname? f
            (let ((clp (current-load-port)))
              (if clp
                  (string-append
                    (dirname (port-filename clp)) "/" f)
                  f)))))))
```

在SCM中可以这样写：

```scheme
(define load-relative
  (lambda (f)
    (let* ((n (string-length f))
           (full-pathname?
            (and (> n 0)
                 (let ((c0 (string-ref f 0)))
                   (or (char=? c0 #\/)
                       (char=? c0 #\~))))))
    (load (if (and *load-pathname* full-pathname?)
              (in-vicinity (program-vicinity) f)
              f)))))
```

对于STk，下面的`load-relative`过程仅在没有使用`load`过程时生效：

```scheme
(define *load-pathname* #f)

(define stk%load load)

(define load-relative
  (lambda (f)
    (fluid-let ((*load-pathname*
                  (if (not *load-pathname*) f
                      (let* ((n (string-length f))
                             (full-pathname?
                               (and (> n 0)
                                    (let ((c0 (string-ref f 0)))
                                      (or (char=? c0 #\/)
                                          (char=? c0 #\~))))))
                        (if full-pathname? f
                            (string-append
                              (dirname *load-pathname*)
                              "/" f))))))
      (stk%load *load-pathname*))))

(define load
  (lambda (f)
    (error "Don't use load.  Use load-relative instead.")))
```


----------------------------
我们使用`~/filename`表示用户家目录中被调用的文件。
