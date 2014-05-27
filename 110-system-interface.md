第十一章 系统接口
=================

一个有用的Scheme程序经常需要与底层操作系统进行交互。

## 11.1 检查和删除文件

`file-exists?`会检查它的参数字符串是否是一个文件。`delete-file`接受一个文件名字符串作为参数并删除相应的文件。这些程序并不是Scheme标准的一部分，但是在大多数Scheme实现中都能找到它们。用这些过程操作目录（而不是文件）并不是很可靠。（用它们操作目录的结果与具体的Scheme实现有关。）

`file-or-directory-modify-seconds`过程接受一个文件名或目录名为参数，并返回这个目录或文件的最后修改时间。时间是从格林威治标准时间1970年1月1日0点开始记时的。例如：
```scheme
(file-or-directory-modify-seconds "hello.scm")
=>  893189629
```

假定`hello.scm`文件最后一次修改的时间是1998年4月21日的某个时间。

## 11.2 调用操作系统命令

`system`程序把它的参数字符串当作操作系统命令来执行 [1]。如果命令成功执行并返回0，则它会返回真，如果命令执行失败并返回某非0值，则它会返回假。命令产生的任何输出都会进入标准的输出。

```scheme
(system "ls")
;lists current directory

(define fname "spot")

(system (string-append "test -f " fname)) 
;tests if file `spot' exists

(system (string-append "rm -f " fname)) 
;removes `spot'
```
最后两个命令等价于：

```scheme
(file-exists? fname)

(delete-file fname)
```

### 11.3 环境变量

过程`getenv`返回操作系统环境变量的设定值，如：

```scheme
(getenv "HOME")
=>  "/home/dorai"

(getenv "SHELL")
=>  "/bin/bash"
```
------------------------------
[1] MzScheme在`process`库中提供了`system`过程。使用`(require (lib "process.ss"))`来加载这个库。
