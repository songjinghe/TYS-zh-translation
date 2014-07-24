第十六章 命令行脚本
===================

如果能把我们想做的东西写到一个文件或脚本中，并且像执行其他操作系统命令一样执行的话通常会非常方便。一些重量级的程序通常以脚本的形式提供接口，用户可以经常编写他们自己的脚本或修改已有的脚本来满足特定的需求。毫无疑问大部分的编程任务都以脚本的形式来执行。对于很多用户而言，这是他们唯一会做的编程了。

Unix或DOS等操作系统（以及Windows系统提供的命令行接口）都提供了脚本的机制。但是这些脚本语言都相当的不成熟。通常一个脚本就是一串可以在命令行上输入的命令。这样用户可以免于每次用这些命令（或相似的命令）都重新输入。某些脚本语言包含一些简单的编程功能如条件语句和循环，但这就是所有的了。这对于简单的程序是足够了。但是当脚本越来越大，要求越来越高——大部分情况都是如此——人们通常会觉得需要一些功能全面的编程语言。包含足够操作系统接口的Scheme语言让脚本编写变得简单而可维护。

这一节会描述如何在Scheme中编写脚本。由于Scheme有太多方言一节太多实现不同的实现方法，我们专注于使用MzScheme方言，附录A中讲解了如果用其他方言需要有哪些修改。我们现在也专门讲解Unix操作系统。附录B里讨论了DOS系统需要注意的问题。

## 16.1 再来一次Hello，World！

我们现在来创建一个Scheme脚本来对世界说hello。这对于通常的脚本语言也算不上什么难事。然而，为了后期上道编写更复杂的脚本，我们必须理解如何用Scheme来编写这个HelloWorld。首先，一个通常的Unix版的HelloWorld是一个文件，里面的内容如下：
```shell
echo Hello, World!
```
这里使用了命令`echo`，这个脚本可以被命名为`hello`，使用下面的命令使之可执行：
```shell
chmod +x hello
```
然后把它放在`PATH`环境变量中的任意一个目录下。然后任何时候从命令行输入
```shell
hello
```
就会输出上面的问候。

Scheme的hello脚本也会用Scheme产生相同的输出（见下面的脚本），但是我们得做点什么，让操作系统知道它应该用Scheme来分析文件中的命令，而不是用它默认的脚本语言。Scheme的脚本文件，有命名为hello，内容如下：
```scheme
":"; exec mzscheme -r $0 "$@"
 
(display "Hello, World!")
(newline))
```
除了第一行以外都是Scheme代码。然而第一行就是把这些代码指定为“脚本”的神奇之处。当用户在Unix命令行上输入`hello`的时候，Unix会像读取一般的脚本一样来读取这个文件。首先读到一个`":"`，这是一个shell的空语句。后面的;是分隔符。下一个命令是`exec`。`exec`告诉Unix放弃当前脚本的执行并转而执行`mzscheme -r $0 "$@"`，这里参数`$0`会被替换为当前文件的名字，参数`$@`会被替换为用户运行该脚本时附加的参数。（在本例中没有参数）

我们现在事实上以及把`hello`命令变换为另一个不同的命令，即：

```shell
mzscheme -r /whereveritis/hello
```

其中`/whereveritis/hello`是`hello`文件的路径名。

mzscheme命令调用了MzScheme的可执行文件。`-r`选项告诉它把紧跟在该选项后面的参数作为一个Scheme文件来加载，在这之前还要把所有其他参数（如果有的话）放进一个叫`argv`的向量中（在本例中，`argv`是一个空向量）。

因此，Scheme脚本会作为一个Scheme文件来执行，而且该文件中的Scheme代码还可以通过`argv`访问到所有该脚本原先的参数。

现在，Scheme不得不来处理这个脚本中的第一行了。正如我们所看到的，这一行可是一个精心构造的Shell脚本。`":"`是一个Scheme中自求值的字符串所以没有关系。`;`则开启了Scheme的注释，因此后面的exec等代码都被安全的忽略掉了。文件剩下的部分都是Scheme代码，被按顺序求值，所有的求值完成后，Scheme就退出了。

总之，在命令提示符后面输入`hello`会产生：

```
Hello, World!
```

并把命令提示符返回给你。


## 16.2 带参数的脚本

Scheme脚本使用argv变量来引用它的参数。例如，下面的脚本输出其所有参数，每个一行：

```scheme
":"; exec mzscheme -r $0 "$@"

;Put in argv-count the number of arguments supplied

(define argv-count (vector-length argv))

(let loop ((i 0))
  (unless (>= i argv-count)
    (display (vector-ref argv i))
    (newline)
    (loop (+ i 1))))
```

我们把这个脚本命名为`echoall`。调用`echoall 1 2 3`会显示：
```
1 
2 
3
```
注意脚本名称不包括在参数向量中。

## 16.3 例子

我们现在来解决一些更大的问题。我们需要在两台电脑之间传输文件，而唯一的方式是使用一张3.6英寸的软盘作为媒介。我们需要一个`split4floppy`的脚本来把大于1.44MB的文件分割为软盘能装下的小块。脚本`split4floppy`如下：

```scheme
":";exec mzscheme -r $0 "$@"

;floppy-size = number of bytes that will comfortably fit on a
;              3.5" floppy

(define floppy-size 1440000)

;split splits the bigfile f into the smaller, floppy-sized
;subfiles, viz, subfile-prefix.1, subfile-prefix.2, etc.

(define split
  (lambda (f subfile-prefix)
    (call-with-input-file f
      (lambda (i)
        (let loop ((n 1))
          (if (copy-to-floppy-sized-subfile i subfile-prefix n)
              (loop (+ n 1))))))))

;copy-to-floppy-sized-subfile copies the next 1.44 million
;bytes (if there are less than that many bytes left, it
;copies all of them) from the big file to the nth
;subfile.  Returns true if there are bytes left over,
;otherwise returns false.

(define copy-to-floppy-sized-subfile
  (lambda (i subfile-prefix n)
    (let ((nth-subfile (string-append subfile-prefix "."
                                      (number->string n))))
      (if (file-exists? nth-subfile) (delete-file nth-subfile))
      (call-with-output-file nth-subfile
        (lambda (o)
          (let loop ((k 1))
            (let ((c (read-char i)))
              (cond ((eof-object? c) #f)
                    (else
                     (write-char c o)
                     (if (< k floppy-size)
                         (loop (+ k 1))
                         #t))))))))))

;bigfile = script's first arg
;        = the file that needs splitting

(define bigfile (vector-ref argv 0))

;subfile-prefix = script's second arg
;               = the basename of the subfiles

(define subfile-prefix (vector-ref argv 1))

;Call split, making subfile-prefix.{1,2,3,...} from
;bigfile

(split bigfile subfile-prefix)
```

脚本`split4floppy`用如下方法调用：
```shell
split4floppy largefile chunk
```

这会把`largefile`分割成`chunk.1`、`chunk.2`等等，每个小块文件都能装进软盘中。

所有`chunk.i`都移动到目标电脑上以后可以通过把`chunk.i`按顺序拼起来还原`largefile`原文件，在Unix上这样做：
```shell
cat chunk.1 chunk.2 ... > largefile
```

在DOS下这样做：
```DOS
copy /b chunk.1+chunk.2+... largefile
```




