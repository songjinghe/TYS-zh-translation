附录B 在DOS中运行Scheme脚本
==========================

DOS的脚本也叫做“批处理”。通常一个输出`Hello World!`的DOS批处理文件应该这样写：
```shell
echo Hello, World!
```

这里用到了DOS的命令`echo`。脚本文件被命名为`hello.bat`，这样会被操作系统认为是可执行的。然后可以放入任一在`PATH`环境变量中的目录下。然后任何时候在DOS提示符下输入
```shell
hello.bat
```
或者更简单的：
```shell
hello
```
就能立即得到这句俗得不能再俗的问候。

Scheme版本的`hello`批处理文件会用Scheme产生相同的输出，但是我们需要在文件中写一些东西来告诉DOS让它用Scheme来分析文件内容，而不是理解为它默认的脚本批处理语言。Scheme的批处理文件（也命名为`hello.bat`），内容如下：

```bat
;@echo off 
;goto :start 
#| 
:start 
echo. > c:\_temp.scm 
echo (load (find-executable-path "hello.bat" >> c:\_temp.scm 
echo "hello.bat")) >> c:\_temp.scm 
mzscheme -r c:\_temp.scm %1 %2 %3 %4 %5 %6 %7 %8 %9 
goto :eof 
|# 
 
(display "Hello, World!") 
(newline) 
 
;:eof 
```

到`|#`之前全部是标准的DOS批处理命令。后面是Scheme的问候代码。最后还有一行标准的DOS批处理，即`;:eof`。

当用户在DOS提示符下输入`hello`时，DOS读取并将`hello.bat`作为一个普通的批处理文件来运行。第一行，`;@echo off`，关闭了命令运行时产生的输出――我们不想让大量冗余信息淹没我们脚本产生的输出。第二行，`;goto :start`，让脚本的执行跳转到标号`:start`即第四行。后面接着的`echo`行创建了一个叫`c:\_temp.scm`的Scheme临时文件，其内容如下：

```scheme
(load (find-executable-path "hello.bat" "hello.bat"))
```

下一个批处理命令调用MzScheme。`-r`选项加载Scheme文件`c:\_temp.scm`。所有的参数（在这个例子里没有）可以在Scheme中通过`argv`向量来获得。这个调用的Scheme会对我们的脚本进行求值，我们下面会看到。Scheme执行返回后，我们仍然需要让批处理文件正常地结束（否则就会碰到它不认识的Scheme代码了）。下一个批处理命令是`goto :eof`，这会让控制流跳过所有的Scheme代码，到达文件末尾，也就是包含`;:eof`标签的一行。然后脚本结束运行。

现在我们知道如何让Scheme来执行它的那部分代码，即运行嵌入在批处理文件中的Scheme表达式。载入`c:\_temp.scm`会使Scheme找到`hello.bat`文件的绝对路径（用`find-executable-path`过程），然后载入`hello.bat`。

因此，Scheme脚本文件现在会以Scheme文件来运行，文件中的Scheme表达式可以通过向量`argv`来访问脚本的原始参数。

现在Scheme略过脚本中的批处理命令。这是因为这些批处理命令要么以分号开头，要么用`#|...|#`包裹，这在Scheme看来是注释。

文件剩下的部分当然是Scheme代码，因此表达式被依次求值（最后的表达式`;:eof`是一个Scheme注释，因此没有关系）总之所有的表达式被求值后，Scheme会退出。

综上所述，在DOS提示符下输入hello会产生

```
Hello, World!
```

并返回DOS提示符。
