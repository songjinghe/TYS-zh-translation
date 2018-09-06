第二章 数据类型
===============

数据类型是由相互关联的值构成的集合。各种数据类型互相联系，而且它们通常是具有层次关系。Scheme拥有丰富的数据类型：有一些是简单的类型，还有一些复合类型由其它的类型组合而成。

## 2.1 简单数据类型


Scheme中的简单数据类型包含 `booleans` (布尔类型) , `number`(数字类型), `characters`(字符类型) 和 `symbols`(标识符类型)。

### 2.1.1 Booleans


Scheme中的booleans类型用 `#t`、`#f`来分别表示true和false。Scheme拥有一个叫`boolean?`的过程，可以用来检测它的参数是否为boolean类型。

```
(boolean? #t)              =>  #t
(boolean? "Hello, World!") =>  #f
```

而`not`过程则直接取其参数的相反值做为boolean类型结果。

```
(not #f)              =>  #t
(not #t)              =>  #f
(not "Hello, World!") =>  #f
```

最后一个表达式清晰的显示出了Scheme的一个便捷性：在一个需要boolean类型的上下文中，Scheme会将任何非 `#f`的值看成true。


### 2.1.2 Numbers


Scheme的numbers类型可以是`integers`(整型，例如`42`)，`rationals`(有理数，例如`22/7`)，`reals`(实数，例如`3.14159`)，或`complex`(复数，`2+3i`)。一个整数是一个有理数，一个有理数是一个实数，一个实数是一个复数，一个复数是一个数字。


Scheme中有可供各种数字进行类型判断的过程：

```scheme
(number? 42)       =>  #t
(number? #t)       =>  #f
(complex? 2+3i)    =>  #t
(real? 2+3i)       =>  #f
(real? 3.1416)     =>  #t
(real? 22/7)       =>  #t
(real? 42)         =>  #t
(rational? 2+3i)   =>  #f
(rational? 3.1416) =>  #t
(rational? 22/7)   =>  #t
(integer? 22/7)    =>  #f
(integer? 42)      =>  #t
```

Scheme的integers(整型)不需要一定是10进制格式。可以通过在数字前加前缀 `#b` 来规定实现2进制。这样 `#b1100`就是10进制数字12了。实现8进制和16进制格式的前缀分别是 `#o` 和` #x`。(decimal前缀 `#d`是可选项)


我们可以使用通用相等判断过程 `eqv?` 来检测数字的相等性。(`eqv?`有点类似引用的相等判断ReferenceEquals)

```scheme
(eqv? 42 42)   =>  #t
(eqv? 42 #f)   =>  #f
(eqv? 42 42.0) =>  #f
```

不过，如果你知道参与比较的参数全是数字，选择专门用来进行数字相等判断的` = `会更合适些。(译注：整数与浮点数之间也可以进行比较)

```scheme
(= 42 42)   =>  #t
(= 42 #f)   -->ERROR!!!
(= 42 42.0) =>  #t
```

其它的数字比较还包括 `<`,` <=`,` >`,` >=`

```scheme
(< 3 2)    =>  #f
(>= 4.5 3) =>  #t
```

`+`, `-`,` *`,` /`, `expt`等数学运算过程具有我们期待的功能。

```scheme
(+ 1 2 3)    =>  6
(- 5.3 2)    =>  3.3
(- 5 2 1)    =>  2
(* 1 2 3)    =>  6
(/ 6 3)      =>  2
(/ 22 7)     =>  22/7
(expt 2 3)   =>  8
(expt 4 1/2) =>  2.0
```

对于一个参数的情况，`-` 和` / `过程会分别得到反数和倒数的结果。


`max`和`min` 过程会分别返回提供给它们的参数的最大值和最小值。它们可以支持任何的数字。

```scheme
(max 1 3 4 2 3) =>  4
(min 1 3 4 2 3) =>  1
```

`abs`过程会返回提供给它参数的绝对值。

```scheme
(abs  3) =>  3
(abs -4) =>  4
```

这些还只是冰山一角。Scheme提供一整套丰富数学和三角运算过程。比如` atan`, `exp`, 和 `sqrt`等过程分别返回参数的余切、自然反对数和开方值。


其它更具体的数学运算过程信息请参阅Revised^5 Report on the Algorithmic Language Scheme


----
### 2.1.3 Characters


Scheme中字符型数据通过在字符前加 `#\`前缀来表示。像` #\c`就表示字符` c`。那些非可视字符会有更多的描述名称，例如，`#\newline`, `#\tab`。空格字符可以写成 `#\ `，或者可读性更好一些的`#\space`。


字符类型判断过程是`char?` ：

```scheme
(char? #\c) =>  #t
(char? 1)   =>  #f
(char? #\;) =>  #t
```

需要注意的是数据的分号字符不会引发注释。


字符类型数据有自己的比较判断过程：`char=?`, `char<?`, `char<=?`, `char>?`, `char>=?`

```scheme
(char=? #\a #\a)  =>  #t
(char<? #\a #\b)  =>  #t
(char>=? #\a #\b) =>  #f
```

要实现忽略大小写的比较，得使用` char-ci` 过程代替` char`过程：

```scheme
(char-ci=? #\a #\A) =>  #t
(char-ci<? #\a #\B) =>  #t
```

而类型转换过程分别是 `char-downcase` 和`char-upcase`：

```scheme
(char-downcase #\A) =>  #\a
(char-upcase #\a)   =>  #\A
```

----
### 2.1.4 Symbols


前面我们所见到的简单数据类型的求值结果都是自己。也就是如果你在命令提示符后输入了任何这些类型的数据，运算后会返回和你输入内容是一样的结果。

```scheme
#t  =>  #t
42  =>  42
#\c =>  #\c
```

Symbols的求值结果与上面的简单数据类型不同。这是因为symbols通常在Scheme程序中被用来当做变量的标识，求值后可以得到变量所代表的值。然而symbols是一种简单数据类型，而且就像characers、numbers以及其它类型数据一样，是Scheme中可以传递的有效值类型。


创建一个单纯的symbol而非变量时，你需要使用`quote`过程：

```scheme
(quote xyz)
=>  xyz
```

因为在Scheme中经常要引用这种类型，我们有一种更简便的方式。表达式 `'E`和` (quote E) `在Scheme中是等价的。


Scheme中symbols由一个字符串来命名。在命名时不要和其它类型数据发生冲突，比如characters 、booleans、numbers 或复合类型。像` this-is-a-symbol`，`i18n`，` <=>`，和`$!#*`都是symbols，而 `16`，`1+2i`，`#t`，`"this-is-a-string"`和`'("hello" "world")` 都不是symbols类型数据,`'("hello" "world")` 是一个只包含两个字符串的List。


用来检查symbols类型数据的过程是`symbol?`

```scheme
(symbol? 'xyz) =>  #t
(symbol? 42)   =>  #f
```

Scheme的symbols类型通常都是不区分大小写的。因此`Calorie` 和`calorie`是等价的

```scheme
(eqv? 'Calorie 'calorie)
=>  #t
```

我们还可以使用` define` 将symbol 类型的数据 如`xyz`当成一个全局的变量来使用：

```scheme
(define xyz 9)
```

这样可以就创建了一个值为9的变量`xyz`.。
如果现在直接在Scheme命令提示符后输入`xyz`，这样会将xyz中的值做为运算结果。

```scheme
xyz
=>  9
```

如果想改变`xyz`中的值可以用`set!`来实现：

```scheme
(set! xyz #\c)
```

现在`xyz`中的值就是字符` #\c`了。

```scheme
xyz
=>  #\c
```


## 2.2 复合数据类型


复合数据类型是以组合的方式通过组合其它数据类型数据来获得。


----
### 2.2.1，Strings

字符串类型是由字符组成的序列（不能和symbols混淆，symbols仅是由一组字符来命名的简单类型）。你可以通过将一些字符包上闭合的双引号来得到字符串。Strings是自运算类型。

```scheme
"Hello, World!"
=>  "Hello, World!"
```

还可以通过向`string` 过程传递一组字符并返回由它们合并成的字符串：

```scheme
(string #\h #\e #\l #\l #\o)
=>  "hello"
```

现在让我们定义一个全局字符串变量 `greeting`。

```scheme
(define greeting "Hello; Hello!")
```

注意一个字符串数据中的分号不会得到注释。


一个给定字符串数据中的字符可以分别被访问和更改。
通过向`string-ref`过程传递一个字符串和一个从0开始的索引号，可以返回该字符串指定索引号位置的字符。

```scheme
(string-ref greeting 0)
=>  #\H
```

可以通在一个现有的字符串上追加其它字符串的方式来获得新字符串：

```scheme
(string-append "E "
               "Pluribus "
               "Unum")
=>  "E Pluribus Unum"
```

你可以定义一个指定长度的字符串，然后用期望的字符来填充它。

```scheme
(define a-3-char-long-string (make-string 3))
```

检测一个值是否是字符串类型的过程是`string?`。


通过调用`string`， `make-string` 和` string-append`获得的字符串结果都是可修改的。而过程`string-set!`就可以替换字符串指定索引处的字符。

```scheme
(define hello (string #\H #\e #\l #\l #\o)) 
hello
=>  "Hello"
 
(string-set! hello 1 #\a)
hello
=>  "Hallo"
```

### 2.2.2 Vectors (向量)


Vectors是像strings一样的序列，但它们的元素可以是任何类型，而不仅仅是字符，当然元素也可以是Vetors类型，这是一种生成多维向量的好方式。


这使用五个整数创建了一个vector：

```scheme
(vector 0 1 2 3 4)
=>  #(0 1 2 3 4)
```

注意Scheme表现一个向量值的方式：在用一对小括号包括起来的向量元素前面加了一个 `#` 字符。


和`make-string`过程类似，过程`make-vectors`可以构建一个指定长度的向量：

```scheme
(define v (make-vector 5))
```

而过程`vector-ref` 和` vector-set!`分别可以访问和修改向量元素。


检测值是否是一个向量的过程是`vector?`。


----
### 2.2.3 Dotted pairs(点对) 和 lists(列表)


点对是一种复合类型。把两个任意类型的值有序地连起来，即为一个点对。点对的第一个值被称作car，第二个值被称作cdr，而将两个值组合成点值对的过程是cons。

```scheme
(cons 1 #t)
=>  (1 . #t)
```

点对不能自运算，因此直接以值的方式来定义它们（即不通过调用`cons`来创建），必须显式的使用引号：

```scheme
'(1 . #t) =>  (1 . #t)
 
(1 . #t)  -->ERROR!!!
```

访问点值对值的过程分别是`car` (`car`访问点值对的第一个元素)和 `cdr`(`cdr`访问点值对的非一个元素)：

```scheme
(define x (cons 1 #t))
 
(car x)
=>  1
 
(cdr x)
=>  #t
```

点对的元素可以通过修改器过程`set-car!` 和` set-cdr!`来进行修改：

```scheme
(set-car! x 2)
 
(set-cdr! x #f)
 
x
=>  (2 . #f)
```

点对也可以包含其它的点对。

```scheme
(define y (cons (cons 1 2) 3))
 
y
=>  ((1 . 2) . 3)
```

这个点对的`car`运算结果`car`运算结果是`1`,而`car`运算结果的`cdr`运算结果是`2`。即:

```scheme
(car (car y))
=>  1
 
(cdr (car y))
=>  2
```

Scheme提供了可以简化`car` 和` cdr`组合起来连续访问操作的简化过程。像`caar`表示”`car` 运算结果的 `car`运算结果”， `cdar`表示”`car`运算结果的`cdr`运算结果”，等等。

```scheme
(caar y)
=>  1
 
(cdar y)
=>  2
```

像c...r这样风格的简写最多只支持四级连续操作。像`cadr`，`cdadr`，和 `cdaddr`都是存在的。而`cdadadr`这样的就不对了。


当第二个元素是一个嵌套的点对时，Scheme使用一种特殊的标记来表示表达式的结果：

```scheme
(cons 1 (cons 2 (cons 3 (cons 4 5))))
=>  (1 2 3 4 . 5)
```

即，`(1 2 3 4 . 5)`是对`(1 . (2 . (3 . (4 . 5))))`的一种简化。这个表达式的最后一个`cdr`运算结果是5。


如果嵌套点值对最后一个`cdr` 运算结果是一个空列表对象，Scheme提供了一种更进一步的用表达式`'()`来表示的简化方式。
空列表没有被考虑做为可以自运算的值，所以为程序提供一个空列表值时必须用单引号方式来创建：

```scheme
'() =>  ()
```

诸如像`(1 . (2 . (3 . (4 . ()))))`这样形式的点值对被简化成`(1 2 3 4)`。像这样第二元素都是一个点值对特殊形式的嵌套点值对就称作列表list。这是一个四个元素长度的列表。可以像这样来创建：

```scheme
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
```

但Scheme提供了一个list过程可以更方便的创建列表。List可以将任意个数的参数变成列表返回：

```scheme
(list 1 2 3 4)
=>  (1 2 3 4)
```

实际上，如果我们知道列表所包含的所有元素，我们还可以用`quote` 来定义一个列表：

```scheme
'(1 2 3 4)
=>  (1 2 3 4)
```

列表的元素可以通过指定索引号来访问。

```scheme
(define y (list 1 2 3 4))
 
(list-ref y 0) =>  1
(list-ref y 3) =>  4
 
(list-tail y 1) =>  (2 3 4)
(list-tail y 3) =>  (4)
```

`list-tail`返回了给定索引号后的所有元素。


`pair?`， `list?` 和` null?`判断过程可以分别用来检查它们的参数是不是一个点对，列表或空列表。

```scheme
(pair? '(1 . 2)) =>  #t
(pair? '(1 2))   =>  #t
(pair? '())      =>  #f
(list? '())      =>  #t
(null? '())      =>  #t
(list? '(1 2))   =>  #t
(list? '(1 . 2)) =>  #f
(null? '(1 2))   =>  #f
(null? '(1 . 2)) =>  #f
```

### 2.2.1 数据类型转换


Scheme提供了许多可以进行数据类型转换的过程。我们已经知道可以通过`char-downcase` 和 `char-upcase`过程来进字符大小写的转换。字符还可以通过使用`char->integer`来转换成整型，同样的整型也可以通过`integer->char`被转换成字符。(字符转换成整型得到的结果通常是这个字符的ascii码值。)

```scheme
(char->integer #\d) =>  100
(integer->char 50)  =>  #\2
```

字符串可以被转换成等价的字符列表。

```scheme
(string->list "hello") =>  (#\h #\e #\l #\l #\o)
```

其它的转换过程也都是一样的风格`list->string`， `vector->list` 和 `list->vector`。


数字可以转换成字符串：`(number->string 16) =>  "16"`


字符串也可以转换成数字。如果字符串不能转换成数字，则会返回`#f`。

```scheme
(string->number "16")
=>  16
 
(string->number "Am I a not number?")
=>  #f
```

`string->number`第二个参数是可选参数，指示以几进制来转换。

```scheme
(string->number "16" 8) =>  14
```

八进制的数字 `16` 等于` 14`。


Symbols也可以转换为字符串，反之亦然：

```scheme
(symbol->string 'symbol)
=>  "symbol"
 
(string->symbol "string")
=>  string
```


## 2.3 其它数据类型


Scheme还包含了一些其它数据类型。一个是 *procedure* (过程)。我们已经见过了许多过程了，例如，`display`， `+`， `cons`等。实际上，它们是一些承载了过程值的变量，过程本身内部的数值和字符并不可见：

```scheme
cons
=>  <procedure>
```

迄今为止我们所见过的这些过程都属于原始过程（系统过程），由一些全局变量来承载它们。用户还可以添加自定义的过程。


还有另外种数据类型是port端口。一个端口是为输入输出提供执行的通道。端口通常会和文件和控制台操作相关联。


在我们的`"Hello，World！"`程序中，我们使用`display`过程向控制台输出了一个字符串。`display`可以接受两个参数，第一个参数值是将输出的值，另一个值则表示了即将承载显示结果的输出port(端口)。


在我们的程序中，`display`的第二参数是隐式参数。这时候`display`会采用标准输出端口作为它的默认输出端口。我们可以通过调用`current-output-port`过程来取得当前的标准输出端口。我们可以更清楚的写出:

```scheme
(display "Hello, World!" (current-output-port))
```

## 2.4 S-expressions（S表达式）


所有这些已经被讨论过的数据类型可以被统一成一种通用的叫作s-expression(符号表达式或s-表达式)的数据类型(s代表符号)。像 `42`，`#\c`，`(1 . 2)` ， `#(a b c)` ，`"Hello"`， `(quote xyz)` ， `(string->number "16")`， 和 `(begin (display "Hello, World!") (newline))`都是s-表达式。
