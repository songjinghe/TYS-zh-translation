第八章 宏
===========

用户可以通过定义宏来创建属于自己的`special form`。宏是一个具有与它相关联的转换器程序的标记。当Scheme遇到一个宏表达式，即以macro—作为开头的列表时，它会将宏的转换器应用于宏表达式中的子列表，而且会对最后的转换结果进行求值。

理想情况下，“宏”指代从一种代码文本到另一种代码文本的纯文本变换。这种变换对于缩写那些复杂的但经常出现的文本模式十分有用。

宏通过`define-macro`来定义（见附录A.3）。例如，如果你的Scheme缺少条件表达式when，你就可以以下述宏定义when：

```scheme
(define-macro when
  (lambda (test . branch)
    (list 'if test
      (cons 'begin branch))))
```

这样定义的when转换器能够把一个when表达式转换为等价的if表达式。用这个宏，下面的when表达式

```scheme
(when (< (pressure tube) 60)
   (open-valve tube)
   (attach floor-pump tube)
   (depress floor-pump 5)
   (detach floor-pump tube)
   (close-valve tube))
```

将会被转换为另一个表达式，把when转换器应用到when表达式的子`form`：

```scheme
(apply
  (lambda (test . branch)
    (list 'if test
      (cons 'begin branch)))
  '((< (pressure tube) 60)
      (open-valve tube)
      (attach floor-pump tube)
      (depress floor-pump 5)
      (detach floor-pump tube)
      (close-valve tube)))
```

这个转换产生了一个列表：

```scheme
(if (< (pressure tube) 60)
    (begin
      (open-valve tube)
      (attach floor-pump tube)
      (depress floor-pump 5)
      (detach floor-pump tube)
      (close-valve tube)))
```

Scheme将会对这个表达式进行求值，就像它对其他表达式所做的一样。

再来看另一个例子，这有一个`unless`（`when`的另一种形式）的宏定义：

```scheme
(define-macro unless
  (lambda (test . branch)
    (list 'if
          (list 'not test)
          (cons 'begin branch)))) 
```

另外，我们可以调用`when`放进`unless`定义中：

```scheme
(define-macro unless
  (lambda (test . branch)
    (cons 'when
          (cons (list 'not test) branch))))
```

宏表达式可以引用其他的宏。

## 8.1 指定一个扩展为模板

宏转换器一般接受一些S表达式作为参数，同时产生可以被作为`form`使用的S表达式。通常情况下输出是一个列表。在我们的when例子中，使用下面语句创建输出列表：

```scheme
(list 'if test
  (cons 'begin branch))
```

其中test与宏的第一个子`form`绑定，即：

```scheme
(< (pressure tube) 60)
```

同时`branch`与余下的宏的子`form`绑定，即：

```scheme
((open-valve tube)
 (attach floor-pump tube)
 (depress floor-pump 5)
 (detach floor-pump tube)
 (close-valve tube))
```

输出列表可能会变得相当复杂。我们很容易能够发现比when更加庞大的宏可以对输出列表完成精心的加工工程。这种情况下，更方便的方法是把宏的输出指定为模板，对宏的每种用法把相关参数插入到模板的适当位置。Scheme提供了backquote语法来指定这种模板。因此表达式：

```scheme
(list 'IF test
  (cons 'BEGIN branch))
```

写成这样会更加方便：

```scheme
`(IF ,test
  (BEGIN ,@branch)) 
```

我们能够将`when`的宏表达式重构为：

```scheme
(define-macro when
  (lambda (test . branch)
    `(IF ,test
         (BEGIN ,@branch)))) 
```

注意模板的格式，并不像早先列表的结构，而是对输出列表的形态给出了直接的视觉指示。反引号（`）为列表引进了一个模板。除了以逗号（,）或（,@）作为前缀的元素外，模板的元素会在结果列表中逐字出现。（为了举例，我们把模板的每一个会在结果中原封不动出现元素写成了大写）。

`,`和`,@`可以将宏参数插入到模板中。`,`插入的是逗号后面紧接着它的下一个表达式求值后的结果。`,@`(comma-splice)插入的是它的下一个表达式先splice再求值的结果。即：它消除了最外面的括号。（这说明被comma-splice引用的表达式必须是一个列表。）

在我们的例子中，给定`test`和`branch`的绑定值，很容易看到模板将扩展到所需的地步。

```scheme
(IF (< (pressure tube) 60)
    (BEGIN
      (open-valve tube)
      (attach floor-pump tube)
      (depress floor-pump 5)
      (detach floor-pump tube)
      (close-valve tube)))
```

## 8.2 避免在宏内部产生变量捕获

一个二变量的`disjunction form`，`my-or`，可以定义为：

```scheme
(define-macro my-or
  (lambda (x y)
`(if ,x ,x ,y)))
```

`my-or`带有两个参数并返回两个之中第一个为真（非#f）的值。特别的，只有当第一个参数为假时才会对第二个参数求值。

```scheme
(my-or 1 2)
=>  1
(my-or #f 2)
=>  2
```

上述的`my-or`宏时会有一个问题。如果第一个参数为真，会重新求值第一个参数：第一次是在if语句中，第二次在then分支。如果第一个参数包含副作用，这会造成意外的结果，例如：
```scheme
(my-or
  (begin 
    (display "doing first argument")
     (newline)
     #t)
  2)
```
会显示`doing first argument`两次。

这个情况可以通过在局部变量中储存if测试结果来避免：

```scheme
(define-macro my-or
  (lambda (x y)
    `(let ((temp ,x))
       (if temp temp ,y))))
```

这样基本上OK了，除非当第二个参数在宏定义中使用时包含相同的temp。例如：

```scheme
(define temp 3)

(my-or #f temp)
=>  #f
```

当然结果应该是3！错误产生的原因是由于宏使用了局部变量`temp`储存第一个参数（`#f`）的值，而第二个参数中的变量`temp`被宏引入的`temp`所捕获。

```scheme
(define temp 3)

(let ((temp #f))
  (if temp temp 3))
```

为避免这类错误，我们在选择宏定义中的局部变量时需要小心行事。我们应该为这些变量选择古怪的名字并热切希望没有人会跟它们扯上关系。例如：

```scheme
(define-macro my-or
  (lambda (x y)
    `(let ((+temp ,x))
       (if +temp +temp ,y))))
```

如果默认+temp在宏之外的代码中不被使用，则它就是正确的。但这种幻想是迟早要破灭的。

一个更加可靠详细的方法就是生成保证不会被其他方式占用的符号。当调用`gensym`程序时，它会产生出独一无二的标志。这是一个使用`gensym`的`my-or`的安全定义：

```scheme
(define-macro my-or
  (lambda (x y)
    (let ((temp (gensym)))
      `(let ((,temp ,x))
         (if ,temp ,temp ,y)))))
```

为了简明，在本文中定义的宏，不使用`gensym`方法。相反，我们将假设变量捕获这个问题已经被考虑到了，而使用更加简明的`+`作为前缀。我们把这些将加号开头的标识符转换为gensym的工作留给敏锐的读者。

## 8.3 fluid-let

这有一个更加复杂的宏的定义，`fluid-let`（见5.2节）。`fluid-let`对一组已经存在的词法变量指定了临时绑定。假定一个fluid-let表达式如下：
```scheme
(fluid-let ((x 9) (y (+ y 1)))
  (+ x y))
```
我们想扩展为：
```scheme
(let ((OLD-X x) (OLD-Y y))
  (set! x 9)
  (set! y (+ y 1))
  (let ((RESULT (begin (+ x y))))
    (set! x OLD-X)
    (set! y OLD-Y)
    RESULT))
```
在例子中我们希望标识符`OLD-X`，`OLD-Y`和`RESULT`不会捕获`fluid-let`里的变量。

下述例子教你如何构造一个可以实施你的想法的`fluid-let`宏：

```scheme
(define-macro fluid-let
  (lambda (xexe . body)
    (let ((xx (map car xexe))
          (ee (map cadr xexe))
          (old-xx (map (lambda (ig) (gensym)) xexe))
          (result (gensym)))
      `(let ,(map (lambda (old-x x) `(,old-x ,x)) 
                  old-xx xx)
         ,@(map (lambda (x e)
                  `(set! ,x ,e)) 
                xx ee)
         (let ((,result (begin ,@body)))
           ,@(map (lambda (x old-x)
                    `(set! ,x ,old-x)) 
                  xx old-xx)
           ,result)))))
```
宏的参数是`xexe`，是由`fluid-let`引进的变量/表达式列表；而`body`，则是在`fluid-let`主体中的表达式列表。在我们的例子中，这两者分别是`((x 9) (y (+ y 1)`和`((+ xy))`。

宏的主体引进了一堆局部变量：`xx`是从变量/表达式中提取的变量列表。`ee`是对应的表达式列表。`old-xx`是新的标识符的列表，对应于`xx`中的每个变量。这些曾用来储存`xx`的传入值，这样我们可以将`xx`恢复到`fluid-let`主体求值前的状态。Result是另一个新标志符，用来储存`fluid-let`主体的值。在我们的例子中，`xx`是`(x y)`，`ee`是`(9(+ y 1))`。根据你的系统实现`gensym`的方式，`old-xx`会成为列表`(GEN-63 GEN-64)`，`result`会成为`GEN-65`。

在我们的例子中，由宏创建的输出列表像这样：

```scheme
(let ((GEN-63 x) (GEN-64 y))
  (set! x 9)
  (set! y (+ y 1))
  (let ((GEN-65 (begin (+ x y))))
    (set! x GEN-63)
    (set! y GEN-64)
    GEN-65))
```

这确实可以满足我们的需求。

