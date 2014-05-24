第十三章 跳转
=============

Scheme的一个显著标志是它支持跳转或者`nonlocal control`。特别是Scheme允许程序控制跳转到程序的任意位置，相比之下条件语句和函数调用的限制要更多一些。Scheme的`nonlocal control`操作符是一个名为`call-with-current-continuation`的过程。下面我们会看到如何用这个操作符创建一些惊人的控制效果。

## 13.1 call-with-current-continuation

`call-with-current-continuation`用`current-continuation`来调用它的参数（一个只有一个参数的过程）【在调用时传入参数`current-continuation`，译者注】。这就是这个操作符名字的解释了。但是由于这个名字太长，故通常缩写为`call/cc`。

一个程序执行到任意一点的当前续延【即`current-continuation`，译者注】是该程序的后半部分【即将要被执行的部分，译者注】。因此在程序：
```scheme
(+ 1 (call/cc
       (lambda (k)
         (+ 2 (k 3)))))
```
中，“后边部分”——从`call/cc`程序的角度来看，是如下的带有一个“洞”的程序（“洞”用`[]`表示）：
```scheme
(+ 1 [])
```
也就是说，该程序的“续延”是一个把`1`和填到这个“洞”里的东西加起来的程序。

这就是`call/cc`参数被调用的情况。记住`call/cc`的参数是过程：
```scheme
(lambda (k)
  (+ 2 (k 3)))
```
这个过程的把“续延”（现在绑定在`k`上）`apply`到参数`3`上。这就是这个续延与众不同之处。“续延”调用突然放弃了它自己的计算并把当前的计算换成了`k`中保存的程序。也就是说，这个程序中加`2`的操作被放弃了，然后`k`的参数`3`直接被发送到了那个带“洞”的程序：
```scheme
(+ 1 [])
```
然后程序就简单的变成：
```scheme
(+ 1 3)
```
然后返回`4`。即：
```scheme
(+ 1 (call/cc
       (lambda (k)
         (+ 2 (k 3)))))
```
上面的例子叫做“‘退出’续延”，用来退出某个计算过程（这里是`(+ 2 [])`的计算）。这是一个很有用的功能，但是Scheme的续延可以用来返回到前面放弃计算的地方，然后多次调用它们。程序的“后半部分”意味着一个续延不论我们调用的次数和时间都存在，这也让`call/cc`更加强大和令人迷惑。看一个简单的例子，在解释器里输入以下代码：
```scheme
(define r #f)

(+ 1 (call/cc
       (lambda (k)
         (set! r k)
         (+ 2 (k 3)))))
=>  4
```
后面的表达式和刚才一样返回了`4`，不同之处在于这次我们把续延`k`保存到了全局变量`r`里。

现在我们在`r`中永久保存了这个续延。如果我们以一个数字为参数调用它，就会返回数字加1后的结果：
```scheme
(r 5)
=>  6
```
注意`r`会放弃它自己的续延，打个比方我们把对`r`的调用放在一个上下文中：
```scheme
(+ 3 (r 5))
=>  6
```
因此`call/cc`提供的续延是一种“放弃”的续延。

## 13.2 “退出”续延

“退出”续延是`call/cc`最简单的用法，而且在退出函数或循环时非常有用。考虑一个过程`list-product`接收一个数字列表并把所有的数乘起来。一个直观的递归定义可以这样写：
```scheme
(define list-product
  (lambda (s)
    (let recur ((s s))
      (if (null? s) 1
          (* (car s) (recur (cdr s)))))))
```
这个方法有一个问题。如果列表中有一个数是0，而且0后面还有很多元素，那么结果是可以预知的。如果这样上面的代码会在得出结果前产生很多无意义的递归调用。这就是“退出”续延大显身手的时候。用`call/cc`，我们可以这样重写这个过程：
```scheme
(define list-product
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (if (null? s) 1
              (if (= (car s) 0) (exit 0)
                  (* (car s) (recur (cdr s))))))))))
```
如果遇到一个为0的元素，续延`exit`就会以参数0被调用，这样就防止了更多的调用`recur`。

## 13.3 树匹配

一个更加复杂的例子是把续延用于解决两个树是否有相同边缘（就是相同的元素（叶节点）有相同的顺序）的问题上。如：

```scheme
(same-fringe? '(1 (2 3)) '((1 2) 3))
=>  #t

(same-fringe? '(1 2 3) '(1 (3 2)))
=>  #f
```

纯粹的函数式解决方案是把两个树都抹平然后看结果是否一样。

```scheme
(define same-fringe?
  (lambda (tree1 tree2)
    (let loop ((ftree1 (flatten tree1))
               (ftree2 (flatten tree2)))
      (cond ((and (null? ftree1) (null? ftree2)) #t)
            ((or (null? ftree1) (null? ftree2)) #f)
            ((eqv? (car ftree1) (car ftree2))
             (loop (cdr ftree1) (cdr ftree2)))
            (else #f)))))

(define flatten
  (lambda (tree)
    (cond ((null? tree) '())
          ((pair? (car tree))
           (append (flatten (car tree))
                   (flatten (cdr tree))))
          (else
           (cons (car tree)
                 (flatten (cdr tree)))))))
```

然而，这样会遍历整个树来进行抹平操作，而且还要再做一遍这样的操作【遍历被抹平后生成的列表，译者注】才能找到不匹配的元素。退一步讲，即使最好的抹平算法也需要`cons`（直接修改输入的树是不可以的）

我们可以用`call/cc`来解决这个问题，不需要遍历，也不需要用`cons`来拼接。每个树会被`map`到一个生成器——一个带有内部状态的过程，按照叶节点在树中出现的顺序从左到右连续的输出叶节点。
```scheme
(define tree->generator
  (lambda (tree)
    (let ((caller '*))
      (letrec
          ((generate-leaves
            (lambda ()
              (let loop ((tree tree))
                (cond ((null? tree) 'skip)
                      ((pair? tree)
                       (loop (car tree))
                       (loop (cdr tree)))
                      (else
                       (call/cc
                        (lambda (rest-of-tree)
                          (set! generate-leaves
                            (lambda ()
                              (rest-of-tree 'resume)))
                          (caller tree))))))
              (caller '()))))
        (lambda ()
          (call/cc
           (lambda (k)
             (set! caller k)
             (generate-leaves))))))))
```

当一个`tree->generator`创建的生成器被调用时，这个生成器会把调用的续延存在`caller`中，这样它就知道当找到叶节点时把它发送给谁。然后它调用一个内部定义的函数`generate-leaves`，该函数会从左到右循环遍历这个树。当循环到一个叶节点时，该函数就使用`caller`来返回该叶节点作为生成器的结果，但是它会记住后续的循环（被`call/cc`捕获为一个续延）并保存到`generate-leaves`变量，下次生成器被调用时，循环从刚才终端的地方恢复，这样它可以寻找下一个叶节点。

注意`generate-leaves`做的最后一件事情，在循环结束后，它返回一个空列表给`caller`。由于空列表不是一个合法的叶节点，我们可以用它来告诉生成器没有叶节点需要生成了。

过程`same-fringe?`把树作为参数来创建生成器，然后交替调用这两个生成器。只要一找到两个不同的叶节点就会返回失败。

```scheme
(define same-fringe?
  (lambda (tree1 tree2)
    (let ((gen1 (tree->generator tree1))
          (gen2 (tree->generator tree2)))
      (let loop ()
        (let ((leaf1 (gen1))
              (leaf2 (gen2)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))
```
很容易看到每个树只被遍历了最多一次，在遇到不匹配的情况时，只会遍历最左边的那个不匹配节点【？】。而且没有用到`cons`。

## 13.4 协程

上面用到的生成器是一些有趣而普遍的过程概念。每次生成器被调用时，它都恢复计算，而且当它返回前会把它的续延保存在一个内部变量中这样这个生成器可以再次恢复。我们可以对生成器进行推广，这样他们可以相互恢复其他的生成器，并且互相传递结果。这样的过程叫协程。

我们将会看到一个协程作为一元过程，其主体可以包含`resume`调用，`resume`是一个两参数的过程，可以被一个协程用来继续执行另一个协程（带着一个转换值）。宏`coroutine`定义一个这样的协程过程，一个变量名作为协程的初始参数，内容作为协程。
```scheme
(define-macro coroutine
  (lambda (x . body)
    `(letrec ((+local-control-state (lambda (,x) ,@body))
              (resume
                (lambda (c v)
                 (call/cc
                  (lambda (k)
                    (set! +local-control-state k)
                    (c v))))))
       (lambda (v)
         (+local-control-state v)))))
```
调用这个宏可以创建一个协程（我们叫为`A`），这个协程可以有一个参数。`A`有一个内部变量叫做`+local-control-state`来保存任意时刻这个协程接下来的计算。当调用`resume`时——也就是调用另一个协程`B`时——当前协程会更新它的`+local-control-state`变量为之后的计算，然后停止，然后跳到恢复了的协程`B`，当协程`A`之后恢复时，它的计算会从它`+local-control-state`变量里存放的续延开始。

### 13.4.1 用协程进行树匹配

用协程会进一步简化树匹配的操作。匹配过程被编写为一个协程，该协程依赖另外两个协程提供各自的叶节点。

```scheme
(define make-matcher-coroutine
  (lambda (tree-cor-1 tree-cor-2)
    (coroutine dont-need-an-init-arg
      (let loop ()
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))
```
叶生成器协程会记住把它的节点返回给谁：

```scheme
(define make-leaf-gen-coroutine
  (lambda (tree matcher-cor)
    (coroutine dont-need-an-init-arg
      (let loop ((tree tree))
        (cond ((null? tree) 'skip)
              ((pair? tree)
               (loop (car tree))
               (loop (cdr tree)))
              (else
               (resume matcher-cor tree))))
      (resume matcher-cor '()))))
```
