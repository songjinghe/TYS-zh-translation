第六章，递归
=====

一个过程体中可以包含对其它过程的调用，特别的是也可以调用自己。

```scheme
(define factorial
 (lambda (n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))
```

这个递归过程用来计算一个数的阶乘。如果这个数是0，则结果为1。对于任何其它的值n，这个过程会调用其自身来完成n-1阶乘的计算，然后将这个子结果乘上n并返回最终产生的结果。


互递归过程也是可以的。下面判断奇偶数的过程相互进行了调用。

```scheme
(define is-even?
 (lambda (n)
    (if (= n 0) #t
        (is-odd? (- n 1)))))
 
(define is-odd?
 (lambda (n)
    (if (= n 0) #f
        (is-even? (- n 1)))))
```

这里提供的两个过程的定义仅作为简单的互递归示例。Scheme已经提供了简单的判断过程`even?`和`odd?`。



## 6.1 letrec

如果希望将上面的过程定义为局部的，我们会尝试使用let结构：

```scheme
(let ((local-even? (lambda (n)
                     (if (= n 0) #t
                         (local-odd? (- n 1)))))
      (local-odd? (lambda (n)
                    (if (= n 0) #f
                        (local-even? (- n 1))))))
 (list (local-even? 23) (local-odd? 23)))
```

但这并不能成功，因为在初始化值过程中出现的`local-even?` 和 `local-odd?`指向的并不是这两个过程本身。


把`let`换成`let*`同样也不能奏效，因为这时虽然`local-odd?`中出现的`local-even?`指向的是前面刚创建好的局部的过程，但`local-even?` 中的`local-odd?`还是指向了别处。

为解决这个问题，Scheme提供了`letrec`结构。

```scheme
(letrec ((local-even? (lambda (n)
                        (if (= n 0) #t
                            (local-odd? (- n 1)))))
         (local-odd? (lambda (n)
                       (if (= n 0) #f
                           (local-even? (- n 1))))))
 (list (local-even? 23) (local-odd? 23)))
```

用`letrec`创建的词法变量不仅可以在`letrec`执行体中可见而且在初始化中也可见。`letrec`是专门为局部的递归和互递归过程而设置的。(这里也可以使用`define`来创建两个子结构的方式来实现局部递归)



## 6.2 命名let

使用`letrec`定义递归过程可以实现循环。如果我们想显示10到1的降数列，可以这样写：

```scheme
(letrec ((countdown (lambda (i)
                      (if (= i 0) 'liftoff
                          (begin
                            (display i)
                            (newline)
                            (countdown (- i 1)))))))
 (countdown 10))
```

这会在控制台上输出10到1，并会返回结果`liftoff`。


Scheme允许使用一种叫“命名let”的`let`变体来更简洁的写出这样的循环:

```scheme
(let countdown ((i 10))
 (if (= i 0) 'liftoff
      (begin
        (display i)
        (newline)
        (countdown (- i 1)))))
```

注意在`let`的后面立即声明了一个变量用来表示这个循环。这个程序和先前用`letrec`写的程序是等价的。你可以将“命名let”看成一个对`letrec`结构进行扩展的宏。




## 6.3 迭代
上面定义的`countdown`函数事实上是一个递归的过程。Scheme只有通过递归才能定义循环，不存在特殊的循环或迭代结构。

尽管如此，上述定义的循环是一个“真”循环，与其他语言实现它们的循环的方法完全相同。也就是说,Scheme十分注意确保上面使用过的递归类型不会产生过程调用/返回开销。

Scheme通过一种消除尾部调用（tail-call elimination）的过程完成这个功能。如果你注意观察`countdown`的步骤，你会注意到当递归调用出现在`countdown`主体内时，就变成了“尾部调用”，或者说是最后完成的事情——`countdown`的每次调用要么不调用它自身，要么当它调用自身时把这个动作留在最后。对于一个Scheme语言的实现来说（解释器），这会使递归不同于迭代。因此，尽管用递归去写循环吧，这是安全的。

这是又一个有用的尾递归程序的例子：

```scheme
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))
```

`list-position`发现了`o`对象在列表`l`中第一次出现的索引。如果在列表中没有发现对象，过程将会返回`#f`。

这又是一个尾部递归过程，它将自身的参数列表就地反转，也就是使现有的列表内容产生变异，而没有分配一个新的列表：
```scheme
(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	  (let ((d (cdr s)))
            (set-cdr! s r)
	    (loop d s))))))
```
`reverse!`是一个十分有用的过程，它在很多Scheme方言中都能使用，例如MzScheme和Guile）

更多地递归例子（包括迭代）参见附录C。

## 6.4 用自定义过程映射整个列表

有一种特殊类型的迭代，对列表中每个元素，它都会重复相同的动作。Scheme为这种情况提供了两种程序：`map`和`for-each`。

`map`程序为给定列表中的每个元素提供了一种既定程序，并返回一个结果的列表。例如：
```scheme
(map add2 '(1 2 3))
=>  (3 4 5)
```

`for-each`程序也为列表中的每个元素提供了一个程序，但返回值为空。这个程序纯粹是产生的副作用。例如：

```scheme
(for-each display
  (list "one " "two " "buckle my shoe"))
```

这个程序在控制台上有显示字符串（在它们出现的顺序上）的副作用。

这个由`map`和`for-each`用在列表上的程序并不一定是单参数程序。举例来说，假设一个n参数的程序，`map`会接受n个列表，每个列表都是由一个参数所组成的集合，而`map`会从每个列表中取相应元素提供给程序。例如：

```scheme
(map cons '(1 2 3) '(10 20 30))
=>  ((1 . 10) (2 . 20) (3 . 30))

(map + '(1 2 3) '(10 20 30))
=>  (11 22 33)
```
