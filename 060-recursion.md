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
