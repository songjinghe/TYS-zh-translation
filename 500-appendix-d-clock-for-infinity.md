附录D 可设为infinity的时钟
====================

Guile的过程`alarm`提供了一种可中断的定时器机制。用户可以给这个时钟设置或重置一些时间片，或者让它停止。当时钟的定时器递减到0后，它就会执行用户之前设定的动作。Guile的`alarm`不是一个类似于第十五章第一节里定义的那种时钟，但是我们可以很容易的把它改造成那样。

时钟的定时器的初始状态是停止的，也就是说它不会随着时间流逝而被“触发”。如果想把定时器的触发时间设置为`n`秒（`n`不为0），运行`(alarm n)`。如果定时器已经被设定过了，那么`(alarm n)`就返回该定时器在本次设定前剩余的秒数。如果之前没有设定过，则返回0。

执行`(alarm 0)`让时钟的定时器停止，即定时器中的计数器【你可以理解为一个变量】不会随时间而递减，而且不会触发。`(alarm 0)`同样返回定时器在本次设定前剩余的秒数（如果之前设定过的话）。

默认情况下，当时钟的定时器计数减到0时，Guile会在控制台上显示一条消息并退出。更多的行为可以用过程`sigaction`来设定，如下所示：

```scheme
(sigaction SIGALRM
  (lambda (sig)
    (display "Signal ")
    (display sig)
    (display " raised.  Continuing...")
    (newline)))
```

第一个参数`SIGALRM`（恰好是14）告诉`sigaction`需要设定的时钟处理函数[1]。第二个参数是一个用户指定的单参数过程。在这个例子里，当时钟触发时，处理函数会在控制台上显示`"Signal 14 raised. Continuing..."`而不是退出Scheme（14是变量`SIGALRM`的值，时钟会把它传递给它对应的处理过程，我们现在先不考虑这个）。

从我们的角度看，这种简单的定时器机制有一个问题。过程`alarm`的返回值`0`的意义是不明确的：既可能是指时钟处于停止状态，也有可能是刚好计时器减到了0。我们可以通过在时钟的算法里引入`*infinity*`来解决这个问题。换句话说，我们需要的时钟与`alarm`基本上是差不多的，除了一点，那就是如果时钟停止的话，那么它有`*infinity*`秒。这样就看起来比较自然了。

1. `(clock n)`对于一个停止的时钟返回`*infinity*`，而不是0。
2. 如果让时钟停止，执行`(clock *infinity*)`，而不是`(clock 0)`。
3. `(clock 0)`相当于给时钟设置一个无限小的时间，也就是让它立即触发。

在Guile中，我们可以把`*infinity*`定义为如下的“数”：

```scheme
(define *infinity* (/ 1 0))
```

我们用`alarm`来定义`clock`。

```scheme
(define clock
  (let ((stopped? #t)
        (clock-interrupt-handler
         (lambda () (error "Clock interrupt!"))))
    (let ((generate-clock-interrupt
           (lambda ()
             (set! stopped? #t)
             (clock-interrupt-handler))))
      (sigaction SIGALRM
                 (lambda (sig) (generate-clock-interrupt)))
      (lambda (msg val)
        (case msg
          ((set-handler)
           (set! clock-interrupt-handler val))
          ((set)
           (cond ((= val *infinity*)
                  ;This is equivalent to stopping the clock.
                  ;This is almost equivalent to (alarm 0), except
                  ;that if the clock is already stopped,
                  ;return *infinity*.

                  (let ((time-remaining (alarm 0)))
                    (if stopped? *infinity*
                        (begin (set! stopped? #t)
                          time-remaining))))

                 ((= val 0)
                  ;This is equivalent to setting the alarm to
                  ;go off immediately.  This is almost equivalent
                  ;to (alarm 0), except you force the alarm
                  ;handler to run.

                  (let ((time-remaining (alarm 0)))
                    (if stopped?
                        (begin (generate-clock-interrupt)
                          *infinity*)
                        (begin (generate-clock-interrupt)
                          time-remaining))))

                 (else
                  ;This is equivalent to (alarm n) for n != 0.
                  ;Just remember to return *infinity* if the
                  ;clock was previously quiescent.

                  (let ((time-remaining (alarm val)))
                    (if stopped?
                        (begin (set! stopped? #f) *infinity*)
                        time-remaining))))))))))
```

过程`clock`用到了三个内部状态变量：

1. `stopped?`，表示时钟是否是停止的；
2. `clock-interrupt-handler`，一个过程，表示用户希望在时钟触发后执行的动作；
3. `generate-clock-interrupt`，另一个过程，该过程会在运行用户定义的时钟处理过程前把`stopped?`设为`false`。

过程`clock`有两个参数。如果第一个参数是`set-handler`，那么就把第二个参数作为时钟处理器。

如果第一个参数是`set`，就把该时钟触发时间设置为第二个参数，返回本次设定前定时器剩余的秒数。代码对`0`、`*infinity*`以及其他时间值的处理是不同的，这样用户可以得到一个算术上对`alarm`透明的接口。

----------------------------------------
[1]. 还有一些其他的信号和与之相应的处理器，`sigaction`同样可以使用它们。
