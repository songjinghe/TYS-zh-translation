第十五章 引擎
==============

引擎表示服从时间抢占的运算过程。换句话说，一个引擎下面的运算过程是普通的程序作为定时器可抢占的进程。

一个引擎用三个参数来调用：

1. 分配时间片（运行时间单元）的数目
2. 成功过程
3. 失败过程

如果引擎的计算在分配的时间片内完成了，那么就把计算的结果作为参数来调用成功过程，如果没有计算完成，那么把未计算完的部分作为参数来调用失败过程。

比如，考虑一个引擎，其下的运算是一个循环，该循环打印非负整数的序列。该引擎用下面的`make-engine`过程（后面会定义该过程）创建，`make-engine`接受一个程序（即该引擎下面的计算过程）为参数，并返回对应的引擎。
```scheme
(define printn-engine
  (make-engine
    (lambda ()
      (let loop ((i 0))
        (display i)
        (display " ")
        (loop (+ i 1))))))
```
下面调用`pritn-engine`：
```scheme
(define *more* #f)
(printn-engine 50 list (lambda (ne) (set! *more* ne)))
=>  0 1 2 3 4 5 6 7 8 9
```
也就是循环打印到某个特定的数（这里是`9`）然后就失败(fail)了，因为时钟中断了。然而，我们定义的失败过程把fail掉的引擎赋值给了全局变量`*more*`。这样我们就可以从上个引擎中断的地方恢复：
```scheme
(*more* 50 list (lambda (ne) (set! *more* ne)))
=>  10 11 12 13 14 15 16 17 18 19
```
我们现在来构建引擎，使用`call/cc`来捕获一个失败引擎未完成的计算。首先我们会构造一个flat引擎，也就是说该引擎的计算中不能运行其他引擎。稍后我们会让代码更通用，实现nestable引擎，这样的引擎可以调用其他引擎。但是不管那种引擎，我们都需要一个定时的东西，时钟（clock）。

## 15.1 时钟

我们的引擎假设有一个全局的时钟或可中断的定时器来记录程序运行的时间片。我们假设下面的时钟接口——你通常应该很容易把Scheme提供的时钟接口（如果有的话）打包成下面这种类型。（附录D用Scheme的Guile方言定义了一个时钟）

我们的`clock`过程的内部状态包括以下两项：

1. 剩余的时间片的数目，以及
2. 一个中断处理器(handler)，当时钟的时间片用完了的时候被调用。

`clock`允许下面的操作：

1. `(clock 'set‑handler h)`设置中断处理器为`h`。
2. `(clock 'set n)`把时钟的剩余时间片重置为`n`，返回之前的值。

`n`的取值范围是所有非负整数以及一个叫`*infinity*`的原子。一个时钟如果有`*infinity*`的时间片永远不会终止，所以也用不着设置中断处理器。这样的时钟总是“静止的”或“停止的”（定时器的工作就是：减到0并中断，而这样的时钟永远不会减到0并中断，所以处于“非工作”状态）。让一个时钟停止只要把时间片设为`*infinity*`即可。

时钟的处理器被设置为一个程序，比如：

```scheme
(clock 'set-handler
  (lambda ()
    (error "Say goodnight, cat!")))

(clock 'set 9)
```

这样`9`个时间片过去后会产生一个错误，并且显示的错误信息是`"Say goodnight, cat!"`

## 15.2 flat引擎

我们将首先设置时钟的中断处理器。注意这个处理器只有在“工作”状态的时钟用完时间片后才会被调用。这只有引擎计算失败时才发生，因为只有引擎设置时钟。

处理器捕获当前的续延，也就是当前失败引擎的剩余计算部分。这个续延被保存到全局的`*engine-escape*`变量中。该变量存放当前引擎的续延。因此时钟处理器捕获失败引擎的剩余部分并把它发送到引擎代码的出口。这样所需的失败处理才能执行。
```scheme
(define *engine-escape* #f)
(define *engine-entrance* #f)

(clock 'set-handler
  (lambda ()
    (call/cc *engine-escape*)))
```

让我们来看一下引擎代码的内部。如上所述，`make-engine`接受一个程序并为之构造一个引擎：
```scheme
(define make-engine
  (lambda (th)
    (lambda (ticks success failure)
      (let* ((ticks-left 0)
             (engine-succeeded? #f)
             (result
              (call/cc
               (lambda (k)
                 (set! *engine-escape* k)
                 (let ((result
                        (call/cc
                         (lambda (k)
                           (set! *engine-entrance* k)
                           (clock 'set ticks)
                           (let ((v (th)))
                             (*engine-entrance* v))))))
                   (set! ticks-left (clock 'set *infinity*))
                   (set! engine-succeeded? #t)
                   result)))))
        (if engine-succeeded?
            (success result ticks-left)
            (failure 
             (make-engine 
              (lambda () 
                (result 'resume)))))))))
```

首先我们引入变量`ticks-left`和`engine-succeeded?`。前者保存引擎的程序应该在多少时间片内完成。后者是一个标志，表示引擎是否成功。

我们接下来在两层对`call/cc`的调用中执行引擎的程序。第一个`call/cc`捕获的续延被失败引擎用来退出其引擎的计算。这个续延被保存到全局的`*engine-escape*`变量中。第二个`call/cc`捕获一个内部的续延，该续延会被`th`的返回值使用，如果`th`完成了的话。这个续延保存在全局的`*engine-entrance*`变量中。

查看上面的代码，我们能发现在捕获续延`*engine-escape*`和`*engine-entrance*`后，我们设置时钟的时间片为允许允许的时间并运行`th`。如果`th`成功了，其返回值`v`被发送到续延`*engine-escape*`，然后时钟就停止了，剩下的时间片的数量就确定了，并且标记`engine-succeeded?`被设置为真。我们现在略过(?)`*engine-escape*`续延，并执行最后一段选择语句：由于我们知道引擎成功了，我们以执行结果和剩余的时间片为参数调用`success`过程。

如果程序`th`没能在制定时间完成，就会被中断。这会调用时钟的中断处理器，来捕获当前失败程序的续延，并把它传给`*engine-escape*`变量。这样就把`result`变量设置为失败任务的续延，我们现在执行最后一个`if`语句，由于`engine-succeeded?`是`false`，我们以`result`构造一个新引擎并作为参数调用`failure`过程。

注意当一个失败的引擎被移除时，it will traverse the control path charted by the first run of the original engine.尽管如此，因为我们总是显式的使用保存在`*engine-entrance*`和`*engine-escape*`变量中的续延，而且我们总是在开启引擎计算前重新设置它们，我们能保证跳转总是会到当前执行的引擎代码。

## 15.3 交叠(nestable)的引擎

为了让上面的代码更通用化，适应交叠的引擎，我们需要引入一些时间片管理的机制，来为交叠运行的所有引擎管理正确的时间片。

为了跑一个新引擎（子引擎），我们需要停止当前引擎（父引擎）。然后需要给子引擎分配适当的时间。这可不是直接在程序代码里设置那样，因为给子引擎分配比父引擎剩下的时间片更多的时间片是不对的。在子引擎跑完后我们还得更新父引擎剩余的时间片。如果子引擎在给定时间内跑完，所有它剩下的时间片都还给父引擎。如果子引擎要求的时间片被拒绝（因为父引擎的剩余时间片都无法满足），那么如果子引擎失败了，父引擎也会失败，但是必须记得在重启父引擎时也重启子引擎，其时间片仍然是之前需要的那些。

我们需要用`fluid-let`来声明全局的`*engine-escape*`和`*engine-entrance*`变量，因为每个引擎都必须有它自己的这两个做控制用的续延。当引擎退出时（不论是成功还是失败），`fluid-let`会保证其外层引擎会接管这个控制(sentinel)。

考虑到以上这些，可交叠引擎的代码应该像下面这样：

```scheme
(define make-engine
  (lambda (th)
    (lambda (ticks s f)
      (let* ((parent-ticks 
              (clock 'set *infinity*))

             ;A child can't have more ticks than its parent's
             ;remaining ticks
             (child-available-ticks 
              (clock-min parent-ticks ticks))

             ;A child's ticks must be counted against the parent
             ;too
             (parent-ticks-left
              (clock-minus parent-ticks child-available-ticks))

             ;If child was promised more ticks than parent could
             ;afford, remember how much it was short-changed by
             (child-ticks-left 
              (clock-minus ticks child-available-ticks))

             ;Used below to store ticks left in clock
             ;if child completes in time
             (ticks-left 0)

             (engine-succeeded? #f)

             (result
              (fluid-let ((*engine-escape* #f)
                          (*engine-entrance* #f))
                (call/cc
                 (lambda (k)
                   (set! *engine-escape* k)
                   (let ((result
                          (call/cc
                           (lambda (k)
                             (set! *engine-entrance* k)
                             (clock 'set child-available-ticks)

                             (let ((v (th)))

                               (*engine-entrance* v))))))
                     (set! ticks-left
                       (let ((n (clock 'set *infinity*)))
                         (if (eqv? n *infinity*) 0 n)))
                     (set! engine-succeeded? #t)
                     result))))))

        ;Parent can reclaim ticks that child didn't need
        (set! parent-ticks-left
          (clock-plus parent-ticks-left ticks-left))

        ;This is the true ticks that child has left --
        ;we include the ticks it was short-changed by
        (set! ticks-left 
          (clock-plus child-ticks-left ticks-left))

        ;Restart parent with its remaining ticks
        (clock 'set parent-ticks-left)
        ;The rest is now parent computation

        (cond
         ;Child finished in time -- celebrate its success
         (engine-succeeded? (s result ticks-left))

         ;Child failed because it ran out of promised time --
         ;call failure procedure
         ((= ticks-left 0)
          (f (make-engine (lambda () (result 'resume)))))

         ;Child failed because parent didn't have enough time,
         ;ie, parent failed too.  If so, when parent is
         ;resumed, its first order of duty is to resume the
         ;child with its fair amount of ticks
         (else
          ((make-engine (lambda () (result 'resume)))
           ticks-left s f)))))))
```

注意我们使用算术运算符`clock-min`，`clock-minus`和`clock-plus`替代了`min`，`-`和`+`。这是因为时钟算数的值除了整数之外还包含`*infinity*`。有些Scheme的方言在它们的算数运算中提供了`*infinity*`的值——如果这样的话，你就可以用这些通用的算术运算符了。如果没有的话，定义这几个增强运算符也是很简单的事情。

--------------------------
1. 在Guile中，你可以`(define *infinity* (/ 1 0))`。



















