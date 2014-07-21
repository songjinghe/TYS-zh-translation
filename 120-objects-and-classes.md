第十二章 对象和类
===================

类是描述了一组有共同行为的对象。由类描述的对象称为类的一个实例。类指定了其实例拥有的`属性`（原文为slot卡槽）的名称，而这些`属性`的值由实例自身来进行填充。类同样也指定了可以应用于其实例的`方法`(method)。属性值可以是任何形式，但方法的值必须是过程。

类具有继承性。因此，一个类可以是另一个类的子类，我们称另一个类为它的父类。一个子类不仅有它自己“直接的”属性和方法，也会继承它的父类的所有属性和方法。如果一个类里有与其父类相同名称的属性和方法，那么仅保留子类的属性和方法。

## 12.1 一个简单的对象系统

现在我们用Scheme来实现一个基本的对象系统。对于每个类，我们只允许有一个父类（单继承性）。如果我们不想指定一个父类，我们可以用`#t`作为一个“元”父类，既没有属性，也没有方法。而`#t`的父类则认为是它自己。

作为一次尝试，用结构`standard-class`来定义类应该是很好的一种方式，用结构的字段来保存属性名字，父类以及方法。前两个字段我们分别叫做`slots`和`superclass`。我们将使用两个字段来描述方法，用`method-names`字段来描述类的方法的名称列表，用`method-vector`字段来保存一个矢量，里面放着类的方法。这是`standard-class`的定义：

```scheme
(defstruct standard-class
  slots superclass method-names method-vector)
```

我们可以用`make-standard-class`，即`standard-class`的制造程序(见第九章)来创建一个新的类：

```scheme
(define trivial-bike-class
  (make-standard-class
   'superclass #t
   'slots '(frame parts size)
   'method-names '()
   'method-vector #()))
```

这是一个非常简单的类，更加复杂的类会有有意义的父类和方法，这需要在创建类时进行大量的初始化设置，我们希望把这些工作隐藏在创建类的过程中。因此我们定义一个`create-class`宏来对`make-standard-class`进行适当的调用。

```scheme
(define-macro create-class
  (lambda (superclass slots . methods)
    `(create-class-proc
      ,superclass
      (list ,@(map (lambda (slot) `',slot) slots))
      (list ,@(map (lambda (method) `',(car method)) methods))
      (vector ,@(map (lambda (method) `,(cadr method)) methods)))))
```

我们稍后再介绍`create-class-proc`程序的定义。

`make-instance`程序创建类的一个实例，由类中包含的信息产生一个新的向量。实例向量的格式非常简单：它的第一个元素指向这个类（引用），余下的元素都是属性值。`make-instance`的第一个参数是一个类，后面的参数是成对的序列，而每一个“对”是属性名称和该实例中属性的值。

```scheme
(define make-instance
  (lambda (class . slot-value-twosomes)

    ;Find `n', the number of slots in `class'.
    ;Create an instance vector of length `n + 1',
    ;because we need one extra element in the instance
    ;to contain the class.

    (let* ((slotlist (standard-class.slots class))
           (n (length slotlist))
           (instance (make-vector (+ n 1))))
      (vector-set! instance 0 class)

      ;Fill each of the slots in the instance
      ;with the value as specified in the call to
      ;`make-instance'.

      (let loop ((slot-value-twosomes slot-value-twosomes))
        (if (null? slot-value-twosomes) instance
            (let ((k (list-position (car slot-value-twosomes) 
                                    slotlist)))
              (vector-set! instance (+ k 1) 
                (cadr slot-value-twosomes))
              (loop (cddr slot-value-twosomes))))))))
```

这是一个类的实例化的例子：

```scheme
(define my-bike
  (make-instance trivial-bike-class
                 'frame 'cromoly
                 'size '18.5
                 'parts 'alivio))
```

这将`my-bike`变量绑定到如下所示的实例上。

```scheme
#(<trivial-bike-class> cromoly 18.5 alivio)
```

`<trivial‑bike‑class>`是一个Scheme数据(另一个向量)代表之前定义的`trivia-bike-class`的值。

`class-of`程序返回该实例对应的类：

```scheme
(define class-of
  (lambda (instance)
    (vector-ref instance 0)))
```

这里假定`class-of`的参数是一个类的实例，即一个向量，其第一个元素指向`standard-class`的一些实例。我们可能想使`class-of`对我们给定的任何类型Scheme对象返回一个合适的值。

```scheme
(define class-of
  (lambda (x)
    (if (vector? x)
        (let ((n (vector-length x)))
          (if (>= n 1)
              (let ((c (vector-ref x 0)))
                (if (standard-class? c) c #t))
              #t))
        #t)))
```

不是用`standard-class`创建的Scheme对象的类被认为是`#t`，即“元类”。

`slot-value`过程和`set!slot-value`过程用来访问和改变一个类实例的值：

```scheme
(define slot-value
  (lambda (instance slot)
    (let* ((class (class-of instance))
           (slot-index
            (list-position slot (standard-class.slots class))))
      (vector-ref instance (+ slot-index 1)))))

(define set!slot-value
  (lambda (instance slot new-val)
    (let* ((class (class-of instance))
           (slot-index
            (list-position slot (standard-class.slots class))))
      (vector-set! instance (+ slot-index 1) new-val))))
```

我们现在来解决`create-class-proc`的定义问题。这个过程接受一个父类，一个属性的列表，一个方法名称的列表和一个包含方法体的向量，并适当调用`make-standard-class`程序。唯一困难的部分是给定的属性字段的值。由于一个类必须包括它的父类的属性，因此不能只有`create-class`提供的属性参数。我们必须把所给的属性追加到父类的属性中，并保证没有重复的属性。

```scheme
(define create-class-proc
  (lambda (superclass slots method-names method-vector)
    (make-standard-class
     'superclass superclass
     'slots
     (let ((superclass-slots 
            (if (not (eqv? superclass #t))
                (standard-class.slots superclass)
                '())))
       (if (null? superclass-slots) slots
           (delete-duplicates
            (append slots superclass-slots))))
     'method-names method-names
     'method-vector method-vector)))
```

过程`delete-duplicates`接受一个列表`s`为参数，返回一个新列表，该列表只包含`s`中每个元素的最后一次出现。

```scheme
(define delete-duplicates
  (lambda (s)
    (if (null? s) s
        (let ((a (car s)) (d (cdr s)))
          (if (memv a d) (delete-duplicates d)
              (cons a (delete-duplicates d)))))))
```

现在谈谈方法的应用。我们通过使用`send`程序调用一个类实例的方法。`send`的参数是方法的名字，紧接着是类实例，以及除了类实例本身之外的该方法的其他参数。由于方法储存在实例的类中而不是在实例本身中，因此`send`会在该实例对于的类中寻找该方法。如果没有找到，则到父类中寻找，如此直到找完整个继承链：

```scheme
(define send
  (lambda (method instance . args)
    (let ((proc
           (let loop ((class (class-of instance)))
             (if (eqv? class #t) (error 'send)
                 (let ((k (list-position 
                           method
                           (standard-class.method-names class))))
                   (if k
                       (vector-ref (standard-class.method-vector class) k)
                       (loop (standard-class.superclass class))))))))
      (apply proc instance args))))
```

我们现在可以定义一些更有趣的类了：

```scheme
(define bike-class
  (create-class
   #t
   (frame size parts chain tires)
   (check-fit (lambda (me inseam)
                (let ((bike-size (slot-value me 'size))
                      (ideal-size (* inseam 3/5)))
                  (let ((diff (- bike-size ideal-size)))
                    (cond ((<= -1 diff 1) 'perfect-fit)
                          ((<= -2 diff 2) 'fits-well)
                          ((< diff -2) 'too-small)
                          ((> diff 2) 'too-big))))))))
```

这里，`bike-class`包括一个名为`check-fit`的方法，它接受一个自行车的实例和一个裤腿的尺寸作为参数，并报告该车对这种裤腿尺寸的人的适应性。

我们再来定义`my-bike`：

```scheme
(define my-bike
  (make-instance bike-class
                 'frame 'titanium ; I wish
                 'size 21
                 'parts 'ultegra
                 'chain 'sachs
                 'tires 'continental))
```

检查这个车与裤腿尺寸为32的某个人是否搭配：

```scheme
(send 'check-fit my-bike 32)
```

我们再定义子类`bike-class`。

```scheme
(define mtn-bike-class
  (create-class
    bike-class
    (suspension)
    (check-fit (lambda (me inseam)
                (let ((bike-size (slot-value me 'size))
                      (ideal-size (- (* inseam 3/5) 2)))
                  (let ((diff (- bike-size ideal-size)))
                    (cond ((<= -2 diff 2) 'perfect-fit)
                          ((<= -4 diff 4) 'fits-well)
                          ((< diff -4) 'too-small)
                          ((> diff 4) 'too-big))))))))
```

`Mtn-bike-class`添加了一个名为`suspension`的属性。并定义了一个稍微不同的名为`check-fit`的方法。

## 12.2 类也是实例

到这里为止，精明的读者可能已经发现了：类本身可以是某些其他类（如“元类”）的实例。注意所有类都有一些相同的特点：每个都有属性、父类、方法名称的列表和包含方法体的向量。`make-instance`看起来像是他们所共享的方法。这意味着我们可以通过另一个类（当然也是某个类的实例啦）来指定这些共同的特点。

具体的说就是我们可以重写我们的类实现并实现其自身（好别扭）。使用面向对象的方法，这样我们可以确保不会遇到鸡生蛋，蛋生鸡的问题。这样我们会跳出`class`结构和它相关的过程并余下的方法来把类定义为对象。

我们现在把`standard-class`作为其他类的父类。特别的，`standard-class`必须是它自己的一个实例。那么`standard-class`应该是什么样子的呢？

我们知道`standard-class`是一个实例，而且我们用一个向量来表示这个实例。所以最终是一个向量，其第一个元素是它的父类，也就是它自己，而余下的元素是属性值。我们已经确定有四个所有类都必须有的属性，因此`standard-class`是一个5个元素的向量。

```scheme
(define standard-class
  (vector 'value-of-standard-class-goes-here
          (list 'slots
                'superclass
                'method-names
                'method-vector)
          #t
          '(make-instance)
          (vector make-instance)))
```

注意到`standard-class`这个向量并没有被完全填充：符号`value‑of‑standard‑class‑goes‑here`此时仅仅做占位用。现在我们已经定义了一个`standard-class`的值，现在我们可以用它来确定它自己的类，即它本身。

```scheme
(vector-set! standard-class 0 standard-class)
```

注意我们不能用`class`结构提供的过程了。我们必须把下面的形式：

```scheme
(standard-class? x)
(standard-class.slots c)
(standard-class.superclass c)
(standard-class.method-names c)
(standard-class.method-vector c)
(make-standard-class ...)
```
换成：

```scheme
(and (vector? x) (eqv? (vector-ref x 0) standard-class))
(vector-ref c 1)
(vector-ref c 2)
(vector-ref c 3)
(vector-ref c 4)
(send 'make-instance standard-class ...)
```

## 12.3 多重继承

我们可以容易的修改这个对象系统使类可以有一个以上的父类。我们重新定义`standard‑class`来添加一个属性叫`class‑precedence‑list`取代`superclass`，一个类的`class‑precedence‑list`是它所有父类的列表，而不只有通过`create-class`创建时指定的“直接”的父类。从这个名字可以看出其超类是以一种特定的顺序来存放的，前面的超类有比后面超类更高的优先级。

```scheme
(define standard-class
  (vector 'value-of-standard-class-goes-here
          (list 'slots 'class-precedence-list 'method-names 'method-vector)
          '()
          '(make-instance)
          (vector make-instance)))
```

不仅属性列表改变来存放新的属性，而且`superclass`属性也从`#t`变为`()`，这是因为`standard‑class`的`class‑precedence‑list`必须是一个列表。我们可以令它的值为`(#t)`，但是我们不会提到元类，由于它在每个类的`class‑precedence‑list`中。

宏`create-class`也需要修改来接受一个超类的列表而不是一个单独的超类。

```scheme
(define-macro create-class
  (lambda (direct-superclasses slots . methods)
    `(create-class-proc
      (list ,@(map (lambda (su) `,su) direct-superclasses))
      (list ,@(map (lambda (slot) `',slot) slots))
      (list ,@(map (lambda (method) `',(car method)) methods))
      (vector ,@(map (lambda (method) `,(cadr method)) methods))
      )))
```

`create‑class‑proc`必须根据提供的超类给出类的优先级列表，并根据优先级给出属性列表：

```scheme
(define create-class-proc
  (lambda (direct-superclasses slots method-names method-vector)
    (let ((class-precedence-list
           (delete-duplicates
            (append-map
             (lambda (c) (vector-ref c 2))
             direct-superclasses))))
      (send 'make-instance standard-class
            'class-precedence-list class-precedence-list
            'slots
            (delete-duplicates
             (append slots (append-map
                            (lambda (c) (vector-ref c 1))
                            class-precedence-list)))
            'method-names method-names
            'method-vector method-vector))))
```

过程`append-map`是一个`append`和`map`的组合：

```scheme
(define append-map
  (lambda (f s)
    (let loop ((s s))
      (if (null? s) '()
          (append (f (car s))
                  (loop (cdr s)))))))
```

过程`send`在寻找一个方法时必须从左到右搜索类的优先级列表：

```scheme
(define send
  (lambda (method-name instance . args)
    (let ((proc
           (let ((class (class-of instance)))
             (if (eqv? class #t) (error 'send)
                 (let loop ((class class)
                            (superclasses (vector-ref class 2)))
                   (let ((k (list-position 
                             method-name
                             (vector-ref class 3))))
                     (cond (k (vector-ref 
                               (vector-ref class 4) k))
                           ((null? superclasses) (error 'send))
                           (else (loop (car superclasses)
                                       (cdr superclasses))))
                     ))))))
      (apply proc instance args))))
```

--------------------------

理论上我们可以把方法也定义为属性（值为一个过程），但是有很多理由不这样做，类的实例共享方法但是通常有不同的属性值。也就是说，方法可以包括在类定义中，而且不需要每次实例化时都进行设置——就像属性那样。
