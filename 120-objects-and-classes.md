第12章

对象和类
类描述的是有共同行为的对象的集合。由类描述的对象成为类的一个成员。虽然这些slot会由对象成员以特殊的值填充，但类会指定其对象成员拥有的slot的名称。类同样也指定了应用于对象成员的method。Slot值可以是任何形式，但method的值必须是程序。
类具有继承性。因此，一个类可以是另一个类的子类，我们称之为父类。一个子类不仅有它自己的slot和method，也会继承它的父类的所有slot和method。如果一个类具有和它的父类有相同名称的slot和method，那么子类的slot和method还会被保留。

12.1 一个简单的对象系统
让我们用Scheme来执行一个基本的对象系统。对于每个类，我们只允许有一个父类（单继承性）。如果我们不想指定一个父类，我们可以用#t作为一个“零”父类，既没有slot，也没有method。而#t的父类则认为是它自己。
作为第一个例子，对于slot名字，父类以及method的字段，用结构standard-class来定义类是很有效的。前两个字段我们分别成为slots和superclass。我们将使用两个字段来描述method，用method-names字段来描述类的method的名字列表，用method-vector字段来描述类的method的值的矢量。这是standard-class的定义：
(defstruct standard-class
  slots superclass method-names method-vector)
我们可以用make-standard-class，即standard-class的制造程序来创建一个新的类：
(define trivial-bike-class
  (make-standard-class
   'superclass #t
   'slots '(frame parts size)
   'method-names '()
   'method-vector #()))
这是一个非常简单的类，更加复杂的类将包含非平凡的父类和method，在创建类的过程中我们希望隐藏大量的标准初始化设置。因此我们将定义create-class宏来对make-standard-class进行适当的调用。
(define-macro create-class
  (lambda (superclass slots . methods)
    `(create-class-proc
      ,superclass
      (list ,@(map (lambda (slot) `',slot) slots))
      (list ,@(map (lambda (method) `',(car method)) methods))
      (vector ,@(map (lambda (method) `,(cadr method)) methods)))))
我们将在稍后介绍create-class-proc程序的定义。
通过产生一个基于包含在类中的信息的新的向量，make-instance程序创建了一个类的成员。成员向量的的格式非常简单：它的第一个元素参考类，余下的元素将会被插值。make-instance的参数是包含着一连串成对的量的类，而每一对量是slot名字和slot假定在类成员中的值。
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
这是一个类的实例化的例子：
(define my-bike
  (make-instance trivial-bike-class
                 'frame 'cromoly
                 'size '18.5
                 'parts 'alivio))
这将my-bike程序结合到例子中
#(<trivial-bike-class> cromoly 18.5 alivio)
<trivial‑bike‑class>是一个代表如上定义的trivia-bike-class程序中的值的Scheme数据。
class-of程序反悔了一个实例类：
(define class-of
  (lambda (instance)
    (vector-ref instance 0)))
这里假定class-of的参数是一个类成员，即一个向量，其中第一个元素指定了standard-class中的一些实例。我们可能想使class-of对我们定义的任何类型Scheme对象返回一个合适的值。
(define class-of
  (lambda (x)
    (if (vector? x)
        (let ((n (vector-length x)))
          (if (>= n 1)
              (let ((c (vector-ref x 0)))
                (if (standard-class? c) c #t))
              #t))
        #t)))
没有用standard-class创建Scheme对象的类被认为是#t，零类。
slot-value程序和set!slot-value程序会访问和改变一个类成员的值：
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
我们现在来解决create-class-proc的定义问题。这个程序有一个父类，slot的列表，method名字的列表和method的向量，并适当调用make-standard-class程序。唯一困难的部分是给定的slot字段的值。由于一个类必须包括它的父类的slot，因此不能只是create-class提供的slot参数。我们必须把所提供的slot追加到父类的slot中，并保证没有重复的slot。
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
在列表s中调用的程序delete-duplicates，返回一个只包含s每个元素最后一次出现的新列表。
(define delete-duplicates
  (lambda (s)
    (if (null? s) s
        (let ((a (car s)) (d (cdr s)))
          (if (memv a d) (delete-duplicates d)
              (cons a (delete-duplicates d)))))))
现在谈谈method的应用。我们通过使用send程序调用一个类成员的method。send参数是method的名字，类成员，以及除类成员本身之外的method的参数。由于method是储存在成员的类中而不是成员本身中，因此send将寻找method的成员的类。如果method没有找到，则到父类中寻找，一直寻找到父类链：
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
我们能定义一些更有趣的类：
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
这里，bike-class包括一个名为check-fit的method，这需要一个自行车，一个裤腿的测量方法，以及在这种裤腿下自行车和人的适应性的报告。
我们再来定义my-bike：
(define my-bike
  (make-instance bike-class
                 'frame 'titanium ; I wish
                 'size 21
                 'parts 'ultegra
                 'chain 'sachs
                 'tires 'continental))
检查如尺寸为32的裤腿与某个人是否搭配：
(send 'check-fit my-bike 32)
我们再定义子类bike-class。
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
Mtn-bike-class添加了一个名为suspension的slot。并使用了一个稍微不同的名为check-fit的method定义。

12.2 类也是成员

