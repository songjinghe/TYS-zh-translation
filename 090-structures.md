第九章 结构
=============

自然分组的数据被称为结构。它能够使用Scheme复合数据结构如向量和列表来代表结构。例如：我们正在处理与一棵（植物）树相似的分组数据。数据或字段中的单个元素则是：高度，周长，年龄，树叶的形状和树叶的颜色，总计5个字段。这样的数据可以表示为5元向量。这些字段可以利用vvector-ref访问，或使用vector-set修饰。
尽管如此，我们却非常不希望一直去记忆哪个向量索引对应哪个字段，这将是一个吃力不讨好而且易出错的活动，尤其是在字段包含或排除在时间的过程中。
因此我们使用Scheme的宏defstruct去定义一个结构数据类型，基本上都是一个向量，但是也是创建结构实例的一个适当套件程序，用来访问或修饰它的字段。因此，我们的树结构应这样定义：
(defstruct tree height girth age leaf-shape leaf-color)
这提供了我们一个名为make-tree的构造过程；每个字段的访问程序，命名为tree.height，tree.girth等等。构造的方法如下：
(define coconut 
  (make-tree 'height 30
             'leaf-shape 'frond
             'age 5))
这个构造函数的参数以成对的形式出现，字段名紧跟其初始化出现。这些字段能以任意顺序出现，在字段的值没有被定义时也可以忽略。
访问程序的调用如下所示：
(tree.height coconut) =>  30
(tree.leaf-shape coconut) =>  frond
(tree.girth coconut) =>  <undefined>
tree.girth存取程序返回一个未定义的值，因为我们没有为coconut tree指定girth。
修饰程序的调用如下所示：
(set!tree.height coconut 40)
(set!tree.girth coconut 10)
如果我们现在用相应的访问程序去访问这些字段，我们会得到新的值：
(tree.height coconut) =>  40
(tree.girth coconut) =>  10

9.1 默认初始化
我们可以在定义构造函数时进行一些初始化的设置，而不是在每个实例中都进行。因此，我们假定leaf-shape和leaf-color在默认情况下分别为frond和green。我们可以在调用make-tree时利用明显的初始化来覆盖这些默认值，或者在创建一个结构实例后使用一个字段修饰器：
(defstruct tree height girth age
                (leaf-shape 'frond)
                (leaf-color 'green))

(define palm (make-tree 'height 60))

(tree.height palm) 
=>  60

(tree.leaf-shape palm) 
=>  frond

(define plantain 
  (make-tree 'height 7
             'leaf-shape 'sheet))

(tree.height plantain) 
=>  7

(tree.leaf-shape plantain) 
=>  sheet

(tree.leaf-color plantain) 
=>  green

9.2 defstruct定义
宏defstruct的定义如下：
(define-macro defstruct
  (lambda (s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i 
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol 
                       (string-append "make-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i 
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st 
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol 
                                         (string-append
                                          s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append 
                                           "set!" s-s "." f))
                                  (lambda (x v) 
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s))))))))))

