第十章 关联表和表格
===================

关联表是Scheme一种特殊形式的列表。列表的每一个元素都是一个点对，其中的car（左边的元素）被称为一个“键”，cdr（右边的元素）被称为和该键关联的值。例如：
```scheme
((a . 1) (b . 2) (c . 3))
```
调用程序`(assv k al)`能在关联表`al`中找到和键`k`关联的CONS单元。在查找时关联表中的键与`k`使用`eqv?`过程来比较。然而有时我们可能希望自定义一个键的比较函数。例如，如果键是不区分大小写的字符串，那默认的`eqv?`就没什么用了。

我们现在定义一个结构`table`(表格)，这是一个改进后的关联表，它可以允许用户在它的键上自定义比较函数。它的字段是`equ`和`alist`。

```scheme
(defstruct table (equ eqv?) (alist '()))
```
（默认的比较函数是`eqv?`——对于一个普通的关联表——关联表的初始化为空。）

我们将使用程序`table-get`得到与一个给定键关联的值（相对于cons单元）。`table-get`接受一个`table`(表格)和一个键作为参数，还有一个可选的默认值，这样若在表格中未找到该键则返回该默认值：
```scheme
(define table-get
  (lambda (tbl k . d)
    (let ((c (lassoc k (table.alist tbl) (table.equ tbl))))
      (cond (c (cdr c))
            ((pair? d) (car d))))))
```

在`table-get`中使用的程序`lassoc`，定义如下：

```scheme
(define lassoc
  (lambda (k al equ?)
    (let loop ((al al))
      (if (null? al) #f
          (let ((c (car al)))
            (if (equ? (car c) k) c
                (loop (cdr al))))))))
```
程序`table-put`用来更新给定表格中的一个键的值：
```scheme
(define table-put!
  (lambda (tbl k v)
    (let ((al (table.alist tbl)))
      (let ((c (lassoc k al (table.equ tbl))))
        (if c (set-cdr! c v)
            (set!table.alist tbl (cons (cons k v) al)))))))
```
程序`table-for-each`为每个表格中键/值对调用给定的程序
```scheme
(define table-for-each
  (lambda (tbl p)
    (for-each
     (lambda (c)
       (p (car c) (cdr c)))
     (table.alist tbl))))
```
