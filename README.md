
<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">

<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Ольховський Максим Олександрович КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини 12(4)

Алгоритм сортування вставкою No2 (з лінійним пошуком справа) за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
(defun insertion-sort-functional (lst &key (key #'identity) (test #'<))
  "Функціональне сортування вставкою з використанням функцій вищого порядку.
  
  :key - функція для порівняння елементів (за замовчуванням identity).
  :test - функція для перевірки умови сортування (за замовчуванням <)."
  
  (labels ((insert (x sorted-list)
             "Вставляє елемент X у впорядкований список sorted-list, використовуючи key і test."
             (cond
               ((null sorted-list) (list x))
               ((funcall test (funcall key x) (funcall key (car sorted-list)))
                (cons x sorted-list))
               (t (cons (car sorted-list) (insert x (cdr sorted-list))))))

           (recursive-sort (unsorted sorted)
             "Рекурсивно сортує список, додаючи кожен елемент до відсортованого."
             (if (null unsorted)
                 sorted
                 (recursive-sort (cdr unsorted)
                                 (insert (car unsorted) sorted)))))
    
    (recursive-sort lst nil)))
         
```

### Тестові набори та утиліти першої частини

```lisp
(defun check-insertion-sort (name input expected &key (key #'identity) (test #'<))
  "Виконує сортування за допомогою insertion-sort-functional, порівнює результат з expected та виводить статус."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (insertion-sort-functional input :key key :test test) expected)
          name))

(defun test-insertion-sort ()
  "Тестує функцію сортування вставкою."
  (check-insertion-sort "test 1:" '() '())
  (check-insertion-sort "test 2:" '(42) '(42))
  (check-insertion-sort "test 3:" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-insertion-sort "test 4:" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-insertion-sort "test 5:" '(3 1 4 1 5 9 2 6 5) '(1 1 2 3 4 5 5 6 9))
  (check-insertion-sort "test 6:" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)
  (check-insertion-sort "test 7:" 
                       '((1 . "a") (3 . "c") (2 . "b")) 
                       '((1 . "a") (2 . "b") (3 . "c")) :key #'car)
  (check-insertion-sort "test 8:" 
                       '((1 . "a") (3 . "c") (2 . "b")) 
                       '((3 . "c") (2 . "b") (1 . "a")) 
                       :key #'car :test #'>))
```

### Тестування першої частини

```lisp
CL-USER> (test-insertion-sort)
   passed... test 1:
   passed... test 2:
   passed... test 3:
   passed... test 4:
   passed... test 5:
   passed... test 6:
   passed... test 7:
   passed... test 8:
```

## Варіант другої частини (12)

Написати функцію replacer , яка має два основні параметри what і to та два
ключові параметри — test та count . repalcer має повернути функцію, яка при
застосуванні в якості першого аргументу reduce робить наступне: при обході списку з
кінця, кожен елемент списка-аргумента reduce , для якого функція test , викликана з
цим елементом та значенням what , повертає значення t (або не nil ), заміняється
на значення to . Якщо count передане у функцію, заміна виконується count разів.
Якщо count не передане тоді обмежень на кількість разів заміни немає. test має
значення за замовчуванням #'eql . Обмеження, які накладаються на використання
функції-результату replacer при передачі у reduce визначаються розробником (тобто,
наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів
функції reduce from-end та initial-value ).
## Лістинг функції для другого завдання:

```lisp
(defun replacer (what to &key (test #'eql) count)
  "Повертає функцію для використання в reduce, яка замінює елементи списку з кінця.
   WHAT - значення для пошуку,
   TO - значення для заміни,
   TEST - функція порівняння (за замовчуванням #'eql),
   COUNT - кількість замін (якщо не вказано, замінює всі підходящі елементи)."
  (let ((remaining (or count nil))) 
    (lambda (item acc)
      (if (and (or (not remaining) (> remaining 0)) (funcall test item what))
          (progn
            (when remaining (decf remaining)) 
            (cons to acc)) 
          (cons item acc))))) 
```

### Тестові набори та утиліти

```lisp
(defun check-replacer (name input expected &key (test #'eql) (count nil))
  "Execute reduce with replacer on input, compare result with expected and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (reduce (replacer (car input) (cadr input) :test test :count count)
                         input :from-end t :initial-value nil)
                 expected)
          name))

(defun test-replacer ()
  "Run tests for the replacer function."
  (check-replacer "test 1:" '(1 1 1 4) '(2 2 2 4) :test #'eql)
  (check-replacer "test 2:" '(1 1 1 4) '(1 2 2 4) :test #'eql :count 2)
  (check-replacer "test 3:" '(1 2 2 2 3) '(2 2 2 2 3) :test #'eql)
  (check-replacer "test 4:" '(1 1 1 4) '(2 2 2 4) :test #'eql)
  (check-replacer "test 5:" '(1 1 1 4) '(1 2 2 4) :test #'eql :count 1))
```

### Тестування
```lisp
CL-USER> (test-replacer)
passed... test 1:
passed... test 2:
passed... test 3:
passed... test 4:
passed... test 5:
```
