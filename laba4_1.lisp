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