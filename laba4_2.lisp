(defun replacer (what to &key (test #'eql) count)
  "Повертає функцію для використання в reduce, яка замінює елементи списку з кінця.
   WHAT - значення для пошуку,
   TO - значення для заміни,
   TEST - функція порівняння (за замовчуванням #'eql),
   COUNT - кількість замін (якщо не вказано, замінює всі підходящі елементи)."
  (let ((remaining (or count nil))) ; Змінна для відслідковування кількості замін
    (lambda (item acc)
      (if (and (or (not remaining) (> remaining 0)) (funcall test item what))
          (progn
            (when remaining (decf remaining)) ; Зменшуємо лічильник замін
            (cons to acc)) ; Замінюємо елемент
          (cons item acc))))) ; Додаємо елемент без заміни

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