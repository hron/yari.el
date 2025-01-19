;;; test-helper.el --- Helpers for yari-test.el

(require 'yari)

(defmacro yari-with-ruby-obarray-cache-mock (cache-mock &rest body)
  (declare (indent 1))
  `(unwind-protect
	   (let* ((,cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
              (yari-ruby-obarray-cache ,cache-mock))
         ,@body)))

;;; test-helper.el ends here
