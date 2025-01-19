;;; test-helper.el --- Helpers for yari-test.el

(require 'yari)

(defmacro yari-with-ruby-obarray-cache-mock (cache-mock &rest body)
  (declare (indent 1))
  `(unwind-protect
	   (let* ((,cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
              (yari-ruby-obarray-cache ,cache-mock))
         ,@body)))

(defun yari-test-command (name)
  (let* ((mock (lambda (&rest _args) name)))
    (cl-letf (((symbol-function 'completing-read) mock)
              ((symbol-function 'ido-completing-read) mock))
      (with-temp-buffer
        (yari)
        (switch-to-buffer (format "*yari %s*" name))
        (should (string-prefix-p name
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point-max))))))))

;;; test-helper.el ends here
