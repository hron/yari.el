;;; yari.el --- Yet Another RI interface for Emacs.

;; Copyright (C) 2010  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar yari-ruby-obarray-cache nil
  "Variable to store all possible completions of RI pages.")

(defun yari-ruby-obarray (&optional rehash)
  "Build collection of classes and methods for completions."
  (if (and (null rehash) (consp yari-ruby-obarray-cache))
      ;; TODO: I do not know how to return from here properly... ;]
      (setq yari-ruby-obarray-cache yari-ruby-obarray-cache)
    (let* ((methods (yari-ruby-methods-from-ri))
           (classes (delete-dups (mapcar '(lambda (m)
                                            (car (split-string m "#\\|::")))
                                         methods))))
      (setq yari-ruby-obarray-cache (append methods classes)))))

(defun yari-ruby-methods-from-ri ()
  "Return list with all ruby methods known to ri command."
  (delete ". not found, maybe you meant:"
          (delete ""
                  (split-string (shell-command-to-string "ri -T '.'") "\n"))))

;;; Tests:

(when (featurep 'ert)
  (ert-deftest yari-test-ruby-methods-from-ri-filter-standard-warning ()
    (ert-should-not (member ". not found, maybe you meant:"
                            (yari-ruby-methods-from-ri))))

  (ert-deftest yari-test-ruby-methods-from-ri-filter-empty-string ()
    (ert-should-not (member "" (yari-ruby-methods-from-ri))))

  (ert-deftest yari-test-ruby-obarray-should-rehash ()
    (yari-with-ruby-obarray-cache-mock cache-mock
      (yari-ruby-obarray t)
      (ert-should-not (equal yari-ruby-obarray-cache cache-mock))))

  (ert-deftest yari-test-ruby-obarray-should-use-cache ()
    (yari-with-ruby-obarray-cache-mock cache-mock
      (yari-ruby-obarray)
      (ert-should (equal yari-ruby-obarray-cache cache-mock))))

  (ert-deftest yari-test-ruby-obarray-should-set-cache ()
    (let ((yari-ruby-obarray-cache))
      (yari-ruby-obarray)
      (ert-should yari-ruby-obarray-cache)))

  (ert-deftest yari-test-ruby-obarray-for-class ()
    (ert-should (member "Array" (yari-ruby-obarray))))

  (ert-deftest yari-test-ruby-obarray-for-class-method ()
    (ert-should (member "Array::new" (yari-ruby-obarray))))

  (ert-deftest yari-test-ruby-obarray-for-object-method ()
    (ert-should (member "Array#size" (yari-ruby-obarray))))

  (defmacro yari-with-ruby-obarray-cache-mock (cache-mock &rest body)
    (declare (indent 1))
    `(unwind-protect
         (let* ((,cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
		(yari-ruby-obarray-cache ,cache-mock))
           ,@body))))

(provide 'yari)
;;; yari.el ends here
