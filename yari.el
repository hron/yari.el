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

(defvar yari-collection-cache nil
  "Variable to store all possible completions of RI pages.")

(defun yari-ruby-obarray (&optional rehash)
  "Build collection of classes and methods for completions."
  (if (not (null yari-collection-cache))
      ;; TODO: I do not know how to return from here properly... ;]
      (setq yari-collection-cache yari-collection-cache)
    (let* ((methods (split-string (shell-command-to-string "ri -T '.'")))
           (classes (delete-dups (mapcar '(lambda (m)
                                            (car (split-string m "#\\|::")))
                                         methods))))
      (setq yari-collection-cache (append methods classes)))))

;;; Tests:

(when (featurep 'ert)
  ;; (ert-deftest yari-test-ruby-obarray-should-filter-errors ()

  (ert-deftest yari-test-ruby-obarray-should-use-cache ()
    (let* ((cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
           (yari-collection-cache cache-mock))
      (yari-ruby-obarray)
      (ert-should (equal yari-collection-cache cache-mock))))

  (ert-deftest yari-test-ruby-obarray-should-set-cache ()
    (let ((yari-collection-cache))
      (yari-ruby-obarray)
      (ert-should yari-collection-cache)))

  (ert-deftest yari-test-ruby-obarray-for-class ()
    (ert-should (member "Array" (yari-ruby-obarray))))

  (ert-deftest yari-test-ruby-obarray-for-class-method ()
    (ert-should (member "Array::new" (yari-ruby-obarray))))

  (ert-deftest yari-test-ruby-obarray-for-object-method ()
    (ert-should (member "Array#size" (yari-ruby-obarray)))))

(provide 'yari)
;;; yari.el ends here
