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

(require 'thingatpt)

(defvar yari-mode-hook nil
  "Hooks to run when invoking yari-mode.")

;;;###autoload
(defun yari (&optional ri-topic rehash)
  "Look up Ruby documentation."
  (interactive (list nil current-prefix-arg))
  (setq ri-topic (or ri-topic (completing-read "yari: "
                                               (yari-ruby-obarray rehash))))
  (let ((yari-buffer-name (format "*yari %s*" ri-topic)))
    (unless (get-buffer yari-buffer-name)
      (let ((yari-buffer (get-buffer-create yari-buffer-name))
            (ri-content (yari-ri-lookup ri-topic)))
        (display-buffer yari-buffer)
        (with-current-buffer yari-buffer
          (erase-buffer)
          (insert ri-content)
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-min))
          (yari-mode))))
    (display-buffer yari-buffer-name)))

(defun yari-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (use-local-map yari-mode-map)
  (setq mode-name "yari")
  (setq major-mode 'yari-mode)
  (setq buffer-read-only t)
  (run-hooks 'yari-mode-hook))

(defvar yari-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q")    'quit-window)
    (define-key map (kbd "SPC")  'scroll-up)
    (define-key map (kbd "\C-?") 'scroll-down)
    map))

(defun yari-ri-lookup (name)
  "Return content from ri for NAME."
  (assert (member name (yari-ruby-obarray)) nil
          (format "%s is unknown symbol to RI." name))
  (shell-command-to-string
   (format "ri -T %s" (shell-quote-argument name))))

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
  (ert-deftest yari-test-ri-lookup-should-generate-error ()
    (ert-should-error
     (yari-ri-lookup "AbSoLuTttelyImposibleThisexists#bbb?")))

  (ert-deftest yari-test-ri-lookup-should-have-content ()
    (ert-should (string-match "Array < Object" (yari-ri-lookup "Array"))))

  (ert-deftest yari-test-ri-lookup ()
    (ert-should (yari-ri-lookup "Array")))

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
