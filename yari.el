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

(defgroup yari nil
  "Yet Another Ri Interface."
  :group 'programming)

(defcustom yari-mode-hook nil
  "Hooks to run when invoking yari-mode."
  :group 'yari
  :type 'hook)

;;;###autoload
(defun yari (&optional ri-topic rehash)
  "Look up Ruby documentation."
  (interactive (list nil current-prefix-arg))
  (setq ri-topic (or ri-topic (completing-read "yari: "
                                               (yari-ruby-obarray rehash)
                                               nil
                                               t
                                               (yari-symbol-at-point))))
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

(defun yari-symbol-at-point ()
  ;; TODO: make this smart about class/module at point
  (let ((yari-symbol (symbol-at-point)))
    (if yari-symbol
        (symbol-name yari-symbol)
      "")))

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

(defmacro when-ert-loaded (&rest body)
  `(when (featurep 'ert)
     ,@body))

(when-ert-loaded
 (defmacro yari-with-ruby-obarray-cache-mock (cache-mock &rest body)
   (declare (indent 1))
   `(unwind-protect
	(let* ((,cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
               (yari-ruby-obarray-cache ,cache-mock))
          ,@body))))


(defun yari-ri-lookup (name)
  "Return content from ri for NAME."
  (assert (member name (yari-ruby-obarray)) nil
          (format "%s is unknown symbol to RI." name))
  (shell-command-to-string
   (format "ri -T %s" (shell-quote-argument name))))

(when-ert-loaded
 (ert-deftest yari-test-ri-lookup-should-generate-error ()
   (ert-should-error
    (yari-ri-lookup "AbSoLuTttelyImposibleThisexists#bbb?")))

 (ert-deftest yari-test-ri-lookup-should-have-content ()
   (ert-should (string-match "RDoc - Ruby Documentation System"
                             (yari-ri-lookup "RDoc"))))

 (ert-deftest yari-test-ri-lookup ()
   (ert-should (yari-ri-lookup "RDoc"))))


(defvar yari-ruby-obarray-cache nil
  "Variable to store all possible completions of RI pages.")

(defun yari-ruby-obarray (&optional rehash)
  "Build collection of classes and methods for completions."
  (if (and (null rehash) (consp yari-ruby-obarray-cache))
      ;; TODO: I do not know how to return from here properly... ;]
      (setq yari-ruby-obarray-cache yari-ruby-obarray-cache)
    (let* ((methods (yari-ruby-methods-from-ri))
           (classes (yari-ruby-classes-from-ri)))
      (setq yari-ruby-obarray-cache
            (delete-dups (append methods classes))))))

(when-ert-loaded
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

 (ert-deftest yari-test-ruby-obarray-for-class-first-level ()
   (ert-should (member "RDoc" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-class-deep-level ()
   (ert-should (member "RDoc::RI::Driver" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-class-method ()
   (ert-should (member "RDoc::RI::Driver::new" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-object-method ()
   (ert-should (member "RDoc::RI::Driver#parse_name" (yari-ruby-obarray)))))


(defun yari-get-all-modules (name)
  "Return class or module names for RDoc::RI::Driver#run or
  RDoc::RI::Driver::new."
  (let* ((case-fold-search . nil)
         ;; remove class/object method if exists.
         (module-name (car (split-string name "\\(::\\|#\\)[a-z]")))
         (variants))
    (while (not (string= "" module-name))
      (add-to-list 'variants module-name)
      (setq module-name
            (if (string-match "\\(.*\\)::\\([^:]+\\)" module-name)
		(match-string 1 module-name)
              "")))
    variants))

(when-ert-loaded
 (ert-deftest yari-test-get-all-modules ()
   (ert-should (equal '("Array")
                      (yari-get-all-modules "Array::new")))
   (ert-should (equal '("Array")
                      (yari-get-all-modules "Array#size")))
   (ert-should (equal '("RDoc" "RDoc::RI" "RDoc::RI::Driver")
                      (yari-get-all-modules "RDoc::RI::Driver")))
   (ert-should (equal '("RDoc" "RDoc::RI" "RDoc::RI::Driver")
                      (yari-get-all-modules "RDoc::RI::Driver#run")))
   (ert-should (equal '("RDoc" "RDoc::RI" "RDoc::RI::Driver")
                      (yari-get-all-modules "RDoc::RI::Driver::new")))))


(defun yari-ruby-methods-from-ri ()
  "Return list with all ruby methods known to ri command."
  (cond ((yari-ri-version-at-least "2.5")
         (delete-if '(lambda (name)
                         (or (string= ". not found, maybe you meant:" name)
                             (string= "" name)))
                      (split-string (shell-command-to-string "ri -T '.'")
                                    "\n")))
	(t
         (let ((ruby-code "require 'rdoc/ri/reader'; \
                           require 'rdoc/ri/cache';  \
                           require 'rdoc/ri/paths';  \
                           all_paths = RDoc::RI::Paths.path(true,true,true,true); \
                           cache  = RDoc::RI::Cache.new(all_paths); \
                           reader = RDoc::RI::Reader.new(cache);    \
                           puts reader.all_names"))
           (split-string (yari-eval-ruby-code ruby-code))))))


(when-ert-loaded
 (ert-deftest yari-test-ruby-methods-from-ri-filter-standard-warning ()
   (ert-should-not (member ". not found, maybe you meant:"
                           (yari-ruby-methods-from-ri))))

 (ert-deftest yari-test-ruby-methods-from-ri-filter-empty-string ()
   (ert-should-not (member "" (yari-ruby-methods-from-ri))))

 (ert-deftest yari-test-ruby-methods-from-ri-filter-standard-ruler ()
   (ert-should-not (member "----------------------------------------------"
                           (yari-ruby-methods-from-ri)))))

(defun yari-ruby-classes-from-ri ()
  "Return list with all ruby classes/modules know to ri command."
    (cond ((yari-ri-version-at-least "2.5")
           (let ((ruby-code "require 'rdoc/ri/driver';       \
                             driver  = RDoc::RI::Driver.new; \
                             classes = [];                   \
                             driver.stores.each do |store|   \
                               classes << store.modules;     \
                             end;                            \
                             puts classes.flatten.uniq"))
             (split-string (yari-eval-ruby-code ruby-code))))
          (t
           (delq nil
                 (mapcar '(lambda (name) (car (split-string name)))
                         (cddr (split-string
				(shell-command-to-string "ri -T") "[ \n,]+")))))))

(when-ert-loaded
 (ert-deftest yari-test-ruby-classes-from-ri ()
   (yari-with-ruby-obarray-cache-mock cache-mock
     (ert-should (member "RDoc" (yari-ruby-classes-from-ri))))))

(defun yari-eval-ruby-code (ruby-code)
  "Return stdout from ruby -rrubyges -eRUBY-CODE."
  (shell-command-to-string (format "ruby -rrubygems -e\"%s\"" ruby-code)))

(defun yari-get-ri-version ()
  "Return list of version parts or RI."
  (cadr (split-string
         (shell-command-to-string "ri --version"))))

(defun yari-ri-version-at-least (minimum)
  "Detect if RI version at least MINIMUM."
  (let ((ri-version (yari-get-ri-version)))
    (or (string< minimum ri-version) (string= minimum ri-version))))

(provide 'yari)
;;; yari.el ends here
