;;; yari-test.el --- Tests for yari  -*- lexical-binding: t; -*-

(ert-deftest yari-test-ri-lookup-should-generate-error ()
  (let ((debug-on-error nil))
    (should-error
     (yari-ri-lookup "AbSoLuTttelyImposibleThisexists#bbb?"))))

(ert-deftest yari-test-ri-lookup-should-have-content ()
  (should (string-match "RDoc" (yari-ri-lookup "RDoc"))))

(ert-deftest yari-test-ri-lookup ()
  (should (yari-ri-lookup "RDoc")))

(ert-deftest yari-test-ri-lookup-array-intersection-operator ()
  (should (yari-ri-lookup "Array#&")))

(ert-deftest yari-test-ruby-obarray-should-rehash ()
  (yari-with-ruby-obarray-cache-mock
   cache-mock
   (yari-ruby-obarray t)
   (should-not (equal yari-ruby-obarray-cache cache-mock))))

(ert-deftest yari-test-ruby-obarray-should-set-cache ()
  (let ((yari-ruby-obarray-cache))
    (yari-ruby-obarray)
    (should yari-ruby-obarray-cache)))

(ert-deftest yari-test-ruby-obarray-for-class-first-level ()
  (should (member "RDoc" (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-for-class-deep-level ()
  (should (member "RDoc::TopLevel" (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-for-class-method ()
  (should (member "RDoc::TopLevel::new" (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-for-object-method ()
  (should (member "RDoc::TopLevel#full_name" (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-should-use-cache ()
  (yari-with-ruby-obarray-cache-mock
   cache-mock
   (yari-ruby-methods-from-ri nil)
   (should (equal yari-ruby-obarray-cache cache-mock))))

(ert-deftest yari-test-ruby-obarray-filter-standard-warning ()
  (should-not (member ". not found, maybe you meant:"
                      (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-filter-updating-class-cache ()
  (should-not (let ((case-fold-search nil)
                    (bad-thing-found-p))
                (mapc '(lambda (line)
                         (when (string-match "Updating class cache" line)
				           (setq bad-thing-found-p t)))
                      (yari-ruby-obarray))
                bad-thing-found-p)))

(ert-deftest yari-test-ruby-obarray-filter-empty-string ()
  (should-not (member "" (yari-ruby-obarray))))

(ert-deftest yari-test-ruby-obarray-filter-standard-ruler ()
  (should-not (member "----------------------------------------------"
                      (yari-ruby-obarray))))

(ert-deftest yari-test-get-ri-version-for-1.0.0 ()
  (should (equal "1.0.1" (yari-get-ri-version "ri v1.0.1 - 20041108"))))
(ert-deftest yari-test-get-ri-version-for-2.5.6 ()
  (should (equal "2.5.6" (yari-get-ri-version "ri 2.5.6"))))

(ert-deftest yari-test-command-class ()
  (yari-test-command "String"))

(ert-deftest yari-test-command-method ()
  (yari-test-command "String#chomp"))

(ert-deftest yari-test-command-ampersand-operator ()
  (yari-test-command "Array#&"))

;;; yari-test.el ends here
