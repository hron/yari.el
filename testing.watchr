require 'tempfile'

watch('yari.el') { |m| run_all_tests }

def run_all_tests
  run "emacs -Q --script #{lisp_test_file.path}"
end

def lisp_test_file
  lisp_test = Tempfile.new('ri-tests-el')
  lisp_test << <<-LISP
(mapc '(lambda (path)
         (add-to-list 'load-path (expand-file-name path)))
      '("." "./vendor"))
(require 'ert)
(load "yari.el")
(ert-run-tests-batch "^yari-")
LISP
  lisp_test.close
  lisp_test
end

def run string
  puts "\n#{string}"
  system string
end

# --------------------------------------------------
# Signal Handling
# --------------------------------------------------
Signal.trap('INT')  { run_all_tests  } # Ctrl-C
Signal.trap('QUIT') { abort("\n") }    # Ctrl-\
