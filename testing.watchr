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
(let ((stats (ert-run-tests-batch "^yari-")))
  (kill-emacs (+ (ert-stats-passed-unexpected stats)
                 (ert-stats-failed-unexpected stats)
                 (ert-stats-error-unexpected stats))))
LISP
  lisp_test.close
  lisp_test
end

def run string
  puts "\n#{string}"
  `rvm gemset list | egrep '^rdoc'`.split.sort.reverse.each do |gemset|
    system <<-SHELL || break
#{ENV['SHELL']} -c "rvm gemset use #{gemset} && ri --version && #{string}"
SHELL
    break if $?.exitstatus != 0
  end
end

# --------------------------------------------------
# Signal Handling
# --------------------------------------------------
Signal.trap('INT')  { run_all_tests  } # Ctrl-C
Signal.trap('QUIT') { abort("\n") }    # Ctrl-\
