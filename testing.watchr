watch('yari.el') { |m| run_all_tests }

def run_all_tests
  system "#{ENV['SHELL']} -c 'rake test'"
end

# --------------------------------------------------
# Signal Handling
# --------------------------------------------------
Signal.trap('INT')  { run_all_tests  } # Ctrl-C
Signal.trap('QUIT') { abort("\n") }    # Ctrl-\
