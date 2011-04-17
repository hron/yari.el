require 'tempfile'

RUBY_FOR_RDOC = {
  '2.1.0' => '1.8.7',
  '2.0.0' => '1.8.7'
}
RUBY_FOR_RDOC.default = '1.9.2'

namespace :gemsets do
  desc "Prepare gemsets with different versions of RDoc."
  task :prepare do
    unless ENV['VERSIONS']
      puts "Fetching the list of rdoc's versions..."
      ENV['VERSIONS'] = `gem search --remote --all rdoc | egrep '^rdoc '`.chomp
      unless ENV['VERSIONS'] =~ /rdoc \((.*)\)/
        abort "cannot see any rdoc versions."
      end
      ENV['VERSIONS'] = $1.gsub(/ ruby/, '')
    end


    ENV['VERSIONS'].split(/, /).each do |version|
      ruby_version = RUBY_FOR_RDOC[version]
      bash "rvm #{ruby_version} &&                \
              rvm gemset create rdoc#{version} && \
              rvm gemset use rdoc#{version} &&    \
              gem install --verbose --version=#{version} rdoc"
    end
  end

  desc "Remove all gemsets created for testing against different versions of RDoc."
  task :cleanup do
    [ '1.9.2', '1.8.7' ].each do |ruby_version|
      cmd = "rvm #{ruby_version} && rvm gemset list | egrep '^rdoc'"
      output_for(cmd).each do |gemset|
        bash "rvm #{ruby_version} && rvm --force gemset delete #{gemset}"
      end
    end
  end

end

desc "Test yari for ri VERSIONS=x,x,x."
task :test do
  versions = []
  if ENV['VERSIONS']
    versions = ENV['VERSIONS'].split /,/
  else
    [ '1.9.2', '1.8.7' ].each do |ruby_version|
      output_for("rvm #{ruby_version} && rvm gemset list").each do |gemset|
        next unless gemset =~ /rdoc(.*)/
        versions << $1
      end
    end
  end

  versions.sort.reverse.each do |v|
    ruby_version = RUBY_FOR_RDOC[v]
    next unless ruby_version

    bash "rm #{ENV['HOME']}/.ri/cache/classes"
    bash "rvm #{ruby_version} && rvm gemset use rdoc#{v} && #{yari_tests}"
    exit if $?.exitstatus != 0
  end
end

def yari_tests
  "emacs -Q --script #{lisp_test_file.path}"
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

def output_for string
  IO.popen("#{ENV['SHELL']} -c '#{string}'").readlines
end

def bash string
  puts string.gsub /[[:space:]]+/, ' '
  system "#{ENV['SHELL']} -c '#{string}'"
end
