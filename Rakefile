require 'pp'
require 'ruby-debug'

namespace :gemsets do
  task :prepare do
    unless ENV['VERSIONS']
      puts "Fetching the list of rdoc's versions..."
      ENV['VERSIONS'] = `gem search --remote --all rdoc | egrep '^rdoc '`.chomp
      unless ENV['VERSIONS'] =~ /rdoc \((.*)\)/
        abort "cannot see any rdoc versions."
      end
      ENV['VERSIONS'] = $1
    end

    ENV['VERSIONS'].split(/, /).each do |version|
      puts "Preparing gemset for #{version}..."
      system <<-SHELL
#{ENV['SHELL']} -c "rvm gemset create rdoc#{version} && rvm gemset use rdoc#{version} && gem install --verbose --version=#{version} rdoc"
SHELL
    end
  end

  task :cleanup do
    `rvm gemset list | egrep '^rdoc'`.split.each do |gemset|
      system "rvm --force gemset delete #{gemset}"
    end
  end

end
