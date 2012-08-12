# Run `guard` in the project root to have it watch for changes
# and compile and test the system. Requires `gem install guard-shell`.
# Tests can be run manually by a simple `make`.
guard :shell do
  watch /.*/ do |m|
    if m[0] =~ /^lib/ || m[0] == 'repl_log.pl'
      # Don't trigger on dynamically generated files.
    else
      system "make"
    end
  end
end
