# -*- encoding: utf-8 -*-
# stub: irb 1.0.0 ruby lib

Gem::Specification.new do |s|
  s.name = "irb".freeze
  s.version = "1.0.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Keiju ISHITSUKA".freeze]
  s.bindir = "exe".freeze
  s.date = "2018-12-26"
  s.description = "Interactive Ruby command-line tool for REPL (Read Eval Print Loop).".freeze
  s.email = ["keiju@ruby-lang.org".freeze]
  s.executables = ["irb".freeze]
  s.files = [".gitignore".freeze, ".travis.yml".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "bin/console".freeze, "bin/setup".freeze, "exe/irb".freeze, "irb.gemspec".freeze, "lib/irb.rb".freeze, "lib/irb/cmd/chws.rb".freeze, "lib/irb/cmd/fork.rb".freeze, "lib/irb/cmd/help.rb".freeze, "lib/irb/cmd/load.rb".freeze, "lib/irb/cmd/nop.rb".freeze, "lib/irb/cmd/pushws.rb".freeze, "lib/irb/cmd/subirb.rb".freeze, "lib/irb/completion.rb".freeze, "lib/irb/context.rb".freeze, "lib/irb/ext/change-ws.rb".freeze, "lib/irb/ext/history.rb".freeze, "lib/irb/ext/loader.rb".freeze, "lib/irb/ext/multi-irb.rb".freeze, "lib/irb/ext/save-history.rb".freeze, "lib/irb/ext/tracer.rb".freeze, "lib/irb/ext/use-loader.rb".freeze, "lib/irb/ext/workspaces.rb".freeze, "lib/irb/extend-command.rb".freeze, "lib/irb/frame.rb".freeze, "lib/irb/help.rb".freeze, "lib/irb/init.rb".freeze, "lib/irb/input-method.rb".freeze, "lib/irb/inspector.rb".freeze, "lib/irb/lc/.document".freeze, "lib/irb/lc/error.rb".freeze, "lib/irb/lc/help-message".freeze, "lib/irb/lc/ja/encoding_aliases.rb".freeze, "lib/irb/lc/ja/error.rb".freeze, "lib/irb/lc/ja/help-message".freeze, "lib/irb/locale.rb".freeze, "lib/irb/magic-file.rb".freeze, "lib/irb/notifier.rb".freeze, "lib/irb/output-method.rb".freeze, "lib/irb/ruby-lex.rb".freeze, "lib/irb/ruby-token.rb".freeze, "lib/irb/slex.rb".freeze, "lib/irb/src_encoding.rb".freeze, "lib/irb/version.rb".freeze, "lib/irb/workspace.rb".freeze, "lib/irb/ws-for-case-2.rb".freeze, "lib/irb/xmp.rb".freeze]
  s.homepage = "https://github.com/ruby/irb".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.rubygems_version = "3.0.1".freeze
  s.summary = "Interactive Ruby command-line tool for REPL (Read Eval Print Loop).".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
      s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    else
      s.add_dependency(%q<bundler>.freeze, [">= 0"])
      s.add_dependency(%q<rake>.freeze, [">= 0"])
    end
  else
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
  end
end
