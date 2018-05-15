# -*- encoding: utf-8 -*-
# stub: fiddle 1.0.0 ruby lib

Gem::Specification.new do |s|
  s.name = "fiddle".freeze
  s.version = "1.0.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Aaron Patterson".freeze, "SHIBATA Hiroshi".freeze]
  s.bindir = "exe".freeze
  s.date = "2018-05-14"
  s.description = "A libffi wrapper for Ruby.".freeze
  s.email = ["aaron@tenderlovemaking.com".freeze, "hsbt@ruby-lang.org".freeze]
  s.files = [".gitignore".freeze, ".travis.yml".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "bin/console".freeze, "bin/setup".freeze, "ext/fiddle/closure.c".freeze, "ext/fiddle/closure.h".freeze, "ext/fiddle/conversions.c".freeze, "ext/fiddle/conversions.h".freeze, "ext/fiddle/extconf.rb".freeze, "ext/fiddle/extlibs".freeze, "ext/fiddle/fiddle.c".freeze, "ext/fiddle/fiddle.h".freeze, "ext/fiddle/function.c".freeze, "ext/fiddle/function.h".freeze, "ext/fiddle/handle.c".freeze, "ext/fiddle/pointer.c".freeze, "ext/fiddle/win32/fficonfig.h".freeze, "ext/fiddle/win32/libffi-3.2.1-mswin.patch".freeze, "ext/fiddle/win32/libffi-config.rb".freeze, "ext/fiddle/win32/libffi.mk.tmpl".freeze, "fiddle.gemspec".freeze, "lib/fiddle.rb".freeze, "lib/fiddle/closure.rb".freeze, "lib/fiddle/cparser.rb".freeze, "lib/fiddle/function.rb".freeze, "lib/fiddle/import.rb".freeze, "lib/fiddle/pack.rb".freeze, "lib/fiddle/struct.rb".freeze, "lib/fiddle/types.rb".freeze, "lib/fiddle/value.rb".freeze]
  s.homepage = "https://github.com/ruby/fiddle".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0".freeze)
  s.rubygems_version = "2.7.6".freeze
  s.summary = "A libffi wrapper for Ruby.".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
      s.add_development_dependency(%q<rake>.freeze, [">= 0"])
      s.add_development_dependency(%q<rake-compiler>.freeze, [">= 0"])
    else
      s.add_dependency(%q<bundler>.freeze, [">= 0"])
      s.add_dependency(%q<rake>.freeze, [">= 0"])
      s.add_dependency(%q<rake-compiler>.freeze, [">= 0"])
    end
  else
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<rake-compiler>.freeze, [">= 0"])
  end
end
