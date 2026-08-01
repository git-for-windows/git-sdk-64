# -*- encoding: utf-8 -*-
# stub: digest 3.2.1 ruby lib

Gem::Specification.new do |s|
  s.name = "digest".freeze
  s.version = "3.2.1".freeze

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "msys2_mingw_dependencies" => "openssl" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Akinori MUSHA".freeze]
  s.bindir = "exe".freeze
  s.date = "2026-07-14"
  s.description = "Provides a framework for message digest libraries.".freeze
  s.email = ["knu@idaemons.org".freeze]
  s.files = ["LICENSE.txt".freeze, "README.md".freeze, "digest.so".freeze, "ext/digest/bubblebabble/extconf.h".freeze, "ext/digest/extconf.h".freeze, "ext/digest/md5/extconf.h".freeze, "ext/digest/rmd160/extconf.h".freeze, "ext/digest/sha1/extconf.h".freeze, "ext/digest/sha2/extconf.h".freeze, "lib/digest.rb".freeze, "lib/digest/loader.rb".freeze, "lib/digest/version.rb".freeze]
  s.homepage = "https://github.com/ruby/digest".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "4.0.16".freeze
  s.summary = "Provides a framework for message digest libraries.".freeze
end
