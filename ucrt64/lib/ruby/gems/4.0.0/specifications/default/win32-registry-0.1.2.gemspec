# -*- encoding: utf-8 -*-
# stub: win32-registry 0.1.2 ruby lib

Gem::Specification.new do |s|
  s.name = "win32-registry".freeze
  s.version = "0.1.2".freeze

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/win32-registry", "source_code_uri" => "https://github.com/ruby/win32-registry" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["U.Nakamura".freeze]
  s.bindir = "exe".freeze
  s.date = "2026-07-14"
  s.description = "Provides an interface to the Windows Registry in Ruby".freeze
  s.email = ["usa@garbagecollect.jp".freeze]
  s.files = ["lib/win32/registry.rb".freeze, "lib/win32/resolv.rb".freeze]
  s.homepage = "https://github.com/ruby/win32-registry".freeze
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "4.0.16".freeze
  s.summary = "Provides an interface to the Windows Registry in Ruby".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<fiddle>.freeze, ["~> 1.0".freeze])
end
