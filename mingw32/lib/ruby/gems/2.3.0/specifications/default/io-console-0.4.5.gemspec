# -*- encoding: utf-8 -*-
# stub: io-console 0.4.5 ruby lib
# stub: extconf.rb

Gem::Specification.new do |s|
  s.name = "io-console"
  s.version = "0.4.5"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["Nobu Nakada"]
  s.cert_chain = ["certs/nobu.pem"]
  s.date = "2015-12-24"
  s.description = "add console capabilities to IO instances."
  s.email = "nobu@ruby-lang.org"
  s.extensions = ["extconf.rb"]
  s.files = ["extconf.rb", "io/console.so", "io/console/size.rb"]
  s.homepage = "http://www.ruby-lang.org"
  s.licenses = ["BSD-2-Clause"]
  s.required_ruby_version = Gem::Requirement.new(">= 2.0.0")
  s.rubygems_version = "2.5.1"
  s.summary = "Console interface"
end
