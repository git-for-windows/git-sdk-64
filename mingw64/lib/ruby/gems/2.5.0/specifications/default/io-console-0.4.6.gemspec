# -*- encoding: utf-8 -*-
# stub: io-console 0.4.6 ruby lib
# stub: ext/io/console/extconf.rb

Gem::Specification.new do |s|
  s.name = "io-console".freeze
  s.version = "0.4.6"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Nobu Nakada".freeze]
  s.cert_chain = ["certs/nobu.pem".freeze]
  s.date = "2017-09-16"
  s.description = "add console capabilities to IO instances.".freeze
  s.email = "nobu@ruby-lang.org".freeze
  s.extensions = ["ext/io/console/extconf.rb".freeze]
  s.files = ["ext/io/console/console.c".freeze, "ext/io/console/extconf.rb".freeze, "ext/io/console/win32_vk.inc".freeze, "lib/console/size.rb".freeze]
  s.homepage = "https://github.com/ruby/io-console".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.0.0".freeze)
  s.rubygems_version = "2.7.3".freeze
  s.summary = "Console interface".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<rake-compiler>.freeze, [">= 0"])
      s.add_development_dependency(%q<rake-compiler-dock>.freeze, [">= 0.6.1"])
    else
      s.add_dependency(%q<rake-compiler>.freeze, [">= 0"])
      s.add_dependency(%q<rake-compiler-dock>.freeze, [">= 0.6.1"])
    end
  else
    s.add_dependency(%q<rake-compiler>.freeze, [">= 0"])
    s.add_dependency(%q<rake-compiler-dock>.freeze, [">= 0.6.1"])
  end
end
