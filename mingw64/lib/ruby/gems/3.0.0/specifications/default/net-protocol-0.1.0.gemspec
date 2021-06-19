# -*- encoding: utf-8 -*-
# stub: net-protocol 0.1.0 ruby lib

Gem::Specification.new do |s|
  s.name = "net-protocol".freeze
  s.version = "0.1.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/net-protocol", "source_code_uri" => "https://github.com/ruby/net-protocol" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Yukihiro Matsumoto".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-06-16"
  s.description = "The abstruct interface for net-* client.".freeze
  s.email = ["matz@ruby-lang.org".freeze]
  s.files = ["net/ftp.rb".freeze, "net/http.rb".freeze, "net/http/backward.rb".freeze, "net/http/exceptions.rb".freeze, "net/http/generic_request.rb".freeze, "net/http/header.rb".freeze, "net/http/proxy_delta.rb".freeze, "net/http/request.rb".freeze, "net/http/requests.rb".freeze, "net/http/response.rb".freeze, "net/http/responses.rb".freeze, "net/http/status.rb".freeze, "net/https.rb".freeze, "net/imap.rb".freeze, "net/pop.rb".freeze, "net/protocol.rb".freeze, "net/smtp.rb".freeze]
  s.homepage = "https://github.com/ruby/net-protocol".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0".freeze)
  s.rubygems_version = "3.2.15".freeze
  s.summary = "The abstruct interface for net-* client.".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<timeout>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<io-wait>.freeze, [">= 0"])
  else
    s.add_dependency(%q<timeout>.freeze, [">= 0"])
    s.add_dependency(%q<io-wait>.freeze, [">= 0"])
  end
end
