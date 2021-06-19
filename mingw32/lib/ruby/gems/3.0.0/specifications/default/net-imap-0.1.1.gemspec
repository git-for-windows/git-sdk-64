# -*- encoding: utf-8 -*-
# stub: net-imap 0.1.1 ruby lib

Gem::Specification.new do |s|
  s.name = "net-imap".freeze
  s.version = "0.1.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/net-imap", "source_code_uri" => "https://github.com/ruby/net-imap" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Shugo Maeda".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-06-16"
  s.description = "Ruby client api for Internet Message Access Protocol".freeze
  s.email = ["shugo@ruby-lang.org".freeze]
  s.files = ["net/ftp.rb".freeze, "net/http.rb".freeze, "net/http/backward.rb".freeze, "net/http/exceptions.rb".freeze, "net/http/generic_request.rb".freeze, "net/http/header.rb".freeze, "net/http/proxy_delta.rb".freeze, "net/http/request.rb".freeze, "net/http/requests.rb".freeze, "net/http/response.rb".freeze, "net/http/responses.rb".freeze, "net/http/status.rb".freeze, "net/https.rb".freeze, "net/imap.rb".freeze, "net/pop.rb".freeze, "net/protocol.rb".freeze, "net/smtp.rb".freeze]
  s.homepage = "https://github.com/ruby/net-imap".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "3.2.15".freeze
  s.summary = "Ruby client api for Internet Message Access Protocol".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<digest>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<strscan>.freeze, [">= 0"])
  else
    s.add_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_dependency(%q<digest>.freeze, [">= 0"])
    s.add_dependency(%q<strscan>.freeze, [">= 0"])
  end
end
