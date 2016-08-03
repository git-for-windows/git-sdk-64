# -*- encoding: utf-8 -*-
# stub: asciidoctor 1.5.4 ruby lib

Gem::Specification.new do |s|
  s.name = "asciidoctor"
  s.version = "1.5.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["Dan Allen", "Sarah White", "Ryan Waldron", "Jason Porter", "Nick Hengeveld", "Jeremy McAnally"]
  s.date = "2016-01-05"
  s.description = "A fast, open source text processor and publishing toolchain, written in Ruby, for converting AsciiDoc content to HTML5, DocBook 5 (or 4.5) and other formats.\n"
  s.email = ["dan.j.allen@gmail.com"]
  s.executables = ["asciidoctor", "asciidoctor-safe"]
  s.extra_rdoc_files = ["CHANGELOG.adoc", "CONTRIBUTING.adoc", "LICENSE.adoc"]
  s.files = ["CHANGELOG.adoc", "CONTRIBUTING.adoc", "LICENSE.adoc", "bin/asciidoctor", "bin/asciidoctor-safe"]
  s.homepage = "http://asciidoctor.org"
  s.licenses = ["MIT"]
  s.rdoc_options = ["--charset=UTF-8"]
  s.rubygems_version = "2.5.1"
  s.summary = "An implementation of the AsciiDoc text processor and publishing toolchain in Ruby"

  s.installed_by_version = "2.5.1" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<asciimath>, ["~> 1.0.2"])
      s.add_development_dependency(%q<coderay>, ["~> 1.1.0"])
      s.add_development_dependency(%q<cucumber>, ["~> 1.3.1"])
      s.add_development_dependency(%q<erubis>, ["~> 2.7.0"])
      s.add_development_dependency(%q<haml>, ["~> 4.0.0"])
      s.add_development_dependency(%q<nokogiri>, ["~> 1.5.10"])
      s.add_development_dependency(%q<rake>, ["~> 10.0.0"])
      s.add_development_dependency(%q<rspec-expectations>, ["~> 2.14.0"])
      s.add_development_dependency(%q<slim>, ["~> 2.0.0"])
      s.add_development_dependency(%q<thread_safe>, ["~> 0.3.4"])
      s.add_development_dependency(%q<tilt>, ["~> 2.0.0"])
      s.add_development_dependency(%q<yard>, ["~> 0.8.7"])
      s.add_development_dependency(%q<yard-tomdoc>, ["~> 0.7.0"])
      s.add_development_dependency(%q<minitest>, ["~> 5.3.0"])
    else
      s.add_dependency(%q<asciimath>, ["~> 1.0.2"])
      s.add_dependency(%q<coderay>, ["~> 1.1.0"])
      s.add_dependency(%q<cucumber>, ["~> 1.3.1"])
      s.add_dependency(%q<erubis>, ["~> 2.7.0"])
      s.add_dependency(%q<haml>, ["~> 4.0.0"])
      s.add_dependency(%q<nokogiri>, ["~> 1.5.10"])
      s.add_dependency(%q<rake>, ["~> 10.0.0"])
      s.add_dependency(%q<rspec-expectations>, ["~> 2.14.0"])
      s.add_dependency(%q<slim>, ["~> 2.0.0"])
      s.add_dependency(%q<thread_safe>, ["~> 0.3.4"])
      s.add_dependency(%q<tilt>, ["~> 2.0.0"])
      s.add_dependency(%q<yard>, ["~> 0.8.7"])
      s.add_dependency(%q<yard-tomdoc>, ["~> 0.7.0"])
      s.add_dependency(%q<minitest>, ["~> 5.3.0"])
    end
  else
    s.add_dependency(%q<asciimath>, ["~> 1.0.2"])
    s.add_dependency(%q<coderay>, ["~> 1.1.0"])
    s.add_dependency(%q<cucumber>, ["~> 1.3.1"])
    s.add_dependency(%q<erubis>, ["~> 2.7.0"])
    s.add_dependency(%q<haml>, ["~> 4.0.0"])
    s.add_dependency(%q<nokogiri>, ["~> 1.5.10"])
    s.add_dependency(%q<rake>, ["~> 10.0.0"])
    s.add_dependency(%q<rspec-expectations>, ["~> 2.14.0"])
    s.add_dependency(%q<slim>, ["~> 2.0.0"])
    s.add_dependency(%q<thread_safe>, ["~> 0.3.4"])
    s.add_dependency(%q<tilt>, ["~> 2.0.0"])
    s.add_dependency(%q<yard>, ["~> 0.8.7"])
    s.add_dependency(%q<yard-tomdoc>, ["~> 0.7.0"])
    s.add_dependency(%q<minitest>, ["~> 5.3.0"])
  end
end
