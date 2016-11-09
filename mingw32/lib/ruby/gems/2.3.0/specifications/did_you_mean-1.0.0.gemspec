# -*- encoding: utf-8 -*-
# stub: did_you_mean 1.0.0 ruby lib

Gem::Specification.new do |s|
  s.name = "did_you_mean"
  s.version = "1.0.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["Yuki Nishijima"]
  s.date = "2015-12-22"
  s.description = "\"did you mean?\" experience in Ruby: the error message will tell you the right one when you misspelled something."
  s.email = ["mail@yukinishijima.net"]
  s.homepage = "https://github.com/yuki24/did_you_mean"
  s.licenses = ["MIT"]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0dev")
  s.rubygems_version = "2.5.1"
  s.summary = "\"Did you mean?\" experience in Ruby"

  s.installed_by_version = "2.5.1" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>, ["~> 1.5"])
      s.add_development_dependency(%q<rake>, [">= 0"])
      s.add_development_dependency(%q<minitest>, [">= 0"])
    else
      s.add_dependency(%q<bundler>, ["~> 1.5"])
      s.add_dependency(%q<rake>, [">= 0"])
      s.add_dependency(%q<minitest>, [">= 0"])
    end
  else
    s.add_dependency(%q<bundler>, ["~> 1.5"])
    s.add_dependency(%q<rake>, [">= 0"])
    s.add_dependency(%q<minitest>, [">= 0"])
  end
end
