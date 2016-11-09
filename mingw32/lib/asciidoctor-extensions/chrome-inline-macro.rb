RUBY_ENGINE == 'opal' ? (require 'chrome-inline-macro/extension') : (require_relative 'chrome-inline-macro/extension')

Extensions.register :uri_schemes do
  inline_macro ChromeUriMacro
end
