RUBY_ENGINE == 'opal' ? (require 'man-inline-macro/extension') : (require_relative 'man-inline-macro/extension')

Extensions.register :uri_schemes do
  #inline_macro ManInlineMacro
  # Use the following instead for the git man pages
  inline_macro ManInlineMacro, :linkgit
end
