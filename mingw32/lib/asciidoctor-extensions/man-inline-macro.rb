RUBY_ENGINE == 'opal' ? (require 'man-inline-macro/extension') : (require_relative 'man-inline-macro/extension')

Extensions.register :uri_schemes do
  inline_macro ManInlineMacro
  # The following alias allows this macro to be used with the git man pages
  inline_macro ManInlineMacro, :linkgit
end
