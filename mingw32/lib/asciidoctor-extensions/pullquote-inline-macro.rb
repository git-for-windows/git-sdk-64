RUBY_ENGINE == 'opal' ? (require 'pullquote-inline-macro/extension') : (require_relative 'pullquote-inline-macro/extension')

Extensions.register do
  inline_macro PullquoteInlineMacro
end
