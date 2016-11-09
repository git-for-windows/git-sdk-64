RUBY_ENGINE == 'opal' ? (require 'pass-block-macro/extension') : (require_relative 'pass-block-macro/extension')

Extensions.register do
  block_macro PassBlockMacro
end
