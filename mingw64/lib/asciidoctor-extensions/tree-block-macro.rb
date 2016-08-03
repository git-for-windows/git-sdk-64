RUBY_ENGINE == 'opal' ? (require 'tree-block-macro/extension') : (require_relative 'tree-block-macro/extension')

Asciidoctor::Extensions.register do
  block_macro TreeBlockMacro
end
