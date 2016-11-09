RUBY_ENGINE == 'opal' ? (require 'gist-block-macro/extension') : (require_relative 'gist-block-macro/extension')

Asciidoctor::Extensions.register do
  if (@document.basebackend? 'html') && (@document.safe < SafeMode::SECURE)
    block_macro GistBlockMacro
  end
end
