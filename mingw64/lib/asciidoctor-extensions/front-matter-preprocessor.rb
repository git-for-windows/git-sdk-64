RUBY_ENGINE == 'opal' ? (require 'front-matter-preprocessor/extension') : (require_relative 'front-matter-preprocessor/extension')

Asciidoctor::Extensions.register do
  preprocessor FrontMatterPreprocessor
end
