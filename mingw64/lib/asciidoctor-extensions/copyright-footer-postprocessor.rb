RUBY_ENGINE == 'opal' ? (require 'copyright-footer-postprocessor/extension') : (require_relative 'copyright-footer-postprocessor/extension')

Asciidoctor::Extensions.register do
  postprocessor CopyrightFooterPostprocessor
end
