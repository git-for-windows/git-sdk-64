RUBY_ENGINE == 'opal' ? (require 'view-result-postprocessor/extension') : (require_relative 'view-result-postprocessor/extension')

Asciidoctor::Extensions.register do
  if (@document.basebackend? 'html')
    postprocessor ViewResultPostprocessor
  end
end
