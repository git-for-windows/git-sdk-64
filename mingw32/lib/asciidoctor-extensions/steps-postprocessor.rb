RUBY_ENGINE == 'opal' ? (require 'steps-postprocessor/extension') : (require_relative 'steps-postprocessor/extension')

Asciidoctor::Extensions.register do
  if (@document.basebackend? 'html')
    postprocessor StepsPostprocessor
  end
end
