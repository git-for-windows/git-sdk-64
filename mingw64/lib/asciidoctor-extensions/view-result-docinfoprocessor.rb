RUBY_ENGINE == 'opal' ? (require 'view-result-docinfoprocessor/extension') : (require_relative 'view-result-docinfoprocessor/extension')

Asciidoctor::Extensions.register do
  docinfo_processor ViewResultDocinfoProcessor if @document.basebackend? 'html'
end
