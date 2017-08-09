RUBY_ENGINE == 'opal' ? (require 'protect-emails-postprocessor/extension') : (require_relative 'protect-emails-postprocessor/extension')

Asciidoctor::Extensions.register do
  if document.basebackend? 'html' # currently only html support
    postprocessor ProtectEmailsPostprocessor
    docinfo_processor ProtectEmailsDocinfoProcessor
  end
end
