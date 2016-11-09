RUBY_ENGINE == 'opal' ? (require 'xml-entity-postprocessor/extension') : (require_relative 'xml-entity-postprocessor/extension')

Extensions.register :xml_compliance do
  if (@document.basebackend? 'xml') || (@document.attr? 'htmlsyntax', 'xml')
    postprocessor XmlEntityPostprocessor
  end
end
