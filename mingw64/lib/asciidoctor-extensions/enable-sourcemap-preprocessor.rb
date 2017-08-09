require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

class Asciidoctor::Document
  attr_writer :sourcemap unless method_defined? :sourcemap=
end

# A preprocessor that enables the sourcemap feature if not already enabled via
# the API. Useful to use in combination with other extensions that rely on the
# source location information.
Asciidoctor::Extensions.register do
  preprocessor do
    process do |doc, reader|
      doc.sourcemap = true
      nil
    end
  end
end
