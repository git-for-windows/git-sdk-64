require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
require 'open-uri'

include ::Asciidoctor

# TIP This feature is supported out of the box in Asciidoctor if you set the allow-uri-read attribute
class UriIncludeProcessor < Extensions::IncludeProcessor
  def handles? target
    (target.start_with? 'http://') or (target.start_with? 'https://')
  end

  def process doc, reader, target, attributes
    content = (open target).readlines
    reader.push_include content, target, target, 1, attributes
    reader
  end
end
