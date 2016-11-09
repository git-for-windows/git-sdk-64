require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
require 'tilt'
require 'slim'

include ::Asciidoctor

# An extension that processes the contents of a block
# as a Slim template.
#
# Usage
#
#   [slim]
#   --
#   p Hello, World!
#   ul
#     li red
#     li green
#     li blue
#   --
#
class SlimBlock < Extensions::BlockProcessor
  use_dsl

  named :slim
  on_context :open
  parse_content_as :raw

  def process parent, reader, attrs
    document = parent.document
    lines = reader.lines

    pretty = attrs.fetch('pretty', 'true') == 'true'

    html_syntax = (document.attr 'htmlsyntax', 'html') == 'html' ? :html5 : :xhtml
    tmpl = Slim::Template.new(format: html_syntax, pretty: pretty) { lines * EOL }
    html = tmpl.render document, (attributes_to_locals document.attributes)

    # QUESTION should we allow attribute references in slim source or allow subs to be specified?
    #create_pass_block parent, html, attrs
    create_pass_block parent, html, attrs, subs: nil
  end

  def attributes_to_locals attrs
    attrs.inject({}) do |accum, (key,val)|
      accum[key.tr '-', '_'] = val
      accum
    end
  end
end
