require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A block macro that passes the contents directly to the output.
#
# Usage
# 
#   pass::[<h1>Hello, World!</h1>]
#
class PassBlockMacro < Extensions::BlockMacroProcessor
  use_dsl

  named :pass
  parse_content_as :text

  def process parent, target, attrs
    create_paragraph parent, (attrs.delete 'text'), attrs, subs: nil
  end
end
