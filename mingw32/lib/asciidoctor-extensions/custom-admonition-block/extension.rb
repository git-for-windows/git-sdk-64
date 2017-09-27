require 'asciidoctor/extensions'

include Asciidoctor

# An extension that introduces a custom admonition type, complete
# with a custom icon.
#
# Usage
#
#   [QUESTION]
#   ====
#   What's the main tool for selecting colors?
#   ====
#
# or
#
#   [QUESTION]
#   What's the main tool for selecting colors?
#
class CustomAdmonitionBlock < Extensions::BlockProcessor
  use_dsl
  named :QUESTION
  on_contexts :example, :paragraph

  def process parent, reader, attrs
    attrs['name'] = 'question'
    attrs['caption'] = 'Question'
    create_block parent, :admonition, reader.lines, attrs, content_model: :compound
  end
end

class CustomAdmonitionBlockDocinfo < Extensions::DocinfoProcessor
  use_dsl

  def process doc
    '<style>
.admonitionblock td.icon .icon-question:before {content:"\f128";color:#871452;}
</style>'
  end
end
