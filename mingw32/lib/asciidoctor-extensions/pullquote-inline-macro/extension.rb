require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# An inline macro that extracts pull quotes for prose.
#
# Usage
#
#   q:<[Content of pull quote.]
#
# TODO Use a DocinfoProcessor to add the necessary CSS to the output
# TODO Add a shortform that implies a default target position
class PullquoteInlineMacro < Extensions::InlineMacroProcessor
  use_dsl
  named :q
  def process parent, target, attributes
    align = (target == '<' || target == '&lt;') ? 'left' : 'right'
    if attributes.key? 'precede'
      text = attributes['pull']
      %(<span class="pullquote-#{align}" data-pullquote="#{text}"></span>
#{attributes['precede']}
#{text})
    else
      text = attributes.values * ', ' # iky!
      %(<span class="pullquote-#{align}" data-pullquote="#{text}"></span>)
    end
  end
end
