require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# An inline macro that turns URIs with the chrome:// URI scheme into links.
#
# Usage
#
#   chrome://extensions[Extensions] 
#
class ChromeUriMacro < Extensions::InlineMacroProcessor
  use_dsl

  named :chrome
  parse_content_as :text

  def process parent, target, attrs
    target = %(chrome:#{target})
    if (text = attrs['text']).empty?
      text = target
    end
    (create_anchor parent, text, type: :link, target: target).render
  end
end
