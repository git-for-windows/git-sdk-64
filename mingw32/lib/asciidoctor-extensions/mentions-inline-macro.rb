require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

Extensions.register do
  # matches @username
  inline_macro do
    named :@mention
    using_format :short # not required here for Asciidoctor >= 1.5.6
    match %r/@(\w+)/
    process do |parent, target|
      mentions_uri_pattern = (parent.document.attr 'mentions-uri-pattern') || 'https://github.com/%s'
      mention_uri = mentions_uri_pattern % target
      (create_anchor parent, %(@#{target}), type: :link, target: mention_uri).convert
    end
  end

  # matches mention:username[<text>]
  inline_macro do
    named :mention
    parse_content_as :text
    process do |parent, target, attrs|
      mentions_uri_pattern = (parent.document.attr 'mentions-uri-pattern') || 'https://github.com/%s'
      mention_uri = mentions_uri_pattern % target
      if (text = attrs['text']).empty?
        text = %(@#{target})
      end
      (create_anchor parent, text, type: :link, target: mention_uri).convert
    end
  end
end
