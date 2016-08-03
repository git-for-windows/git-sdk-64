include ::Asciidoctor

Extensions.register do
  inline_macro do
    named :mention
    using_format :short
    match /@(\w+)/
    process do |parent, target|
      mentions_uri_pattern = (parent.document.attr 'mentions-uri-pattern') || 'https://github.com/%s'
      mention_uri = mentions_uri_pattern % target
      (create_anchor parent, %(@#{target}), type: :link, target: mention_uri).render
    end
  end
end
