# An inline macro that turns Java types the begin with java. or javax. into links.
#
# Usage
# 
#   Implement the javax.validation.Validator interface.
#
# @author Aslak Knutsen
Asciidoctor::Extensions.register do
  inline_macro do
    named :apidoc
    match /((?:java|javax)\.\w[\.\w]+\.[A-Z]\w+)/
    process do |parent, target|
      doc_uri_pattern = 'https://javaee-spec.java.net/nonav/javadocs/%s.html'
      doc_uri = doc_uri_pattern % target.gsub(/\./, '/')
      link_name = target
      link_name = $1 if target =~ /([A-Z]\w*$)/
      (create_anchor parent, link_name, type: :link, target: doc_uri, attributes: {'title' => target}).render
    end
  end
end
