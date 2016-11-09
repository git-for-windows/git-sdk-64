require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

class CopyrightFooterPostprocessor < Asciidoctor::Extensions::Postprocessor
  def process document, output
    content = (document.attr 'copyright') || 'Copyright Acme, Inc.'
    if document.basebackend? 'html'
      replacement = %(<div id="footer-text">\\1<br>\n#{content}\n</div>)
      output = output.sub(/<div id="footer-text">(.*?)<\/div>/m, replacement)
    elsif document.basebackend? 'docbook'
      replacement = %(<simpara>#{content}</simpara>\n\\1)
      output = output.sub(/(<\/(?:article|book)>)/, replacement)
    end
    output
  end 
end
