require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A docinfo processor that emits an appropriate license URL into a document.
# Requires that the license attribute contain one or more http or https URL.
#
# Usage
#
#   :license: http://opensource.org/licenses/MIT
#
Extensions.register do
  docinfo_processor do
    #at_location :head
    process do |doc|
      next unless doc.attr? 'license'
      backend = doc.backend

      result = []

      # Support dual licensing.
      (doc.attr 'license').split(/\s+/).each do |url|
        next unless url.start_with? 'http://', 'https://'
        if backend == 'docbook5'
          result << %(<dct:license xmlns:dct="http://purl.org/dc/terms/">#{url}</dct:license>)
        elsif backend == 'html5'
          result << %(<link rel="license" href="#{url}">)
        elsif backend == 'xhtml5'
          result << %(<link rel="license" href="#{url}" />)
        end
      end

      result * EOL
    end
  end
end
