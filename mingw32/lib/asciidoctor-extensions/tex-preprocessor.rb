RUBY_ENGINE == 'opal' ? (require 'tex-preprocessor/extension') : (require_relative 'tex-preprocessor/extension')

Extensions.register do
  opts = @document.instance_variable_get :@options
  unless opts[:parse_header_only]
    doc_file = @document.attributes['docfile']
    meta_doc = Asciidoctor.load_file doc_file, opts.merge(parse_header_only: true)
    preprocessor TeXPreprocessor if (meta_doc.attr 'extensions', '').split(' ').include?('latex')
  end
end
