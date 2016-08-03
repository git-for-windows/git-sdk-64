include ::Asciidoctor

# An include processor that substitutes tabs with spaces in included source code.
#
# FIXME does not accomodate partial includes!!!
Extensions.register do
  include_processor do
    process do |doc, reader, target, attributes|
      source = File.read File.join(doc.base_dir, target)
      # TODO substitute tabs more carefully
      reader.push_include source.gsub("\t", '    '), target, target, 1, attributes
      reader
    end

    def handles? target
      target.start_with? 'code/'
    end
  end
end
