include ::Asciidoctor

# A preprocessor that adds hardbreaks to the end of all lines.
#
# NOTE Asciidoctor already supports this feature via the hardbreaks attribute
Extensions.register {
  preprocessor {
    process {|document, reader|
      Reader.new reader.readlines.map {|l|
        (l.empty? || (l.start_with? '=')) ? l : %(#{l} +)
      }
    }
  }
}
