include ::Asciidoctor

# A preprocessor that adds hardbreaks to the end of all lines.
Extensions.register {
  if @document.attr? 'showcomments'
    preprocessor {
      process {|document, reader|
        Reader.new reader.readlines.map {|l|
          if (l.start_with? '// TODO ') || (l.start_with? '// FIXME ')
            %([comment]###{l[3..-1]}##)
          else
            l
          end
        }
      }
    }
  end
}
