require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

class TeXPreprocessor < Extensions::Preprocessor

  # Map $...$ to stem:[...]
  TEX_DOLLAR_RX = /(^|\s|\()\$(.*?)\$($|\s|\)|,|\.)/
  TEX_DOLLAR_SUB = '\1latexmath:[\2]\3'

  def process document, reader
    return reader if reader.eof?
    replacement_lines = reader.read_lines.map do |line|
      (line.include? '$') ? (line.gsub TEX_DOLLAR_RX, TEX_DOLLAR_SUB) : line
    end
    reader.unshift_lines replacement_lines
    reader
  end

end
