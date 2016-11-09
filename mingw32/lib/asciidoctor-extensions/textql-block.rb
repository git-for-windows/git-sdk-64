require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A block that queries the CSV content in the block using textql
# (https://github.com/dinedal/textql) and generates an AsciiDoc table with the
# results.
#
# The textql command must be available on your PATH.
#
# Usage
#
#   [textql%header, "select name, age from tbl where age >= 40"]
#   ....
#   name,age
#   John,15
#   Paul,50
#   Susan,38
#   Meghan,49
#   ....
#
Extensions.register(:textql) {
  block {
    named :textql
    on_context :literal
    name_positional_attributes 'query', 'header'
    parse_content_as :raw
    process do |parent, reader, attrs|
      # TODO assume header if second line is blank
      input_header = (attrs.has_key? 'header-option') ? reader.lines.first : nil
      output_header = (attrs.has_key? 'header') ? (attrs.delete 'header') : input_header
      output_header = nil if output_header == 'None'
      cmd = %(echo "#{reader.source}" | /home/dallen/opt/gocode/bin/textql#{input_header ? ' -header' : nil} -sql "#{attrs.delete 'query'}")
      result_lines = %x(#{cmd}).chomp.split "\n"
      table_delimiter = ',==='
      result_lines.unshift *[output_header, ''] if output_header
      result_lines.unshift table_delimiter
      result_lines << table_delimiter
      
      through_attrs = ['id', 'role', 'title'].inject({}) {|collector, key|
        collector[key] = attrs[key] if attrs.has_key? key 
        collector
      }
      parse_content parent, result_lines, through_attrs
      #create_open_block parent, result_lines, through_attrs
    end
  }
}
