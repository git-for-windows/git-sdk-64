require 'asciidoctor/extensions'

Asciidoctor::Extensions.register do
  block do
    named :sample
    on_context :open
    process do |parent, reader, attrs|
      wrapper = create_open_block parent, [], {}
      parse_content wrapper, reader
      warn wrapper.blocks
      wrapper
    end
  end
end
