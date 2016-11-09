require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A treeprocessor extension that identifies literal blocks that appear to be
# commands in a shell session and converts them to listing blocks with a
# terminal style.
#
# Usage
#
#   $ gem install asciidoctor
#
class ShellSessionTreeprocessor < Extensions::Treeprocessor
  def process document
    return unless document.blocks?
    process_blocks document
    nil
  end

  def process_blocks node
    node.blocks.each_with_index do |block, i|
      if block.context == :literal &&
          (((first_line = block.lines.first).start_with? '$ ') ||
            (first_line.start_with? '> '))
        node.blocks[i] = convert_to_terminal_listing block
      else
        process_blocks block if block.blocks?
      end
    end
  end

  def convert_to_terminal_listing block
    attrs = block.attributes
    attrs['role'] = 'terminal'
    prompt_attr = (attrs.has_key? 'prompt') ?
        %( data-prompt="#{block.sub_specialchars attrs['prompt']}") : nil
    lines = block.lines.map do |line|
      line = block.sub_specialchars line.chomp
      if line.start_with? '$ '
        %(<span class="command"#{prompt_attr}>#{line[2..-1]}</span>)
      elsif line.start_with? '&gt; '
        %(<span class="output">#{line[5..-1]}</span>)
        #%(<span class="output"><span class="comment-prefix"># </span>#{line[5..-1]}</span>)
      else
        line
      end
    end
    create_listing_block block.document, lines * EOL, attrs, subs: nil
    #attributes['language'] = 'bash'
    #Block.new(@document, :listing, :content_model => :verbatim, :style => 'source', :source => block.source[2..-1], :attributes => attributes)
  end
end
