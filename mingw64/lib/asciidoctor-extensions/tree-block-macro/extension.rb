require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A block macro that reads the contents of the specified directory
# and renders a decorated tree structure in a listing block.
#
# Usage
#
#   tree::directory[]
#
class TreeBlockMacro < Asciidoctor::Extensions::BlockMacroProcessor
  include_dsl
  named :tree
  def process parent, target, attrs
    target = parent.sub_attributes target
    maxdepth = (attrs.has_key? 'maxdepth') ? attrs['maxdepth'].to_i : nil
    if maxdepth && maxdepth < 1
      warn 'asciidoctor: maxdepth for tree must be greater than 0'
      maxdepth = 1
    end
    levelarg = maxdepth ? %(-L #{maxdepth}) : nil
    sizearg = (attrs.has_key? 'size-option') ? '-sh --du' : nil
    dirarg = (attrs.has_key? 'dir-option') ? '-d' : nil
    # TODO use https://github.com/tokiro/tree.rb instead
    # TODO could also output using plantuml
    cmd = %(tree --charset unicode --noreport --dirsfirst -F #{levelarg} #{sizearg} #{dirarg} #{target}).split(/ +/) * ' '
    tree = %x(#{cmd}).lines.entries[1..-1].join
    subs = if sizearg
      tree = tree.gsub(/\[(.*?)\]  (?=\S)/, '[.gray]#[\1]# ')
      [:specialcharacters, :quotes, :macros]
    else
      [:specialcharacters, :macros]
    end
    tree = tree.gsub(/[|`]-- (.*)\/$/, 'icon:folder-open[role=lime] \1/')
        .gsub(/[|`]-- (.*)\*$/, 'icon:gears[] \1')
        .gsub(/[|`]-- (.*\.adoc)$/, 'icon:file-text[role=silver] \1')
        .gsub(/[|`]-- /, 'icon:file[role=silver] ')
        .tr('|', ' ')
    #warn tree
    create_listing_block parent, tree, attrs, subs: subs
  end
end
