require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
require_relative 'mathoid'

include ::Asciidoctor

module Asciidoctor
class MathoidTreeprocessor < Extensions::Treeprocessor
  def process document
    unless (stem_blocks = document.find_by context: :stem).nil_or_empty?
      mathoid = ::Mathoid.new
      mathoid.start
      stem_blocks.each do |math|
        equation_data = math.content
        equation_type = math.style.to_sym
        # FIXME auto-generate id if one is not provided
        if math.id
          math_id = alt_text = math.id
        else
          require 'digest'
          math_id = %(stem-#{::Digest::MD5.hexdigest math.content})
          alt_text = 'STEM equation'
        end
        image_target = %(#{math_id}.svg)
        image_file = math.normalize_system_path image_target, (math.document.attr 'imagesdir')
        mathoid.convert_to_file image_file, equation_data, equation_type
        attrs = { 'target' => image_target, 'alt' => alt_text }
        parent = math.parent
        image = Block.new parent, :image, attributes: attrs
        parent.blocks[parent.blocks.index(math)] = image
      end
      mathoid.stop
    end
    document
  end
end
end
