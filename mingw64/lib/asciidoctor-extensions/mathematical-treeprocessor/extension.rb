require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
require 'mathematical'
autoload :Digest, 'digest'

include ::Asciidoctor

class MathematicalTreeprocessor < Extensions::Treeprocessor
  def process document
    unless (stem_blocks = document.find_by context: :stem).nil_or_empty?
      # The no-args constructor defaults to SVG and standard delimiters ($..$ for inline, $$..$$ for block)
      mathematical = ::Mathematical.new
      image_output_dir = resolve_image_output_dir document
      image_target_dir = document.attr 'imagesoutdir', (document.attr 'imagesdir')
      image_target_dir = '.' if image_target_dir.nil_or_empty?
      ::FileUtils.mkdir_p image_output_dir unless ::File.directory? image_output_dir

      stem_blocks.each do |stem|
        equation_data = %($$#{stem.content}$$)
        equation_type = stem.style.to_sym
        next unless equation_type == :latexmath

        # FIXME auto-generate id if one is not provided
        unless (stem_id = stem.id)
          stem_id = %(stem-#{::Digest::MD5.hexdigest stem.content})
        end

        # QUESTION should we use stem.content as fallback alt text?
        alt_text = stem.attr 'alt', 'Equation'

        image_target = %(#{stem_id}.svg)
        image_file = ::File.join image_output_dir, image_target
        image_target = ::File.join image_target_dir, image_target unless image_target_dir == '.'

        # TODO check for error
        result = mathematical.parse equation_data
        ::IO.write image_file, result[:data]

        attrs = { 'target' => image_target, 'alt' => alt_text, 'align' => 'center' }
        parent = stem.parent
        stem_image = create_image_block parent, attrs
        stem_image.id = stem.id if stem.id
        if (title = stem.attributes['title'])
          stem_image.title = title
        end
        parent.blocks[parent.blocks.index stem] = stem_image
      end
    end
    nil
  end

  def resolve_image_output_dir doc
    if (images_dir = doc.attr 'imagesoutdir')
      base_dir = nil
    else
      base_dir = (doc.attr 'outdir') || ((doc.respond_to? :options) && doc.options[:to_dir])
      images_dir = doc.attr 'imagesdir'
    end

    doc.normalize_system_path images_dir, base_dir
  end
end
