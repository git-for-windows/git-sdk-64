require 'middleman-core'
require 'middleman-core/extensions/lorem'
require 'asciidoctor'
require 'asciidoctor/extensions'

Asciidoctor::Extensions.register do
  block_macro :lorem do
    name_positional_attributes 'size'
    default_attrs 'size' => 1
    process do |parent, target, attrs|
      lorem = Middleman::Extensions::Lorem::LoremObject
      method = target.to_sym
      if lorem.respond_to? method
        content = lorem.send(method, attrs['size'].to_i.abs)
        create_paragraph parent, content, {}
      else
        warn 'Unknown lorem target for lorem block macro'
        nil
      end
    end
  end
end
