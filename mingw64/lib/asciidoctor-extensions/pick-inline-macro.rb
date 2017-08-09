require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
include ::Asciidoctor

# An inline macro that picks text based on the presence of document-level attributes.
#
# Usage
#
#   pick:[target-web=Web,target-desktop=Desktop]
#   pick:[target-web.target-mobile="Web or mobile",target-desktop=Desktop]
#
#   pick2:target-web,target-mobile@target-desktop[Web or mobile,Desktop]
#
Extensions.register do
  inline_macro :pick do
    #named :pick
    # FIXME `using_format :short` not working in 1.5.2 because of an ordering issue
    #using_format :short
    match resolve_regexp(@name, :short)
    # FIXME allow parse_content_as :attributes
    # parse_content_as :attributes
    process do |parent, target|
      doc = parent.document
      attrs = (AttributeList.new target).parse
      valid_key = attrs.keys.find {|key|
        next false unless String === key
        if key.include? '.'
          key.split('.').find {|key_alt| doc.attr? key_alt }
        else
          doc.attr? key
        end
      }
      valid_key ? attrs[valid_key] : ''
    end
  end

  inline_macro :pick2 do
    # FIXME can't set named inside block (regexp doesn't get setup right)
    #named :ifdef
    # FIXME allow parse_content_as :attributes
    # parse_content_as :attributes
    process do |parent, target, attributes|
      doc = parent.document
      valid_key = target.split('@').find_index {|key|
        # TODO implement + to require all keys in list to be set
        if key.include? ','
          key.split(',').find {|key_alt| doc.attr? key_alt }
        else
          doc.attr? key
        end
      }
      valid_key ? attributes[valid_key + 1] : ''
    end
  end
end
