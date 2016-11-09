RUBY_ENGINE == 'opal' ? (require 'uri-include-processor/extension') : (require_relative 'uri-include-processor/extension')

Extensions.register do
  include_processor UriIncludeProcessor 
end
