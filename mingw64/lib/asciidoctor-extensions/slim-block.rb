RUBY_ENGINE == 'opal' ? (require 'slim-block/extension') : (require_relative 'slim-block/extension')

Extensions.register :markup do
  block SlimBlock
end
