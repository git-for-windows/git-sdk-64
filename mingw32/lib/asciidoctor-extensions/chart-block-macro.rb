RUBY_ENGINE == 'opal' ? (require 'chart-block-macro/extension') : (require_relative 'chart-block-macro/extension')

Extensions.register do
  if document.basebackend? 'html'
    block_macro ChartBlockMacro
    block ChartBlockProcessor
  end
end
