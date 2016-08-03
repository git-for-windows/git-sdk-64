RUBY_ENGINE == 'opal' ? (require 'mathoid-treeprocessor/extension') : (require_relative 'mathoid-treeprocessor/extension')

Extensions.register :stem do
  treeprocessor MathoidTreeprocessor
end

# Run the sample using the following command (requires Asciidoctor 1.5.0.preview4):
#
#  asciidoctor -r ./mathoid-treeprocessor.rb mathoid-treeprocessor/sample.adoc
#
# Visit the file mathoid-treeprocessor/sample.html in your browser to see the result.
