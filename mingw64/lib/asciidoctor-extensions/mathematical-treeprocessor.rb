RUBY_ENGINE == 'opal' ? (require 'mathematical-treeprocessor/extension') : (require_relative 'mathematical-treeprocessor/extension')

Extensions.register do
  treeprocessor MathematicalTreeprocessor
end

# First, install the mathematical gem:
#
# $ gem install mathematical
#
# Next, run the sample using the following command:
#
#  asciidoctor -r ./mathematical-treeprocessor.rb mathematical-treeprocessor/sample.adoc
#
# Visit the file mathematical-treeprocessor/sample.html in your browser to see the result.
