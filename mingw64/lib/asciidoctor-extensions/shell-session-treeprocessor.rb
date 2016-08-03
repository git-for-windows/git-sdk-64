RUBY_ENGINE == 'opal' ? (require 'shell-session-treeprocessor/extension') : (require_relative 'shell-session-treeprocessor/extension')

Extensions.register do
  treeprocessor ShellSessionTreeprocessor
end
