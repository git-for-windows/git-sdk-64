require 'asciidoctor/extensions'
require 'open3'

include ::Asciidoctor

# A treeprocessor (and companion docinfo processor) that highlights source
# blocks using the highlight command.
#
# Usage
#
#   :source-highlighter: highlight
#
#   [source,ruby]
#   ----
#   puts 'Hello, World!'
#   ----
#
Extensions.register do
  treeprocessor do
    process do |document|
      document.find_by context: :listing, style: 'source' do |src|
        # TODO handle callout numbers
        src.subs.clear
        lang = src.attr 'language', 'text', false
        highlight = document.attr 'highlight', 'highlight'
        if (document.attr 'highlight-css', 'class') == 'class'
          cmd = %(#{highlight} -f -O html --src-lang #{lang})
        else
          style = document.attr 'highlight-style', 'edit-eclipse'
          cmd = %(#{highlight} -f -O html --inline-css -s #{style} --src-lang #{lang})
        end
        cmd = %(#{cmd} -l -j 2) if src.attr? 'linenums', nil, false
        Open3.popen3 cmd do |stdin, stdout, stderr, wait_thr|
          stdin.write src.source
          stdin.close
          result = []
          while (line = stdout.gets)
            result << line.chomp
          end
          src.lines.replace result
          wait_thr.value
        end
      end if document.attr? 'source-highlighter', 'highlight'
      nil
    end 
  end

  docinfo_processor do
    at_location :footer
    process do |document|
      if (document.attr? 'source-highlighter', 'highlight') &&
          (document.attr 'highlight-css', 'class') == 'class'
        style = document.attr 'highlight-style', 'edit-eclipse'
        highlight = document.attr 'highlight', 'highlight'
        css = %x(#{highlight} -c stdout --print-style -s #{style}).rstrip
        if /^pre\.hl\s+{(?:\s*color:(.+?);)?(?:\s+background-color:(.+?);).*}$/ =~ css
          fg, bg = $1, $2
          css.sub! /^pre\.hl\s+{.*?}/, %(.listingblock pre.highlight { background-color:#{bg}; }\npre.highlight>code { color: #{fg}; })
        end
        ['<style>', css, '</style>'] * "\n"
      end
    end
  end
end
