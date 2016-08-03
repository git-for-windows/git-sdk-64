require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

# An extension that automatically hides blocks marked with the style
# "result" and adds a link to the previous element that has the style
# "title" that allows displaying the "result".
#
# Usage
#
#   = View Result Sample
#
#   .This will have a link next to it
#   ----
#   * always displayed
#   * always displayed 2
#   ----
#
#   [.result]
#   ====
#   * hidden till clicked
#   * hidden till clicked 2
#   ====
#
#
class ViewResultPostprocessor < Asciidoctor::Extensions::Postprocessor
  def process output
    if @document.basebackend? 'html'
      # Eventually we want an API (similar to docinfo hook) for adding content to the header
      replacement = %(<style>
.listingblock a.view-result {
float: right;
font-weight: normal;
text-decoration: none;
font-size: 0.9em;
line-height: 1.4;
margin-top: 0.15em;
}
.exampleblock.result {
display: none;
}
</style>
<script src="http://cdnjs.cloudflare.com/ajax/libs/zepto/1.1.3/zepto.min.js"></script>
<script type="text/javascript">
function toggle_result_block(e) {
  this.prev().toggleClass('stacked');
  this.toggle();
  return false;
}

function insert_result_links() {
  $('.result').each(function(idx, node) {
    znode = $(node);
    title_div = znode.prev().find('.title')
    title_div.append('<a class="view-result" href="#">[ view result ]</a>');
    view_result_link = title_div.children().last();
    view_result_link.on('click', $.proxy(toggle_result_block, znode));
  });
}

$(insert_result_links);
</script>
</head>)
      output = output.sub(/<\/head>/m, replacement)
    end
    output
  end
end
