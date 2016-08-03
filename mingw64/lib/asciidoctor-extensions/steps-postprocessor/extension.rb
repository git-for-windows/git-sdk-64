require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

class StepsPostprocessor < Extensions::Postprocessor
  def process document, output
    replacement = %(<style>
ol.arabic {
list-style-type: none;
}

ol {
counter-reset: li;
list-style-type: none;
list-style: none;
position: relative;
padding-bottom: 8px;
}


ol, li {
border: 0;
margin: 0;
padding: 0;
font-size: 100%;
}


ol > li {
padding: 5px 0 5px 55px;
position: relative;
margin-bottom: 5px;
}

ol > li:before {
content: counter(li);
counter-increment: li;
position: absolute;
top: 0;
left: 0;
height: 100%;
width: 30px;
padding: 0 10px 0 0;
color: #999;
font-size: 20px;
font-weight: bold;
line-height: 35px;
text-align: right;
border-right: 1px solid #ddd;
}

ol > li:after {
content: ".";
display: block;
clear: both;
visibility: hidden;
line-height: 0;
height: 0;
}
</style>
)
    output.sub /(?=<\/head>)/, replacement
  end
end
