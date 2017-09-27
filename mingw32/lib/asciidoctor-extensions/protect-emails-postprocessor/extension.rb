require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

# Function rewrite_response is taken from
# https://github.com/amsardesai/middleman-protect-emails v0.4.0
# Copyright (c) 2014 Ankit Sardesai
# MIT license

class ProtectEmailsDocinfoProcessor < Asciidoctor::Extensions::DocinfoProcessor
  # DocinfoProcessor as used in the chart-block-macro extension for adding HTML in head or footer
  use_dsl
  at_location :footer # script needs to be included in the body

  ROT13_SCRIPT = '<script type="text/javascript">!function(){try{var a,b,c,d,g=document.getElementsByTagName("a");for(c=0;g.length-c;c++)try{b=g[c].getAttribute("href"),b&&b.indexOf("#email-protection-")>-1&&b.length>19&&(a="",d=19+b.indexOf("#email-protection-"),b.length>d&&(a=b.substr(18).replace(/[a-zA-Z]/g,function(a){return String.fromCharCode(("Z">=a?90:122)>=(a=a.charCodeAt(0)+13)?a:a-26)})),g[c].setAttribute("href","mailto:"+a))}catch(h){}}catch(h){}}();</script>'

  def process doc
    %(#{ROT13_SCRIPT})
  end
end

class ProtectEmailsPostprocessor < Asciidoctor::Extensions::Postprocessor
  def process document, output
    output = rewrite_response(output)
  end

  private

  def rewrite_response(body)

    # Keeps track of email replaces
    replaced_email = false

    # Replaces mailto links with ROT13 equivalent
    # TODO: Don't replace plaintext mailto links
    #       Please contribute upstream
    invalid_character = '\s"\'>'
    email_username = "[^@#{invalid_character}]+"
    email_domain = "[^?#{invalid_character}]+"
    email_param = "[^&#{invalid_character}]+"
    new_content = body.gsub /mailto:(#{email_username}@#{email_domain}(\?#{email_param}(\&#{email_param})*)?)/i do
      replaced_email = true
      email = $1.tr 'A-Za-z','N-ZA-Mn-za-m'
      "#email-protection-#{email}"
    end

    return new_content
  end
end
