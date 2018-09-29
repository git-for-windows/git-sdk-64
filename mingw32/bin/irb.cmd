:""||{ ""=> %q<-*- ruby -*-
@"%~dp0ruby" -x "%~f0" %*
@exit /b %ERRORLEVEL%
};{ #
bindir="${0%/*}" #
exec "$bindir/ruby" "-x" "$0" "$@" #
>,
}
#!/usr/bin/env ruby
#
#   irb.rb - interactive ruby
#   	$Release Version: 0.9.6 $
#   	$Revision: 40560 $
#   	by Keiju ISHITSUKA(keiju@ruby-lang.org)
#

require "irb"

IRB.start(__FILE__)
