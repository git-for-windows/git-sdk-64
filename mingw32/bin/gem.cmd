:""||{ ""=> %q<-*- ruby -*-
@"%~dp0ruby" -x "%~f0" %*
@exit /b %ERRORLEVEL%
};{ #
bindir="${0%/*}" #
exec "$bindir/ruby" "-x" "$0" "$@" #
>,
}
#!/usr/bin/env ruby
# frozen_string_literal: true

#--
# Copyright 2006 by Chad Fowler, Rich Kilmer, Jim Weirich and others.
# All rights reserved.
# See LICENSE.txt for permissions.
#++

require "rubygems/gem_runner"

Gem::GemRunner.new.run ARGV.clone
