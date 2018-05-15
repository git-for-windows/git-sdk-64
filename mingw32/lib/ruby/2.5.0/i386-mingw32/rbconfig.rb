# encoding: ascii-8bit
# frozen-string-literal: false
#
# The module storing Ruby interpreter configurations on building.
#
# This file was created by mkconfig.rb when ruby was built.  It contains
# build information for ruby which is used e.g. by mkmf to build
# compatible native extensions.  Any changes made to this file will be
# lost the next time ruby is built.

module RbConfig
  RUBY_VERSION.start_with?("2.5.") or
    raise "ruby lib version (2.5.1) doesn't match executable version (#{RUBY_VERSION})"

  # Ruby installed directory.
  TOPDIR = File.dirname(__FILE__).chomp!("/lib/ruby/2.5.0/i386-mingw32")
  # DESTDIR on make install.
  DESTDIR = TOPDIR && TOPDIR[/\A[a-z]:/i] || '' unless defined? DESTDIR
  # The hash configurations stored.
  CONFIG = {}
  CONFIG["DESTDIR"] = DESTDIR
  CONFIG["MAJOR"] = "2"
  CONFIG["MINOR"] = "5"
  CONFIG["TEENY"] = "1"
  CONFIG["PATCHLEVEL"] = "57"
  CONFIG["INSTALL"] = '/usr/bin/install -c'
  CONFIG["EXEEXT"] = ".exe"
  CONFIG["prefix"] = (TOPDIR || DESTDIR + "/mingw32")
  CONFIG["ruby_install_name"] = "$(RUBY_BASE_NAME)"
  CONFIG["RUBY_INSTALL_NAME"] = "$(RUBY_BASE_NAME)"
  CONFIG["RUBY_SO_NAME"] = "msvcrt-$(RUBY_BASE_NAME)$(MAJOR)$(MINOR)0"
  CONFIG["exec"] = "exec"
  CONFIG["ruby_pc"] = "ruby-2.5.pc"
  CONFIG["PACKAGE"] = "ruby"
  CONFIG["BUILTIN_TRANSSRCS"] = " enc/trans/newline.c"
  CONFIG["USE_RUBYGEMS"] = "YES"
  CONFIG["MANTYPE"] = "doc"
  CONFIG["vendorarchhdrdir"] = "$(vendorhdrdir)/$(sitearch)"
  CONFIG["sitearchhdrdir"] = "$(sitehdrdir)/$(sitearch)"
  CONFIG["rubyarchhdrdir"] = "$(rubyhdrdir)/$(arch)"
  CONFIG["vendorhdrdir"] = "$(rubyhdrdir)/vendor_ruby"
  CONFIG["sitehdrdir"] = "$(rubyhdrdir)/site_ruby"
  CONFIG["rubyhdrdir"] = "$(includedir)/$(RUBY_VERSION_NAME)"
  CONFIG["RUBY_SEARCH_PATH"] = ""
  CONFIG["UNIVERSAL_INTS"] = ""
  CONFIG["UNIVERSAL_ARCHNAMES"] = ""
  CONFIG["configure_args"] = " '--prefix=/mingw32' '--build=i686-w64-mingw32' '--host=i686-w64-mingw32' '--target=i686-w64-mingw32' 'build_alias=i686-w64-mingw32' 'host_alias=i686-w64-mingw32' 'target_alias=i686-w64-mingw32' 'CFLAGS=-march=i686 -mtune=generic -O2 -pipe -I/mingw32/include/ncurses -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include' 'LDFLAGS=-pipe' 'CPPFLAGS=-D_FORTIFY_SOURCE=2 -D__USE_MINGW_ANSI_STDIO=1 -DFD_SETSIZE=2048 -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include -I/mingw32/include/ncurses' 'CXXFLAGS=-march=i686 -mtune=generic -O2 -pipe -I/mingw32/include/ncurses -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include'"
  CONFIG["CONFIGURE"] = "configure"
  CONFIG["vendorarchdir"] = "$(vendorlibdir)/$(sitearch)"
  CONFIG["vendorlibdir"] = "$(vendordir)/$(ruby_version)"
  CONFIG["vendordir"] = "$(rubylibprefix)/vendor_ruby"
  CONFIG["sitearchdir"] = "$(sitelibdir)/$(sitearch)"
  CONFIG["sitelibdir"] = "$(sitedir)/$(ruby_version)"
  CONFIG["sitedir"] = "$(rubylibprefix)/site_ruby"
  CONFIG["rubyarchdir"] = "$(rubylibdir)/$(arch)"
  CONFIG["rubylibdir"] = "$(rubylibprefix)/$(ruby_version)"
  CONFIG["ruby_version"] = "2.5.0"
  CONFIG["sitearch"] = "i386-msvcrt"
  CONFIG["arch"] = "i386-mingw32"
  CONFIG["sitearchincludedir"] = "$(includedir)/$(sitearch)"
  CONFIG["archincludedir"] = "$(includedir)/$(arch)"
  CONFIG["sitearchlibdir"] = "$(libdir)/$(sitearch)"
  CONFIG["archlibdir"] = "$(libdir)/$(arch)"
  CONFIG["libdirname"] = "libdir"
  CONFIG["RUBY_EXEC_PREFIX"] = ""
  CONFIG["RUBY_LIB_VERSION"] = ""
  CONFIG["RUBY_LIB_VERSION_STYLE"] = "3\t/* full */"
  CONFIG["RI_BASE_NAME"] = "ri"
  CONFIG["ridir"] = "$(datarootdir)/$(RI_BASE_NAME)"
  CONFIG["rubysitearchprefix"] = "$(rubylibprefix)/$(sitearch)"
  CONFIG["rubyarchprefix"] = "$(rubylibprefix)/$(arch)"
  CONFIG["MAKEFILES"] = "Makefile GNUmakefile"
  CONFIG["PLATFORM_DIR"] = "win32"
  CONFIG["THREAD_MODEL"] = "win32"
  CONFIG["SYMBOL_PREFIX"] = "_"
  CONFIG["EXPORT_PREFIX"] = " "
  CONFIG["COMMON_HEADERS"] = "winsock2.h windows.h"
  CONFIG["COMMON_MACROS"] = ""
  CONFIG["COMMON_LIBS"] = "m"
  CONFIG["MAINLIBS"] = ""
  CONFIG["ENABLE_SHARED"] = "yes"
  CONFIG["DLDLIBS"] = ""
  CONFIG["SOLIBS"] = "$(LIBS)"
  CONFIG["LIBRUBYARG_SHARED"] = "-l$(RUBY_SO_NAME)"
  CONFIG["LIBRUBYARG_STATIC"] = "-l$(RUBY_SO_NAME)-static"
  CONFIG["LIBRUBYARG"] = "$(LIBRUBYARG_SHARED)"
  CONFIG["LIBRUBY"] = "lib$(RUBY_SO_NAME).dll.a"
  CONFIG["LIBRUBY_ALIASES"] = ""
  CONFIG["LIBRUBY_SONAME"] = "lib$(RUBY_SO_NAME).$(SOEXT).$(RUBY_API_VERSION)"
  CONFIG["LIBRUBY_SO"] = "$(RUBY_SO_NAME).dll"
  CONFIG["LIBRUBY_A"] = "lib$(RUBY_SO_NAME)-static.a"
  CONFIG["RUBYW_INSTALL_NAME"] = "$(RUBYW_BASE_NAME)"
  CONFIG["rubyw_install_name"] = "$(RUBYW_INSTALL_NAME)"
  CONFIG["EXTDLDFLAGS"] = ""
  CONFIG["EXTLDFLAGS"] = "-static-libgcc"
  CONFIG["strict_warnflags"] = ""
  CONFIG["warnflags"] = "-Wall -Wextra -Wno-unused-parameter -Wno-parentheses -Wno-long-long -Wno-missing-field-initializers -Wno-tautological-compare -Wno-parentheses-equality -Wno-constant-logical-operand -Wno-self-assign -Wunused-variable -Wimplicit-int -Wpointer-arith -Wwrite-strings -Wdeclaration-after-statement -Wimplicit-function-declaration -Wdeprecated-declarations -Wmisleading-indentation -Wno-packed-bitfield-compat -Wsuggest-attribute=noreturn -Wsuggest-attribute=format -Wimplicit-fallthrough=0 -Wduplicated-cond -Wrestrict"
  CONFIG["debugflags"] = "-ggdb3"
  CONFIG["optflags"] = "-O3 -fno-omit-frame-pointer"
  CONFIG["NULLCMD"] = ":"
  CONFIG["ENABLE_DEBUG_ENV"] = ""
  CONFIG["DLNOBJ"] = "dln.o"
  CONFIG["INSTALL_STATIC_LIBRARY"] = "no"
  CONFIG["EXECUTABLE_EXTS"] = ".exe .com .cmd .bat"
  CONFIG["ARCHFILE"] = ""
  CONFIG["LIBRUBY_RELATIVE"] = "yes"
  CONFIG["EXTOUT"] = ".ext"
  CONFIG["PREP"] = "miniruby$(EXEEXT)"
  CONFIG["CROSS_COMPILING"] = "no"
  CONFIG["TEST_RUNNABLE"] = "yes"
  CONFIG["rubylibprefix"] = "$(libdir)/$(RUBY_BASE_NAME)"
  CONFIG["setup"] = "Setup"
  CONFIG["ENCSTATIC"] = ""
  CONFIG["EXTSTATIC"] = ""
  CONFIG["STRIP"] = "strip"
  CONFIG["SOEXT"] = "dll"
  CONFIG["TRY_LINK"] = ""
  CONFIG["PRELOADENV"] = ""
  CONFIG["LIBPATHENV"] = "PATH"
  CONFIG["RPATHFLAG"] = ""
  CONFIG["LIBPATHFLAG"] = " -L%s"
  CONFIG["LINK_SO"] = ""
  CONFIG["ASMEXT"] = "S"
  CONFIG["LIBEXT"] = "a"
  CONFIG["DLEXT2"] = ""
  CONFIG["DLEXT"] = "so"
  CONFIG["LDSHAREDXX"] = "$(CXX) -shared"
  CONFIG["LDSHARED"] = "$(CC) -shared"
  CONFIG["CCDLFLAGS"] = ""
  CONFIG["STATIC"] = ""
  CONFIG["ARCH_FLAG"] = ""
  CONFIG["DLDFLAGS"] = "-pipe -Wl,--enable-auto-image-base,--enable-auto-import $(DEFFILE)"
  CONFIG["ALLOCA"] = ""
  CONFIG["dsymutil"] = ""
  CONFIG["codesign"] = ""
  CONFIG["POSTLINK"] = ":"
  CONFIG["WERRORFLAG"] = "-Werror"
  CONFIG["CHDIR"] = "cd"
  CONFIG["RMALL"] = "rm -fr"
  CONFIG["RMDIRS"] = "rmdir --ignore-fail-on-non-empty -p"
  CONFIG["RMDIR"] = "rmdir --ignore-fail-on-non-empty"
  CONFIG["CP"] = "cp"
  CONFIG["RM"] = "rm -f"
  CONFIG["PKG_CONFIG"] = "pkg-config"
  CONFIG["DOXYGEN"] = "doxygen"
  CONFIG["DOT"] = "dot"
  CONFIG["MAKEDIRS"] = "/usr/bin/mkdir -p"
  CONFIG["MKDIR_P"] = "/usr/bin/mkdir -p"
  CONFIG["INSTALL_DATA"] = "$(INSTALL) -m 644"
  CONFIG["INSTALL_SCRIPT"] = "$(INSTALL)"
  CONFIG["INSTALL_PROGRAM"] = "$(INSTALL)"
  CONFIG["SET_MAKE"] = ""
  CONFIG["LN_S"] = "cp -pR"
  CONFIG["NM"] = "nm"
  CONFIG["DLLWRAP"] = "dllwrap"
  CONFIG["WINDRES"] = "windres"
  CONFIG["OBJCOPY"] = ":"
  CONFIG["OBJDUMP"] = "objdump"
  CONFIG["ASFLAGS"] = ""
  CONFIG["AS"] = "as"
  CONFIG["ARFLAGS"] = "rcD "
  CONFIG["AR"] = "ar"
  CONFIG["RANLIB"] = "ranlib"
  CONFIG["try_header"] = ""
  CONFIG["CC_VERSION_MESSAGE"] = "i686-w64-mingw32-gcc.exe (Rev2, Built by MSYS2 project) 7.3.0\nCopyright (C) 2017 Free Software Foundation, Inc.\nThis is free software; see the source for copying conditions.  There is NO\nwarranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
  CONFIG["CC_VERSION"] = "$(CC) --version"
  CONFIG["CSRCFLAG"] = ""
  CONFIG["COUTFLAG"] = "-o "
  CONFIG["OUTFLAG"] = "-o "
  CONFIG["CPPOUTFILE"] = "-o conftest.i"
  CONFIG["GNU_LD"] = "yes"
  CONFIG["LD"] = "ld"
  CONFIG["GCC"] = "yes"
  CONFIG["EGREP"] = "/usr/bin/grep -E"
  CONFIG["GREP"] = "/usr/bin/grep"
  CONFIG["CPP"] = "$(CC) -E"
  CONFIG["CXXFLAGS"] = "-march=i686 -mtune=generic -O2 -pipe -I/mingw32/include/ncurses -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include"
  CONFIG["CXX"] = "i686-w64-mingw32-g++"
  CONFIG["OBJEXT"] = "o"
  CONFIG["CPPFLAGS"] = "-D_FORTIFY_SOURCE=2 -D__USE_MINGW_ANSI_STDIO=1 -DFD_SETSIZE=2048 -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include -I/mingw32/include/ncurses -D_WIN32_WINNT=0x0501 -D__MINGW_USE_VC2005_COMPAT $(DEFS) $(cppflags)"
  CONFIG["LDFLAGS"] = "-L. -pipe"
  CONFIG["CFLAGS"] = "-march=i686 -mtune=generic -O2 -pipe -I/mingw32/include/ncurses -IC:/building/msys64/mingw32/lib/libffi-3.2.1/include"
  CONFIG["CC"] = "i686-w64-mingw32-gcc"
  CONFIG["target_os"] = "mingw32"
  CONFIG["target_vendor"] = "w64"
  CONFIG["target_cpu"] = "i386"
  CONFIG["target"] = "i386-w64-mingw32"
  CONFIG["host_os"] = "mingw32"
  CONFIG["host_vendor"] = "w64"
  CONFIG["host_cpu"] = "i686"
  CONFIG["host"] = "i686-w64-mingw32"
  CONFIG["RUBY_VERSION_NAME"] = "$(RUBY_BASE_NAME)-$(ruby_version)"
  CONFIG["RUBYW_BASE_NAME"] = "rubyw"
  CONFIG["RUBY_BASE_NAME"] = "ruby"
  CONFIG["build_os"] = "mingw32"
  CONFIG["build_vendor"] = "w64"
  CONFIG["build_cpu"] = "i686"
  CONFIG["build"] = "i686-w64-mingw32"
  CONFIG["RUBY_API_VERSION"] = "$(MAJOR).$(MINOR)"
  CONFIG["RUBY_PROGRAM_VERSION"] = "2.5.1"
  CONFIG["HAVE_GIT"] = "yes"
  CONFIG["GIT"] = "git"
  CONFIG["cxxflags"] = "$(optflags) $(debugflags) $(warnflags)"
  CONFIG["cppflags"] = ""
  CONFIG["cflags"] = "$(optflags) $(debugflags) $(warnflags)"
  CONFIG["target_alias"] = "i386-w64-mingw32"
  CONFIG["host_alias"] = "i686-w64-mingw32"
  CONFIG["build_alias"] = "i686-w64-mingw32"
  CONFIG["LIBS"] = "-lgmp -lshell32 -lws2_32 -liphlpapi -limagehlp -lshlwapi "
  CONFIG["ECHO_T"] = ""
  CONFIG["ECHO_N"] = "-n"
  CONFIG["ECHO_C"] = ""
  CONFIG["DEFS"] = "-D_FILE_OFFSET_BITS=64"
  CONFIG["mandir"] = "$(datarootdir)/man"
  CONFIG["localedir"] = "$(datarootdir)/locale"
  CONFIG["libdir"] = "$(exec_prefix)/lib"
  CONFIG["psdir"] = "$(docdir)"
  CONFIG["pdfdir"] = "$(docdir)"
  CONFIG["dvidir"] = "$(docdir)"
  CONFIG["htmldir"] = "$(docdir)"
  CONFIG["infodir"] = "$(datarootdir)/info"
  CONFIG["docdir"] = "$(datarootdir)/doc/$(PACKAGE)"
  CONFIG["oldincludedir"] = "/usr/include"
  CONFIG["includedir"] = "$(prefix)/include"
  CONFIG["localstatedir"] = "$(prefix)/var"
  CONFIG["sharedstatedir"] = "$(prefix)/com"
  CONFIG["sysconfdir"] = ""
  CONFIG["datadir"] = "$(datarootdir)"
  CONFIG["datarootdir"] = "$(prefix)/share"
  CONFIG["libexecdir"] = "$(exec_prefix)/libexec"
  CONFIG["sbindir"] = "$(exec_prefix)/sbin"
  CONFIG["bindir"] = "$(exec_prefix)/bin"
  CONFIG["exec_prefix"] = "$(prefix)"
  CONFIG["PACKAGE_URL"] = ""
  CONFIG["PACKAGE_BUGREPORT"] = ""
  CONFIG["PACKAGE_STRING"] = ""
  CONFIG["PACKAGE_VERSION"] = ""
  CONFIG["PACKAGE_TARNAME"] = ""
  CONFIG["PACKAGE_NAME"] = ""
  CONFIG["PATH_SEPARATOR"] = ":"
  CONFIG["SHELL"] = "/bin/sh"
  CONFIG["UNICODE_VERSION"] = "10.0.0"
  CONFIG["archdir"] = "$(rubyarchdir)"
  CONFIG["topdir"] = File.dirname(__FILE__)
  # Almost same with CONFIG. MAKEFILE_CONFIG has other variable
  # reference like below.
  #
  #   MAKEFILE_CONFIG["bindir"] = "$(exec_prefix)/bin"
  #
  # The values of this constant is used for creating Makefile.
  #
  #   require 'rbconfig'
  #
  #   print <<-END_OF_MAKEFILE
  #   prefix = #{Config::MAKEFILE_CONFIG['prefix']}
  #   exec_prefix = #{Config::MAKEFILE_CONFIG['exec_prefix']}
  #   bindir = #{Config::MAKEFILE_CONFIG['bindir']}
  #   END_OF_MAKEFILE
  #
  #   => prefix = /usr/local
  #      exec_prefix = $(prefix)
  #      bindir = $(exec_prefix)/bin  MAKEFILE_CONFIG = {}
  #
  # RbConfig.expand is used for resolving references like above in rbconfig.
  #
  #   require 'rbconfig'
  #   p Config.expand(Config::MAKEFILE_CONFIG["bindir"])
  #   # => "/usr/local/bin"
  MAKEFILE_CONFIG = {}
  CONFIG.each{|k,v| MAKEFILE_CONFIG[k] = v.dup}

  # call-seq:
  #
  #   RbConfig.expand(val)         -> string
  #   RbConfig.expand(val, config) -> string
  #
  # expands variable with given +val+ value.
  #
  #   RbConfig.expand("$(bindir)") # => /home/foobar/all-ruby/ruby19x/bin
  def RbConfig::expand(val, config = CONFIG)
    newval = val.gsub(/\$\$|\$\(([^()]+)\)|\$\{([^{}]+)\}/) {
      var = $&
      if !(v = $1 || $2)
	'$'
      elsif key = config[v = v[/\A[^:]+(?=(?::(.*?)=(.*))?\z)/]]
	pat, sub = $1, $2
	config[v] = false
	config[v] = RbConfig::expand(key, config)
	key = key.gsub(/#{Regexp.quote(pat)}(?=\s|\z)/n) {sub} if pat
	key
      else
	var
      end
    }
    val.replace(newval) unless newval == val
    val
  end
  CONFIG.each_value do |val|
    RbConfig::expand(val)
  end

  # call-seq:
  #
  #   RbConfig.ruby -> path
  #
  # returns the absolute pathname of the ruby command.
  def RbConfig.ruby
    File.join(
      RbConfig::CONFIG["bindir"],
      RbConfig::CONFIG["ruby_install_name"] + RbConfig::CONFIG["EXEEXT"]
    )
  end
end
CROSS_COMPILING = nil unless defined? CROSS_COMPILING
