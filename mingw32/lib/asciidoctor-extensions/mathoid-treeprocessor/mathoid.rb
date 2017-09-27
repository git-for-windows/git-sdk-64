require 'net/http'
require 'open3'
require 'shellwords'

# A simple processor manager and invoker for Mathoid.
#
# Mathoid is a Node.js-based REST service that uses MathJax to convert
# AsciiMath, LaTeX and MathML to SVG (and other formats).
#
# In order to use this library, you need Node.js. The code has been tested with
# Node.js 4.4.7.
#
#  $ nvm install 4
#
# You also need the librsvg development package:
#
#  # Debian
#  $ [sudo] apt-get install librsvg2-dev
#
#  # Fedora
#  $ [sudo] dnf install librsvg2-devel
#
#  # OS X
#  $ brew install librsvg
#    export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig
#
# Next, you must install the mathoid module from the 
# mathoid-treeprocessor directory:
#
#  npm install mathoid
#
# Now test that you can run the server using:
#
#  npm run mathoid
#
# You can verify that the server is running using:
#
#  curl localhost:10044/_info
#
# Stop the server by pressing Ctrl+C.
#
# Usage:
#
#  mathoid = Mathoid.new
#  mathoid.start
#  svg_data = mathoid.convert 'x^2', :asciimath, color: 'rgb(25,25,24)'
#  mathoid.stop
#
class Mathoid
  MathoidHome = ENV['MATHOID_HOME'] || (::File.expand_path 'node_modules/mathoid', (::File.dirname __FILE__))
  MathoidServerCmd = ::Shellwords.escape %(#{MathoidHome}/server.js)
  MathoidConfig = ::File.expand_path 'mathoid-config.yaml', (::File.dirname __FILE__)

  SvgStartTagRx = /\A<svg\b.*?>/m
  ViewBoxAttrRx = /\sviewBox=(["'])(.*?)\1/
  VacuumSvgAttrsRx = /\s(?:width|height|style)=(["']).*?\1/
  TransformMatrixRx = /<g (.+ )?transform="matrix\(((?:\S+ ){4})\S+ \S+\)"/
  ColorRx = /<g stroke="[^"]+" fill="[^"]+"/

  attr_reader :host
  attr_reader :port
  attr_reader :base_uri
  attr_reader :pid

  def initialize opts = {}
    # FIXME could read these from the Mathoid config file
    @host = opts[:host] || 'localhost'
    @port = opts[:port] || 10044
    @base_uri = %(http://#{@host}:#{@port})
    @svg_uri = URI %(#{@base_uri}/svg)
    @info_uri = URI %(#{@base_uri}/_info)
    @pid = nil
  end

  def convert equation, syntax = :tex, opts = {}
    unless @pid
      warn 'Mathoid server is not running.'
      return
    end
    syntax = :tex if (str_syntax = syntax.to_s).start_with? 'latex'
    params = { q: equation, type: str_syntax }

    # FIXME this hangs if an exception is thrown on the server
    # QUESTION still true?
    if ::Net::HTTPSuccess === (res = ::Net::HTTP.post_form @svg_uri, params)
      postprocess_svg_data res.body, opts
    else
      raise %(Could not process math. Reason: #{res.message})
    end
  end

  def convert_to_file filename, equation, syntax = :tex, opts = {}
    if (data = convert equation, syntax, opts)
      ::File.open(filename, 'w') {|fd| fd.write data }
    end
  end

  def postprocess_svg_data data, opts = {}
    start_tag = (SvgStartTagRx.match data)[0]

    viewBox = ((start_tag.match ViewBoxAttrRx)[2].split ' ').map &:to_f
    horizontal_shift = 0
    vertical_shift = 0

    # FIXME negative viewbox correction no longer necessary as of prawn-svg 0.16.0
    if viewBox[0] < 0
      horizontal_shift = viewBox[0].abs
      viewBox[0] = 0
    end
    if viewBox[1] < 0
      vertical_shift = viewBox[1].abs
      viewBox[1] = 0
    end

    # add a fixed amount of padding around equation
    #padding = opts.fetch :padding, (100)
    padding = opts.fetch :padding, ([viewBox[2], viewBox[3]].min * 0.1)

    # NOTE correct vertical misalignment
    # QUESTION still necessary?
    #vertical_shift -= viewBox[3] * 0.015

    viewBox[2] += padding * 2
    horizontal_shift += padding
    viewBox[3] += padding * 2
    vertical_shift += padding

    new_start_tag = start_tag.
        gsub(VacuumSvgAttrsRx, '').
        sub(ViewBoxAttrRx, %( viewBox="#{viewBox * ' '}"))

    color = opts.fetch :color, 'rgb(25,25,24)'

    %(#{new_start_tag}#{data[start_tag.length..-1]}).
        sub(TransformMatrixRx, %[<g \\1transform="matrix(\\2#{horizontal_shift} #{vertical_shift})"]).
        sub(ColorRx, %(<g stroke="#{color}" fill="#{color}"))
  end

  def start
    if @pid
      # TODO also check if server is available
      warn 'Ignoring request to start already running server.'
      return false
    end
    input, output, wait_thr = ::Open3.popen2 %(#{MathoidServerCmd} -c #{MathoidConfig})
    sleep(waited = 1) # it will take at least a second to start up the server
    started = false
    while !started
      begin
        if ::Net::HTTPSuccess === (res = ::Net::HTTP.get_response @info_uri)
          started = true
        end
      rescue ::Errno::ECONNREFUSED; end
      unless started
        if waited >= 5
          warn 'Failed to start Mathoid server. Check that a process is not already running.'
          @pid = wait_thr.pid
          stop
          return false
        end
        sleep 0.25
        waited += 0.25
      end
    end
    @pid = wait_thr.pid
    true
  end

  def stop
    if @pid
      # refer to node_modules/service-runner/lib/master.js to see how the SIGTERM signal is handled
      ::Process.kill 'SIGTERM', @pid
      ::Process.wait @pid rescue nil
      true
    else
      false
    end
  ensure
    @pid = nil
  end

  def running?
    !!@pid
  end
end

if ARGV[0] == 'test'
  mathoid = Mathoid.new
  mathoid.start
  puts mathoid.convert '\\frac{\\frac{1}{x}+\\frac{1}{y}}{y-z}', :tex, color: 'rgb(25,25,24)'
  mathoid.stop
end
