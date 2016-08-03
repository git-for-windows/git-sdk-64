require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'
require 'open3'
require 'net/http'

# A simple processor manager and invoker for Mathoid.
#
# Mathoid is a PhantomJS script that uses MathJax to convert AsciiMath,
# LaTeX and MathML to SVG output.
#
# In order to use, first install phantomjs:
#
#  npm install phantomjs
#
# Next, clone the mathoid repository (provides the main.js and engine.js scripts)
#
#  git clone -b integration --recursive https://github.com/mojavelinux/mathoid
#
# Usage:
#
#  mathoid = Mathoid.new
#  mathoid.start
#  svg_data = mathoid.convert 'x^2', :asciimath, color: 'rgb(25,25,24)'
#  mathoid.stop
#
class Mathoid
  MathoidHome = ENV['MATHOID_HOME'] || ::File.expand_path('mathoid', ::File.dirname(__FILE__))
  # NOTE could also get the phantomjs path by passing "console.log(require('phantomjs').path)" to node
  PhantomJsCmd = ENV['PHANTOMJS'] || 'phantomjs'

  ViewBoxAttrRx = / viewBox="([^"]+)"/
  VacuumStyleAttrRx = /\A<svg (?:(.+) )?style="[^"]+"/
  VacuumHrefAttrRx = / href="[^"]+"/
  TagBoundaryRx = /(?<!\A)(<(\w+)[^>]*><\/\2>|<\w+[^>]*>|<\/\w+>)/
  TransformMatrixRx = /<g (.+ )?transform="matrix\(((?:\S+ ){4})\S+ \S+\)"/
  ColorRx = /<g stroke="[^"]+" fill="[^"]+"/

  attr_reader :host
  attr_reader :port
  attr_reader :base_uri
  attr_reader :pid

  def initialize opts = {}
    @host = opts[:host] || 'localhost'
    @port = opts[:port] || 16000
    @base_uri = URI %(http://#{@host}:#{@port})
    @pid = nil
  end

  def convert equation, syntax = :tex, opts = {}
    unless @pid
      warn 'Mathoid server is not running.'
      return
    end
    syntax = :tex if syntax.to_s.start_with? 'latex'
    uri = @base_uri.dup
    params = { q: equation, type: %(#{syntax}) }
    uri.query = ::URI.encode_www_form params

    # FIXME this hangs if an exception is thrown on the server
    if ::Net::HTTPSuccess === (res = ::Net::HTTP.get_response uri)
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
    data = data.
        sub(VacuumStyleAttrRx, '<svg \1').
        gsub(VacuumHrefAttrRx, '').
        gsub(TagBoundaryRx, %(\n\\1))

    viewBox = (data.match(ViewBoxAttrRx)[1].split ' ').map &:to_f
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
    vertical_shift -= viewBox[3] * 0.015

    viewBox[2] += padding * 2
    horizontal_shift += padding
    viewBox[3] += padding * 2
    vertical_shift += padding

    color = opts.fetch :color, 'rgb(25,25,24)'

    data.
        sub(ViewBoxAttrRx, %( viewBox="#{viewBox * ' '}")).
        sub(TransformMatrixRx, %[<g \\1transform="matrix(\\2#{horizontal_shift} #{vertical_shift})"]).
        sub(ColorRx, %(<g stroke="#{color}" fill="#{color}"))
  end

  def start
    if @pid
      warn 'Ignoring request to start already running server.'
      return false
    end
    input, output, wait_thr = ::Open3.popen2 %(#{PhantomJsCmd} main.js), chdir: MathoidHome
    # FIXME implement more robust startup status handling
    while (status = output.gets)
      if status.include? 'started'
        break
      elsif status.include? 'failed'
        warn 'Failed to start PhantomJS. Check that a process is not already running.'
        return false
      end
    end
    @pid = wait_thr.pid
    true
  end

  def stop
    if @pid
      # refer to node_modules/.bin/phantomjs to see how this signal is handled
      ::Process.kill 'SIGTERM', @pid
      ::Process.wait @pid rescue nil
      true
    else
      false
    end
  end

  def running?
    !!@pid
  end
end
