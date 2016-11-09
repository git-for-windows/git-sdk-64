RUBY_ENGINE == 'opal' ? (require 'shout-block/extension') : (require_relative 'shout-block/extension')

Extensions.register do
  # defined as a qualified class name
  #block '::ShoutBlock', :shout
  #block '::ShoutBlock'

  # defined as class
  #block ShoutBlock, :shout
  block ShoutBlock

  # defined as instance
  #block ShoutBlock.new, :shout
  #block ShoutBlock.new

  # defined as block
  #block do
  #  PeriodRx = /\.(?= |$)/
  #  named :shout
  #  on_context :paragraph
  #  parse_content_as :simple
  #  name_attributes 'vol'
  #  process do |parent, reader, attrs|
  #    volume = ((attrs.delete 'vol') || 1).to_i
  #    create_paragraph parent, (reader.lines.map {|l| l.upcase.gsub PeriodRx, '!' * volume }), attrs
  #  end
  #end

  # defined as block with lambda as argument to process
  #block do
  #  named :shout
  #  on_context :paragraph
  #  parse_content_as :simple
  #  name_attributes 'vol'
  #  process &-> (parent, reader, attrs) {
  #    volume = ((attrs.delete 'vol') || 1).to_i
  #    create_paragraph parent, (reader.lines.map {|l| l.upcase.gsub PeriodRx, '!' * volume }), attrs
  #  }
  #end
end
