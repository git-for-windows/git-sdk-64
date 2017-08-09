require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include ::Asciidoctor

# A block macro that embeds a Chart into the output document
#
# Usage
#
#   chart::sample.csv[line,800,500,engine=chartist]
#
class ChartBlockMacro < Extensions::BlockMacroProcessor
  use_dsl
  named :chart
  name_positional_attributes 'type', 'width', 'height'

  def process(parent, target, attrs)
    data_path = parent.normalize_asset_path(target, 'target')
    read_data = parent.read_asset(data_path, warn_on_failure: true, normalize: true)
    unless read_data.nil? || read_data.empty?
      engine = ChartBackend.resolve_engine attrs, parent.document
      raw_data = PlainRubyCSV.parse(read_data)
      html = ChartBackend.process engine, attrs, raw_data
      create_pass_block parent, html, attrs, subs: nil
    end
  end
end

class ChartBlockProcessor < Extensions::BlockProcessor
  use_dsl
  named :chart
  on_context :literal
  name_positional_attributes 'type', 'width', 'height'
  parse_content_as :raw

  def process(parent, reader, attrs)
    engine = ChartBackend.resolve_engine attrs, parent.document
    raw_data = PlainRubyCSV.parse(reader.source)
    html = ChartBackend.process engine, attrs, raw_data
    create_pass_block parent, html, attrs, subs: nil
  end
end

class ChartAssetsDocinfoProcessor < Extensions::DocinfoProcessor
  use_dsl
  #at_location :head

  C3JS_STYLESHEET = '<link rel="stylesheet" href="http://cdnjs.cloudflare.com/ajax/libs/c3/0.3.0/c3.min.css">'
  D3JS_SCRIPT = '<script src="http://cdnjs.cloudflare.com/ajax/libs/d3/3.4.11/d3.min.js" charset="utf-8"></script>'
  C3JS_SCRIPT = '<script src="http://cdnjs.cloudflare.com/ajax/libs/c3/0.3.0/c3.min.js"></script>'

  CHARTIST_STYLESHEET = '<link rel="stylesheet" href="http://cdn.jsdelivr.net/chartist.js/latest/chartist.min.css">'
  CHARTIST_SCRIPT = '<script src="http://cdn.jsdelivr.net/chartist.js/latest/chartist.min.js"></script>'

  CHARTJS_SCRIPT = '<script src="http://cdnjs.cloudflare.com/ajax/libs/Chart.js/1.0.2/Chart.min.js"></script>'

  def process doc
    # TODO Import only the required engines
    # TODO Honor linkcss and copycss
    %(
#{C3JS_STYLESHEET}
#{D3JS_SCRIPT}
#{C3JS_SCRIPT}
#{CHARTIST_STYLESHEET}
#{CHARTIST_SCRIPT}
#{CHARTJS_SCRIPT})
  end
end

class ChartBackend

  def self.resolve_engine attrs, document
    if attrs.key? 'engine'
      attrs['engine'].downcase
    elsif document.attributes.key? 'chart-engine'
      document.attributes['chart-engine'].downcase
    else
      'c3js'
    end
  end

  def self.process engine, attrs, raw_data
    # TODO Check that the engine can process the required type (bar, line, step...)
    type = attrs['type']
    case engine
      when 'c3js'
        data, labels = C3jsChartBuilder.prepare_data(raw_data)
        (case type
          when 'bar' then C3jsChartBuilder.bar data, labels, attrs
          when 'line' then C3jsChartBuilder.line data, labels, attrs
          when 'step' then C3jsChartBuilder.step data, labels, attrs
          when 'spline' then C3jsChartBuilder.spline data, labels, attrs
          else
            # By default chart line
            C3jsChartBuilder.line data, labels, attrs
        end)
      when 'chartist'
        data, labels = ChartistChartBuilder.prepare_data(raw_data)
        (case type
          when 'bar' then ChartistChartBuilder.bar data, labels, attrs
          when 'line' then ChartistChartBuilder.line data, labels, attrs
          else
            # By default chart line
            ChartistChartBuilder.line data, labels, attrs
        end)
      when 'chartjs'
        data, labels = ChartjsChartBuilder.prepare_data(raw_data)
        (case type
          when 'line' then ChartjsChartBuilder.line data, labels, attrs
          else
            # By default chart line
            ChartjsChartBuilder.line data, labels, attrs
        end)
    end
  end
end


class C3jsChartBuilder

  def self.bar(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_bar_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.line(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_line_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.step(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_step_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.spline(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_spline_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.create_chart_div(chart_id)
    %(<div id="#{chart_id}"></div>)
  end

  def self.get_chart_id
    # TODO Read from attributes ?
    'chart' + PlainRubyRandom.uuid
  end

  def self.prepare_data(raw_data)
    labels = raw_data[0]
    raw_data.shift
    raw_data.map.with_index do |row, index|
      row.unshift "#{index}"
    end
    return raw_data, labels
  end

  def self.chart_bar_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
c3.generate({
  bindto: '##{chart_id}',
  size: { height: #{chart_height}, width: #{chart_width} },
  data: {
    columns: #{data.to_s},
    type: 'bar'
  },
  axis: {
    x: {
      type: 'category',
      categories: #{labels.to_s}
    }
  }
});
</script>)
  end

  def self.chart_line_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
c3.generate({
  bindto: '##{chart_id}',
  size: { height: #{chart_height}, width: #{chart_width} },
  data: {
    columns: #{data.to_s}
  },
  axis: {
    x: {
      type: 'category',
      categories: #{labels.to_s}
    }
  }
});
</script>)
  end

  def self.chart_step_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
c3.generate({
  bindto: '##{chart_id}',
  size: { height: #{chart_height}, width: #{chart_width} },
  data: {
    columns: #{data.to_s},
    type: 'step'
  },
  axis: {
    x: {
      type: 'category',
      categories: #{labels.to_s}
    }
  }
});
</script>)
  end

  def self.chart_spline_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
c3.generate({
  bindto: '##{chart_id}',
  size: { height: #{chart_height}, width: #{chart_width} },
  data: {
    columns: #{data.to_s},
    type: 'spline'
  },
  axis: {
    x: {
      type: 'category',
      categories: #{labels.to_s}
    }
  }
});
</script>)
  end

  def self.to_html(chart_div, chart_script)
    %(
    #{chart_div}
    #{chart_script}
    )
  end

  def self.get_chart_height attrs
    (attrs.key? 'height') ? attrs['height'] : '400'
  end

  def self.get_chart_width attrs
    (attrs.key? 'width') ? attrs['width'] : '600'
  end

end

class ChartjsChartBuilder

  def self.line(data, labels, attrs)
    default_colors = [{r:220,g:220,b:220}, {r:151,g:187,b:205}]
    datasets = data.map do |set|
      color = default_colors[data.index(set) % 2]
      color_rgba = "rgba(#{color[:r]},#{color[:g]},#{color[:b]},1.0)"
      %(
{
  fillColor: "#{color_rgba.gsub('1.0', '0.2')}",
  strokeColor: "#{color_rgba}",
  pointColor: "#{color_rgba}",
  pointHighlightStroke: "#{color_rgba}",
  pointStrokeColor: "#fff",
  pointHighlightFill: "#fff",
  data: #{set.to_s}
}
      )
    end.join(',')
    # TODO Generate unique id (or read from attributes)
    chart_id = 'chart' + PlainRubyRandom.uuid
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    chart_canvas = %(<div style="width:#{chart_width}px; height:#{chart_height}px"><canvas id="#{chart_id}"></canvas></div>)
    chart_init_ctx_script = %(var ctx = document.getElementById("#{chart_id}").getContext("2d");)
    chart_init_data_script = %(var data = {
  labels: #{labels.to_s},
  datasets: [
    #{datasets}
  ]
};)
    chart_init_script = 'var chart = new Chart(ctx).Line(data, {responsive : true});'
    %(
    #{chart_canvas}<script type="text/javascript">window.onload = function() {
    #{chart_init_ctx_script}
    #{chart_init_data_script}
    #{chart_init_script}
}
</script>)
  end

  def self.prepare_data(raw_data)
    labels = raw_data[0]
    raw_data.shift
    return raw_data, labels
  end

  def self.get_chart_height attrs
    (attrs.key? 'height') ? attrs['height'] : '400'
  end

  def self.get_chart_width attrs
    (attrs.key? 'width') ? attrs['width'] : '600'
  end

end

class ChartistChartBuilder

  def self.bar(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_bar_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.line(data, labels, attrs)
    chart_id = get_chart_id
    chart_div = create_chart_div(chart_id)
    chart_generate_script = chart_line_script(chart_id, data, labels, attrs)
    to_html(chart_div, chart_generate_script)
  end

  def self.create_chart_div(chart_id)
    %(<div id="#{chart_id}"class="ct-chart"></div>)
  end

  def self.get_chart_id
    # TODO Read from attributes ?
    'chart' + PlainRubyRandom.uuid
  end

  def self.prepare_data(raw_data)
    labels = raw_data[0]
    raw_data.shift
    return raw_data, labels
  end

  def self.chart_bar_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
var options = {
  height: '#{chart_height}',
  colors:["#72B3CC", "#8EB33B"]
};
var data = {
  labels: #{labels.to_s},
  series: #{data.to_s}
};
new Chartist.Bar('##{chart_id}', data, options);
</script>)
  end

  def self.chart_line_script(chart_id, data, labels, attrs)
    chart_height = get_chart_height attrs
    chart_width = get_chart_width attrs
    %(
<script type="text/javascript">
var options = {
  height: '#{chart_height}',
  width: '#{chart_width}',
  colors:["#72B3CC", "#8EB33B"]
};
var data = {
  labels: #{labels.to_s},
  series: #{data.to_s}
};
new Chartist.Line('##{chart_id}', data, options);
</script>)
  end

  def self.to_html(chart_div, chart_script)
    %(
    #{chart_div}
    #{chart_script}
    )
  end

  def self.get_chart_height attrs
    (attrs.key? 'height') ? attrs['height'] : '400'
  end

  def self.get_chart_width attrs
    (attrs.key? 'width') ? attrs['width'] : '600'
  end

end

class PlainRubyCSV

  def self.parse(data)
    result = []
    data.each_line do |line|
      line_chomp = line.chomp
      result.push(line_chomp.split(','))
    end
    result
  end

  def self.read(filename)
    result = []
    File.open(filename).each do |line|
      line_chomp = line.chomp
      result.push(line_chomp.split(','))
    end
    result
  end
end


class PlainRubyRandom

  def self.uuid
    (0...8).map { (65 + rand(26)).chr }.join
  end
end
