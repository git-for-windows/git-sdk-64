# WIN32OLE

WIN32OLE objects represent OLE Automation object in Ruby.

By using WIN32OLE, you can access OLE server like VBScript.

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'win32ole'
```

And then execute:

    $ bundle install

Or install it yourself as:

    $ gem install win32ole

## Usage

```
require 'win32ole'

excel = WIN32OLE.new('Excel.Application')
excel.visible = true
workbook = excel.Workbooks.Add();
worksheet = workbook.Worksheets(1);
worksheet.Range("A1:D1").value = ["North","South","East","West"];
worksheet.Range("A2:B2").value = [5.2, 10];
worksheet.Range("C2").value = 8;
worksheet.Range("D2").value = 20;

range = worksheet.Range("A1:D2");
range.select
chart = workbook.Charts.Add;

workbook.saved = true;

excel.ActiveWorkbook.Close(0);
excel.Quit();
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake test` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/ruby/win32ole.

