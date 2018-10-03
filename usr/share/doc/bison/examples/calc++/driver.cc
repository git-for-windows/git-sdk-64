#include "driver.hh"
#include "parser.hh"

driver::driver ()
  : trace_parsing (false), trace_scanning (false)
{
  variables["one"] = 1;
  variables["two"] = 2;
}

int
driver::parse (const std::string &f)
{
  file = f;
  location.initialize (&file);
  scan_begin ();
  yy::parser parser (*this);
  parser.set_debug_level (trace_parsing);
  int res = parser.parse ();
  scan_end ();
  return res;
}
