# Index file to load the TDBC ODBC package.

if {[catch {package require Tcl 8.6}]} {
    return
}
package ifneeded tdbc::odbc 1.1.0 \
    "[list source [file join $dir tdbcodbc.tcl]]\;\
    [list load [file join $dir tdbcodbc110.dll] tdbcodbc]"
