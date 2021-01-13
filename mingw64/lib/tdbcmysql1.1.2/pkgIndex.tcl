# Index file to load the TDBC MySQL package.

if {[catch {package require Tcl 8.6}]} {
    return
}
package ifneeded tdbc::mysql 1.1.2 \
    "[list source [file join $dir tdbcmysql.tcl]]\;\
    [list load [file join $dir tdbcmysql112.dll] tdbcmysql]"
