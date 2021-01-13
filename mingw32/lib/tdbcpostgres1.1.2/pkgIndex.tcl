# Index file to load the TDBC Postgres package.

if {[catch {package require Tcl 8.6}]} {
    return
}
package ifneeded tdbc::postgres 1.1.2 \
    "[list source [file join $dir tdbcpostgres.tcl]]\;\
    [list load [file join $dir tdbcpostgres112.dll] tdbcpostgres]"
