# Index file to load the TDBC Postgres package.

if {![package vsatisfies [package provide Tcl] 8.6-]} {
    return
}
if {[package vsatisfies [package provide Tcl] 9.0-]} {
    package ifneeded tdbc::postgres 1.1.10 \
	    "[list source -encoding utf-8 [file join $dir tdbcpostgres.tcl]]\;\
	    [list load [file join $dir tcl9tdbcpostgres1110.dll] [string totitle tdbcpostgres]]"
} else {
    package ifneeded tdbc::postgres 1.1.10 \
	    "[list source -encoding utf-8 [file join $dir tdbcpostgres.tcl]]\;\
	    [list load [file join $dir tdbcpostgres1110.dll] [string totitle tdbcpostgres]]"
}
