# Index file to load the TDBC MySQL package.

if {![package vsatisfies [package provide Tcl] 8.6-]} {
    return
}
if {[package vsatisfies [package provide Tcl] 9.0-]} {
    package ifneeded tdbc::mysql 1.1.10 \
	    "[list source -encoding utf-8 [file join $dir tdbcmysql.tcl]]\;\
	    [list load [file join $dir tcl9tdbcmysql1110.dll] [string totitle tdbcmysql]]"
} else {
    package ifneeded tdbc::mysql 1.1.10 \
	    "[list source -encoding utf-8 [file join $dir tdbcmysql.tcl]]\;\
	    [list load [file join $dir tdbcmysql1110.dll] [string totitle tdbcmysql]]"
}
