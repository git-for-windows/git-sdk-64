# -*- tcl -*-
# Tcl package index file, version 1.1
#
# Make sure that TDBC is running in a compatible version of Tcl, and
# that TclOO is available.

if {![package vsatisfies [package provide Tcl] 8.6-]} {
    return
}
apply {{dir} {
    set libraryfile [file join $dir tdbc.tcl]
    if {![file exists $libraryfile] && [info exists ::env(TDBC_LIBRARY)]} {
	set libraryfile [file join $::env(TDBC_LIBRARY) tdbc.tcl]
    }
    if {[package vsatisfies [package provide Tcl] 9.0-]} {
	package ifneeded tdbc 1.1.10 \
		"package require TclOO;\
		[list load [file join $dir tcl9tdbc1110.dll] [string totitle tdbc]]\;\
		[list source -encoding utf-8 $libraryfile]"
    } else {
	package ifneeded tdbc 1.1.10 \
		"package require TclOO;\
		[list load [file join $dir tdbc1110.dll] [string totitle tdbc]]\;\
		[list source -encoding utf-8 $libraryfile]"
    }
}} $dir
