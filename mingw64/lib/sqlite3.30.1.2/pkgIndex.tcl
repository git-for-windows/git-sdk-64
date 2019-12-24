#
# Tcl package index file
#
# Note sqlite*3* init specifically
#
package ifneeded sqlite3 3.30.1.2 \
    [list load [file join $dir sqlite33012.dll] Sqlite3]
