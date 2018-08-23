#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "gr2_graphite2" for configuration "Release"
set_property(TARGET gr2_graphite2 APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(gr2_graphite2 PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libgraphite2.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libgraphite2.dll"
  )

list(APPEND _IMPORT_CHECK_TARGETS gr2_graphite2 )
list(APPEND _IMPORT_CHECK_FILES_FOR_gr2_graphite2 "${_IMPORT_PREFIX}/lib/libgraphite2.dll.a" "${_IMPORT_PREFIX}/bin/libgraphite2.dll" )

# Import target "gr2_graphite2_static" for configuration "Release"
set_property(TARGET gr2_graphite2_static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(gr2_graphite2_static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libgraphite2.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS gr2_graphite2_static )
list(APPEND _IMPORT_CHECK_FILES_FOR_gr2_graphite2_static "${_IMPORT_PREFIX}/lib/libgraphite2.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
