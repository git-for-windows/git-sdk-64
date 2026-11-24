#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "yaml" for configuration "Release"
set_property(TARGET yaml APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(yaml PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libyaml.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libyaml.dll"
  )

list(APPEND _cmake_import_check_targets yaml )
list(APPEND _cmake_import_check_files_for_yaml "${_IMPORT_PREFIX}/lib/libyaml.dll.a" "${_IMPORT_PREFIX}/bin/libyaml.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
