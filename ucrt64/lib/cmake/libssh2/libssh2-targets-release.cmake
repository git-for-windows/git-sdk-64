#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "libssh2::libssh2_static" for configuration "Release"
set_property(TARGET libssh2::libssh2_static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(libssh2::libssh2_static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libssh2.a"
  )

list(APPEND _cmake_import_check_targets libssh2::libssh2_static )
list(APPEND _cmake_import_check_files_for_libssh2::libssh2_static "${_IMPORT_PREFIX}/lib/libssh2.a" )

# Import target "libssh2::libssh2_shared" for configuration "Release"
set_property(TARGET libssh2::libssh2_shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(libssh2::libssh2_shared PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libssh2.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libssh2-1.dll"
  )

list(APPEND _cmake_import_check_targets libssh2::libssh2_shared )
list(APPEND _cmake_import_check_files_for_libssh2::libssh2_shared "${_IMPORT_PREFIX}/lib/libssh2.dll.a" "${_IMPORT_PREFIX}/bin/libssh2-1.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
