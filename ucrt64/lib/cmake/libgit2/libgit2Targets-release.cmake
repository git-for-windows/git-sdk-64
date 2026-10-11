#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "libgit2::libgit2package" for configuration "Release"
set_property(TARGET libgit2::libgit2package APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(libgit2::libgit2package PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libgit2.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libgit2-1.9.dll"
  )

list(APPEND _cmake_import_check_targets libgit2::libgit2package )
list(APPEND _cmake_import_check_files_for_libgit2::libgit2package "${_IMPORT_PREFIX}/lib/libgit2.dll.a" "${_IMPORT_PREFIX}/bin/libgit2-1.9.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
