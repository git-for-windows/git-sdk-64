#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "xxHash::xxhash" for configuration "Release"
set_property(TARGET xxHash::xxhash APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(xxHash::xxhash PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libxxhash.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libxxhash.dll"
  )

list(APPEND _cmake_import_check_targets xxHash::xxhash )
list(APPEND _cmake_import_check_files_for_xxHash::xxhash "${_IMPORT_PREFIX}/lib/libxxhash.dll.a" "${_IMPORT_PREFIX}/bin/libxxhash.dll" )

# Import target "xxHash::xxhsum" for configuration "Release"
set_property(TARGET xxHash::xxhsum APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(xxHash::xxhsum PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/xxhsum.exe"
  )

list(APPEND _cmake_import_check_targets xxHash::xxhsum )
list(APPEND _cmake_import_check_files_for_xxHash::xxhsum "${_IMPORT_PREFIX}/bin/xxhsum.exe" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
