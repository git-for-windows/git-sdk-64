#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "ZLIB1DLL::ZLIB1DLL" for configuration "Release"
set_property(TARGET ZLIB1DLL::ZLIB1DLL APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ZLIB1DLL::ZLIB1DLL PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libzlib1.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/zlib1.dll"
  )

list(APPEND _cmake_import_check_targets ZLIB1DLL::ZLIB1DLL )
list(APPEND _cmake_import_check_files_for_ZLIB1DLL::ZLIB1DLL "${_IMPORT_PREFIX}/lib/libzlib1.dll.a" "${_IMPORT_PREFIX}/bin/zlib1.dll" )

# Import target "ZLIB1DLL::ZLIBWAPI" for configuration "Release"
set_property(TARGET ZLIB1DLL::ZLIBWAPI APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ZLIB1DLL::ZLIBWAPI PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libzlibwapi.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libzlibwapi.dll"
  )

list(APPEND _cmake_import_check_targets ZLIB1DLL::ZLIBWAPI )
list(APPEND _cmake_import_check_files_for_ZLIB1DLL::ZLIBWAPI "${_IMPORT_PREFIX}/lib/libzlibwapi.dll.a" "${_IMPORT_PREFIX}/bin/libzlibwapi.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
