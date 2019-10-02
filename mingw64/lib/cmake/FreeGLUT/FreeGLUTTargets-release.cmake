#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "FreeGLUT::freeglut" for configuration "Release"
set_property(TARGET FreeGLUT::freeglut APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(FreeGLUT::freeglut PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libfreeglut.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libfreeglut.dll"
  )

list(APPEND _IMPORT_CHECK_TARGETS FreeGLUT::freeglut )
list(APPEND _IMPORT_CHECK_FILES_FOR_FreeGLUT::freeglut "${_IMPORT_PREFIX}/lib/libfreeglut.dll.a" "${_IMPORT_PREFIX}/bin/libfreeglut.dll" )

# Import target "FreeGLUT::freeglut_static" for configuration "Release"
set_property(TARGET FreeGLUT::freeglut_static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(FreeGLUT::freeglut_static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C;RC"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libfreeglut_static.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS FreeGLUT::freeglut_static )
list(APPEND _IMPORT_CHECK_FILES_FOR_FreeGLUT::freeglut_static "${_IMPORT_PREFIX}/lib/libfreeglut_static.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
