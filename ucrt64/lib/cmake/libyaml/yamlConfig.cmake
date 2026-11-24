# Config file for the yaml library.
#
# It defines the following variables:
#   yaml_LIBRARIES    - libraries to link against


####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was yamlConfig.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

####################################################################################

set_and_check(yaml_TARGETS "${PACKAGE_PREFIX_DIR}/lib/cmake/libyaml/yamlTargets.cmake")

if(NOT yaml_TARGETS_IMPORTED)
  set(yaml_TARGETS_IMPORTED 1)
  include(${yaml_TARGETS})
endif()

set(yaml_LIBRARIES yaml)

