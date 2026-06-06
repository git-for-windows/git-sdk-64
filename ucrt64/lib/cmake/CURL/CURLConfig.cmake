#***************************************************************************
#                                  _   _ ____  _
#  Project                     ___| | | |  _ \| |
#                             / __| | | | |_) | |
#                            | (__| |_| |  _ <| |___
#                             \___|\___/|_| \_\_____|
#
# Copyright (C) Daniel Stenberg, <daniel@haxx.se>, et al.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at https://curl.se/docs/copyright.html.
#
# You may opt to use, copy, modify, merge, publish, distribute and/or sell
# copies of the Software, and permit persons to whom the Software is
# furnished to do so, under the terms of the COPYING file.
#
# This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
# KIND, either express or implied.
#
# SPDX-License-Identifier: curl
#
###########################################################################

####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was curl-config.in.cmake                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

option(CURL_USE_CMAKECONFIG "Enable detecting CURL dependencies via CMake Config. Default: OFF"
  "OFF")
option(CURL_USE_PKGCONFIG "Enable pkg-config to detect CURL dependencies. Default: ON"
  "ON")

if(CMAKE_VERSION VERSION_LESS 3.18)
  message(STATUS "CURL: CURL-specific Find modules require "
    "CMake 3.18 or upper, found: ${CMAKE_VERSION}.")
endif()

include(CMakeFindDependencyMacro)

if("" OR "")
  find_dependency(Threads)  # for Threads::Threads
endif()

if("")
  if("")
    find_dependency(OpenSSL "")
  else()
    find_dependency(OpenSSL)
  endif()
  # Define lib duplicate to fixup lib order for GCC binutils ld in static builds
  if(TARGET OpenSSL::Crypto AND NOT TARGET CURL::OpenSSL_Crypto)
    add_library(CURL::OpenSSL_Crypto INTERFACE IMPORTED)
    set_target_properties(CURL::OpenSSL_Crypto PROPERTIES INTERFACE_LINK_LIBRARIES OpenSSL::Crypto)
  endif()
endif()
if("ON")
  find_dependency(ZLIB "1")
  # Define lib duplicate to fixup lib order for GCC binutils ld in static builds
  if(TARGET ZLIB::ZLIB AND NOT TARGET CURL::ZLIB)
    add_library(CURL::ZLIB INTERFACE IMPORTED)
    set_target_properties(CURL::ZLIB PROPERTIES INTERFACE_LINK_LIBRARIES ZLIB::ZLIB)
  endif()
endif()

set(_curl_cmake_module_path_save ${CMAKE_MODULE_PATH})
list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})

set(_curl_libs "")

if("ON")
  find_dependency(Brotli MODULE)
  list(APPEND _curl_libs CURL::brotli)
endif()
if("")
  find_dependency(Cares MODULE)
  list(APPEND _curl_libs CURL::cares)
endif()
if("")
  find_dependency(GSS MODULE)
  list(APPEND _curl_libs CURL::gss)
endif()
if("")
  find_dependency(Libbacktrace MODULE)
  list(APPEND _curl_libs CURL::libbacktrace)
endif()
if("")
  find_dependency(Libgsasl MODULE)
  list(APPEND _curl_libs CURL::libgsasl)
endif()
if(NOT "ON" AND NOT "OFF")
  find_dependency(LDAP MODULE)
  list(APPEND _curl_libs CURL::ldap)
endif()
if("1")
  find_dependency(Libidn2 MODULE)
  list(APPEND _curl_libs CURL::libidn2)
endif()
if("ON")
  find_dependency(Libpsl MODULE)
  list(APPEND _curl_libs CURL::libpsl)
endif()
if("")
  find_dependency(Libssh MODULE)
  list(APPEND _curl_libs CURL::libssh)
endif()
if("ON")
  find_dependency(Libssh2 MODULE)
  list(APPEND _curl_libs CURL::libssh2)
endif()
if("")
  find_dependency(Libuv MODULE)
  list(APPEND _curl_libs CURL::libuv)
endif()
if("")
  find_dependency(MbedTLS MODULE)
  list(APPEND _curl_libs CURL::mbedtls)
endif()
if("OFF")
  find_dependency(NGHTTP2 MODULE)
  list(APPEND _curl_libs CURL::nghttp2)
endif()
if("")
  find_dependency(NGHTTP3 MODULE)
  list(APPEND _curl_libs CURL::nghttp3)
endif()
if("OFF")
  find_dependency(NGTCP2 MODULE)
  list(APPEND _curl_libs CURL::ngtcp2)
endif()
if("")
  find_dependency(GnuTLS MODULE)
  list(APPEND _curl_libs CURL::gnutls)
  find_dependency(Nettle MODULE)
  list(APPEND _curl_libs CURL::nettle)
endif()
if("OFF")
  find_dependency(Quiche MODULE)
  list(APPEND _curl_libs CURL::quiche)
endif()
if("")
  find_dependency(Rustls MODULE)
  list(APPEND _curl_libs CURL::rustls)
endif()
if("")
  find_dependency(WolfSSL MODULE)
  list(APPEND _curl_libs CURL::wolfssl)
endif()
if("ON")
  find_dependency(Zstd MODULE)
  list(APPEND _curl_libs CURL::zstd)
endif()

set(CMAKE_MODULE_PATH ${_curl_cmake_module_path_save})

# Define lib duplicate to fixup lib order for GCC binutils ld in static builds
if(WIN32 AND NOT TARGET CURL::win32_winsock)
  add_library(CURL::win32_winsock INTERFACE IMPORTED)
  set_target_properties(CURL::win32_winsock PROPERTIES INTERFACE_LINK_LIBRARIES "ws2_32")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/CURLTargets.cmake")

# Alias for either shared or static library
if(NOT TARGET CURL::libcurl)
  add_library(CURL::libcurl ALIAS CURL::libcurl_shared)
endif()

# For compatibility with CMake's FindCURL.cmake
set(CURL_VERSION_STRING "8.20.0")
set(CURL_LIBRARIES CURL::libcurl)
set(CURL_LIBRARIES_PRIVATE "ssh2;idn2;D:/M/msys64/ucrt64/lib/libz.dll.a;brotlidec;brotlicommon;zstd;wldap32;psl;bcrypt;advapi32;crypt32;secur32;ws2_32;iphlpapi;D:/M/msys64/ucrt64/lib/libz.dll.a;ws2_32")
set_and_check(CURL_INCLUDE_DIRS "${PACKAGE_PREFIX_DIR}/include")

set(CURL_SUPPORTED_PROTOCOLS "DICT;FILE;FTP;FTPS;GOPHER;GOPHERS;HTTP;HTTPS;IMAP;IMAPS;IPFS;IPNS;LDAP;LDAPS;MQTT;MQTTS;POP3;POP3S;RTSP;SCP;SFTP;SMTP;SMTPS;TELNET;TFTP;WS;WSS")
set(CURL_SUPPORTED_FEATURES "alt-svc;AsynchDNS;brotli;HSTS;HTTPS-proxy;IDN;IPv6;Kerberos;Largefile;libz;PSL;SPNEGO;SSL;SSPI;threadsafe;UnixSockets;zstd")

foreach(_curl_item IN LISTS CURL_SUPPORTED_PROTOCOLS CURL_SUPPORTED_FEATURES)
  set(CURL_SUPPORTS_${_curl_item} TRUE)
endforeach()

set(_curl_missing_req "")
foreach(_curl_item IN LISTS CURL_FIND_COMPONENTS)
  if(CURL_SUPPORTS_${_curl_item})
    set(CURL_${_curl_item}_FOUND TRUE)
  elseif(CURL_FIND_REQUIRED_${_curl_item})
    list(APPEND _curl_missing_req ${_curl_item})
  endif()
endforeach()

if(_curl_missing_req)
  string(REPLACE ";" " " _curl_missing_req "${_curl_missing_req}")
  if(CURL_FIND_REQUIRED)
    message(FATAL_ERROR "CURL: missing required components: ${_curl_missing_req}")
  endif()
  unset(_curl_missing_req)
endif()

check_required_components("CURL")
