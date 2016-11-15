/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of interpreter of DINO.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

include "ipcerr";

ext except {
  class socket_except () {
    class optype (msg) {}
    class opvalue (msg) {}
    class eof (msg) {}
  }
  ext error {
    class socket_error () {
      class invalid_address (msg) {}
      class host_not_found (msg) {}
      class no_address (msg) {}
      class no_recovery (msg) {}
    }
  }
}

var socket_excepts = excepts.socket_except ();
var socket_errors = errors.socket_error ();

final class __socket_package () {
  extern _socket_errno, _socket_invalid_address, _socket_host_not_found,
    _socket_no_address, _socket_no_recovery, _socket_try_again, _socket_eof,
    _gethostinfo (), _getservbyport (), _getservbyname (),
    _socket_init (), _socket_fin ();
  private _socket_errno, _socket_invalid_address, _socket_host_not_found,
    _socket_no_address, _socket_no_recovery, _socket_try_again, _socket_eof,
    _gethostinfo, _getservbyport, _getservbyname, _socket_init, _socket_fin,
    host_info, serv_info;

  func generate_socket_exception () {
    if (_socket_errno <= 0) throw socket_excepts.eof ();
    else if (_socket_errno == _socket_eof) throw socket_excepts.eof ();
    else if (_socket_errno in ipc_errs.n2e) throw ipc_errs.n2e {_socket_errno};
    else if (_socket_errno == _socket_invalid_address)
      throw socket_errors.invalid_address ();
    else if (_socket_errno == _socket_host_not_found)
      throw socket_errors.host_not_found ("host is unknown");
    else if (_socket_errno == _socket_no_address)
      throw socket_errors.no_address ("does not have an IP address");
    else if (_socket_errno == _socket_no_recovery)
      throw socket_errors.no_recovery ("non-recoverable name server error");
    else __process_errno__ ("generate_socket_exception");
  }

  // If you change it, change code of _gethostinfo too.
  class host_info (final name, final aliases, final ipaddrs) {}

  func gethostinfo (str) {
    if (str == nil) str = "";
    else if (type (str) != vector || eltype (str) != char)
      throw socket_excepts.optype ();
    var h = host_info  ();
    h = _gethostinfo (str, h);
    if (h == nil)
      generate_socket_exception ();
    return h;
  }

  // If you change it, change code of _getservbyname, _getservbyport too.
  class serv_info (final name, final aliases, final port, final proto) {}

  func getservbyport (port, proto) {
    var s;

    if (type (proto) != vector || eltype (proto) != char || type (port) != int)
      throw socket_excepts.optype ();
    s = serv_info  (nil, nil, port, proto);
    s = _getservbyport (s);
    if (s == nil && _socket_errno != 0)
      generate_socket_exception ();
    return s;
  }

  func getservbyname (name, proto) {
    var s;

    if (type (proto) != vector || eltype (proto) != char
	|| type (name) != vector || eltype (name) != char)
      throw socket_excepts.optype ();
    s = serv_info  (name, nil, nil, proto);
    s = _getservbyname (s);
    if (s == nil && _socket_errno != 0)
      generate_socket_exception ();
    return s;
  }

  private generate_socket_exception;

  extern _sread (), _swrite (), _recvfrom (), _sendto (), _accept (),
    _stream_client (), _dgram_client (), _stream_server (), _dgram_server (),
    _close_socket ();
  private _sread, _swrite, _recvfrom, _sendto, _accept,
    _stream_client, _dgram_client, _stream_server, _dgram_server,
    _close_socket;

  // If you change it, change code of _recvfrom too.
  class datagram (str, peer_addr, port) {}
  private datagram;

  var proxy_sfd; private proxy_sfd;

  class stream_client (peer_addr, port) {
    var sfd; private sfd;

    func read (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
      else if (len < 0)
        throw socket_excepts.opvalue ();
      var str = _sread (sfd, len);
      if (str == nil)
        generate_socket_exception ();
      return str;
    }
    func write (str) {
      var nb = _swrite (sfd, str);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    private destroy;
    func destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (peer_addr) != vector || eltype (peer_addr) != char
        || type (port) != int)
      throw socket_excepts.optype ();
    sfd = (proxy_sfd == nil ? _stream_client (peer_addr, port) : proxy_sfd);
    proxy_sfd = nil;
    if (sfd == nil)
      generate_socket_exception ();
  }

  class dgram_client () {
    var sfd; private sfd;

    func recvfrom (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
      else if (len < 0)
        throw socket_excepts.opvalue ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception ();
      return dg;
    }
    func sendto (str, peer_addr, port) {
      if (type (str) != vector || eltype (str) != char
	  || type (peer_addr) != vector || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_excepts.optype ();
      else if (port < 0)
        throw socket_excepts.opvalue ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    private destroy;
    func destroy () {if (sfd != nil) _close_socket (sfd);}

    sfd = _dgram_client ();
    if (sfd == nil)
      generate_socket_exception ();
  }

  class stream_server (port, queue_len) { // bind
    var sfd; private sfd;

    func accept () {
      var v = _accept (sfd);
      if (v == nil)
        generate_socket_exception ();
      proxy_sfd = v [0];      
      return stream_client (v [1], v [2]);
    }

    private destroy;
    func destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (port) != int)
      throw socket_excepts.optype ();
    if (type (queue_len) != int)
      throw socket_excepts.optype ();
    sfd = _stream_server (port, queue_len);
    if (sfd == nil)
      generate_socket_exception ();
  }

  class dgram_server (port) {
    var sfd; private sfd;

    func recvfrom (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
    	else if (len < 0)
        throw socket_excepts.opvalue ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception ();
      return dg;
    }
    func sendto (str, peer_addr, port) {
      if (type (str) != vector || eltype (str) != char
	  || type (peer_addr) != vector || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_excepts.optype ();
      else if (port < 0)
        throw socket_excepts.opvalue ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    private destroy;
    func destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (port) != int)
      throw socket_excepts.optype ();
    sfd = _dgram_server (port);
    if (sfd == nil)
      generate_socket_exception ();
  }

  private destroy;
  func destroy () {_socket_fin ();}

  _socket_init ();
}

var sockets = __socket_package ();
