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

ext except {
  ext error {
    ext invcall {
      ext syserror {
        class eaddrinuse (msg) {}
        class eaddrnotavail (msg) {}
        class eafnosupport (msg) {}
        class ealready (msg) {}
        class econnaborted (msg) {}
        class econnrefused (msg) {}
        class econnreset (msg) {}
        class edestaddrreq (msg) {}
        class ehostdown (msg) {}
        class ehostunreach (msg) {}
        class einprogress (msg) {}
        class eisconn (msg) {}
        class emsgsize (msg) {}
        class enetdown (msg) {}
        class enetreset (msg) {}
        class enetunreach (msg) {}
        class enobufs (msg) {}
        class enoprotoopt (msg) {}
        class enosr (msg) {}
        class enotconn (msg) {}
        class enotsock (msg) {}
        class eopnotsupp (msg) {}
        class epfnosupport (msg) {}
        class eprotonosupport (msg) {}
        class eprototype (msg) {}
        class eremoterelease (msg) {}
        class eshutdown (msg) {}
        class esocktnosupport (msg) {}
        class etimedout (msg) {}
        class etoomanyrefs (msg) {}
        class ewouldblock (msg) {}
        class eos_specific (msg) {}
      }
    }
  }
}

class ipc_err () {
  extern _eaddrinuse_no, _eaddrnotavail_no, _eafnosupport_no,
    _ealready_no, _econnaborted_no, _econnrefused_no, _econnreset_no,
    _edestaddrreq_no, _ehostdown_no, _ehostunreach_no, _einprogress_no,
    _eisconn_no, _emsgsize_no, _enetdown_no, _enetreset_no,
    _enetunreach_no, _enobufs_no, _enoprotoopt_no, _enosr_no,
    _enotconn_no, _enotsock_no, _eopnotsupp_no, _epfnosupport_no,
    _eprotonosupport_no, _eprototype_no, _eremoterelease_no,
    _eshutdown_no, _esocktnosupport_no, _etimedout_no, _etoomanyrefs_no,
    _ewouldblock_no, _eos_specific_nos;

  private _eaddrinuse_no, _eaddrnotavail_no, _eafnosupport_no,
    _ealready_no, _econnaborted_no, _econnrefused_no, _econnreset_no,
    _edestaddrreq_no, _ehostdown_no, _ehostunreach_no, _einprogress_no,
    _eisconn_no, _emsgsize_no, _enetdown_no, _enetreset_no,
    _enetunreach_no, _enobufs_no, _enoprotoopt_no, _enosr_no,
    _enotconn_no, _enotsock_no, _eopnotsupp_no, _epfnosupport_no,
    _eprotonosupport_no, _eprototype_no, _eremoterelease_no,
    _eshutdown_no, _esocktnosupport_no, _etimedout_no, _etoomanyrefs_no,
    _ewouldblock_no, _eos_specific_nos;

  extern _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
    _ealready_msg, _econnaborted_msg, _econnrefused_msg, _econnreset_msg,
    _edestaddrreq_msg, _ehostdown_msg, _ehostunreach_msg, _einprogress_msg,
    _eisconn_msg, _emsgsize_msg, _enetdown_msg, _enetreset_msg,
    _enetunreach_msg, _enobufs_msg, _enoprotoopt_msg, _enosr_msg,
    _enotconn_msg, _enotsock_msg, _eopnotsupp_msg, _epfnosupport_msg,
    _eprotonosupport_msg, _eprototype_msg, _eremoterelease_msg,
    _eshutdown_msg, _esocktnosupport_msg, _etimedout_msg, _etoomanyrefs_msg,
    _ewouldblock_msg, _eos_specific_msgs;

  private _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
    _ealready_msg, _econnaborted_msg, _econnrefused_msg, _econnreset_msg,
    _edestaddrreq_msg, _ehostdown_msg, _ehostunreach_msg, _einprogress_msg,
    _eisconn_msg, _emsgsize_msg, _enetdown_msg, _enetreset_msg,
    _enetunreach_msg, _enobufs_msg, _enoprotoopt_msg, _enosr_msg,
    _enotconn_msg, _enotsock_msg, _eopnotsupp_msg, _epfnosupport_msg,
    _eprotonosupport_msg, _eprototype_msg, _eremoterelease_msg,
    _eshutdown_msg, _esocktnosupport_msg, _etimedout_msg, _etoomanyrefs_msg,
    _ewouldblock_msg, _eos_specific_msgs;

  extern _ipc_err_init (); private _ipc_err_init;
  _ipc_err_init ();

  // table no -> ipc exception
  var n2e = {};

  if (_eaddrinuse_no > 0)
    n2e {_eaddrinuse_no} = syserrors.eaddrinuse (_eaddrinuse_msg);
  if (_eaddrnotavail_no > 0)
    n2e {_eaddrnotavail_no} = syserrors.eaddrnotavail (_eaddrnotavail_msg);
  if (_eafnosupport_no > 0)
    n2e {_eafnosupport_no} = syserrors.eafnosupport (_eafnosupport_msg);
  if (_ealready_no > 0)
    n2e {_ealready_no} = syserrors.ealready (_ealready_msg);
  if (_econnaborted_no > 0)
    n2e {_econnaborted_no} = syserrors.econnaborted (_econnaborted_msg);
  if (_econnrefused_no > 0)
    n2e {_econnrefused_no} = syserrors.econnrefused (_econnrefused_msg);
  if (_econnreset_no > 0)
    n2e {_econnreset_no} = syserrors.econnreset (_econnreset_msg);
  if (_edestaddrreq_no > 0)
    n2e {_edestaddrreq_no} = syserrors.edestaddrreq (_edestaddrreq_msg);
  if (_ehostdown_no > 0)
    n2e {_ehostdown_no} = syserrors.ehostdown (_ehostdown_msg);
  if (_ehostunreach_no > 0)
    n2e {_ehostunreach_no} = syserrors.ehostunreach (_ehostunreach_msg);
  if (_einprogress_no > 0)
    n2e {_einprogress_no} = syserrors.einprogress (_einprogress_msg);
  if (_eisconn_no > 0)
    n2e {_eisconn_no} = syserrors.eisconn (_eisconn_msg);
  if (_emsgsize_no > 0)
    n2e {_emsgsize_no} = syserrors.emsgsize (_emsgsize_msg);
  if (_enetdown_no > 0)
    n2e {_enetdown_no} = syserrors.enetdown (_enetdown_msg);
  if (_enetreset_no > 0)
    n2e {_enetreset_no} = syserrors.enetreset (_enetreset_msg);
  if (_enetunreach_no > 0)
    n2e {_enetunreach_no} = syserrors.enetunreach (_enetunreach_msg);
  if (_enobufs_no > 0)
    n2e {_enobufs_no} = syserrors.enobufs (_enobufs_msg);
  if (_enoprotoopt_no > 0)
    n2e {_enoprotoopt_no} = syserrors.enoprotoopt (_enoprotoopt_msg);
  if (_enosr_no > 0)
    n2e {_enosr_no} = syserrors.enosr (_enosr_msg);
  if (_enotconn_no > 0)
    n2e {_enotconn_no} = syserrors.enotconn (_enotconn_msg);
  if (_enotsock_no > 0)
    n2e {_enotsock_no} = syserrors.enotsock (_enotsock_msg);
  if (_eopnotsupp_no > 0)
    n2e {_eopnotsupp_no} = syserrors.eopnotsupp (_eopnotsupp_msg);
  if (_epfnosupport_no > 0)
    n2e {_epfnosupport_no} = syserrors.epfnosupport (_epfnosupport_msg);
  if (_eprotonosupport_no > 0)
    n2e {_eprotonosupport_no}
      = syserrors.eprotonosupport (_eprotonosupport_msg);
  if (_eprototype_no > 0)
    n2e {_eprototype_no} = syserrors.eprototype (_eprototype_msg);
  if (_eremoterelease_no > 0)
    n2e {_eremoterelease_no} = syserrors.eremoterelease (_eremoterelease_msg);
  if (_eshutdown_no > 0)
    n2e {_eshutdown_no} = syserrors.eshutdown (_eshutdown_msg);
  if (_esocktnosupport_no > 0)
    n2e {_esocktnosupport_no}
      = syserrors.esocktnosupport (_esocktnosupport_msg);
  if (_etimedout_no > 0)
    n2e {_etimedout_no} = syserrors.etimedout (_etimedout_msg);
  if (_etoomanyrefs_no > 0)
    n2e {_etoomanyrefs_no} = syserrors.etoomanyrefs (_etoomanyrefs_msg);
  if (_ewouldblock_no > 0)
    n2e {_ewouldblock_no} = syserrors.ewouldblock (_ewouldblock_msg);

  var i;
  if (_eos_specific_nos != nil)
    for (i = 0; i < #_eos_specific_nos; i++)
      n2e {_eos_specific_nos [i]}
	 = syserrors.eos_specific (_eos_specific_msgs [i]);
}

var ipc_errs = ipc_err ();
