[= AutoGen5 Template  -*- Mode: text -*-

h=%s-fsm.h  c=%s-fsm.c
=]
[= (define cright-years

   "Copyright (C) 1992-2018 Bruce Korb - all rights reserved"

   ) =][= #
/*
## This file is part of AutoGen.
##
## AutoGen is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## AutoGen is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## NB:  THIS FILE IS GPL.  THE OUTPUT OF THIS FILE IS LICENSED MBSD.

DESCRIPTION:
Produce Finite State Machine Code
The machine is derived from a list of states and transitions.

PURPOSE:
   This template collection will produce a finite state machine based on a
   description of such a machine.  The presumption is that you generally
   do not need to know what the current state is in order to determine what
   kind of transition is to be taken.  That is to say, there is a global
   transition table that is indexed by the current state and the next
   transition type to determine the next state and trigger any optional
   transition handling code.

   The finite state machine may be either the master, driving the other
   parts of the program, or act as a subroutine keeping track of state
   between calls.  Consequently, the "type" attribute may be set to:

   looping
       If the machine processes transitions until it reaches a terminal
       state (error or done).
   stepping
       If the FSM code will process a single transition and then return.
   reentrant
       This method is much the same as stepping, except that the caller
       must save the current state and provide it on the next call.
       In this fashion, an FSM may be used in a multi threaded application.

   The machine can be constructed in either of three formats, depending
   on the value of the "method" attribute:

   callout
       This method will use a callout table instead of a switch statement
       to implement the machine.
   case
       This is the alternate implementation method.
   none
       Do not supply the "method" attribute.  Choosing this will cause only
       the dispatch table to be created.  The implementation code is omitted.
       The "type" attribute is then ignored.

YOU SUPPLY a "method" and a "type" as above, plus:

  state          The list of valid state names.  The "init" and "done" states
                 are automatically added to this.  If there are other terminal
                 states, they must set "nxtSt" to "done".
  event          The list of valid transition types.
  prefix         A prefix to glue onto the front of created names
  cookie         zero, one or more of these each containing a C type and name
                 suitable for use in a procedure header.  It is used to pass
                 through arguments to implementation code.

  transition     Define the handling for a transition to a new state.
                 It contains:
     tst         the starting state(s).  This may be one, or a list or '*'
                 to indicate all states.
     tev         the event that triggers this transition.  This may also be
                 a list of events or a '*'.
     ttype       the transition type.  By default it is named after the state
                 and event names, but by specifying a particular type, multiple
                 different transitions may invoke the same handling code.
     next        the presumptive destination state.  "presumptive" because
                 the code that handles the transition may select a different
                 destination.  Doing that will violate mathematical models, but
                 it often makes writing this stuff easier.

  debug_flag     names the CPP flag to indicate a debug compile
  exit_proc      function for exiting program ("exit(3)" is default)
  extra_header   headers to #include in the .h file

  handler_file   file that contains all the handler code.  In case you wish
                 to keep all hand crafted code out of the generated file.
                 In that case, specify this file and it will be #include-d
                 in five sections:

                 #if   defined FSM_USER_HEADERS
                 #elif defined FSM_HANDLER_CODE
                 #elif defined FSM_SWITCH_CODE
                 #elif defined FSM_FIND_TRANSITION
                 #elif defined FSM_FINISH_STEP
                 #else
                 #  error "fsm include type not specified"
                    choke-me-now.
                 #endif

  TODO:  allow longer user names for states and events.
 */ =][=

CASE (suffix) =][=

== h =][=

  (define fmt     "")
  (define fsm-ver "1.0")   =][=
  INCLUDE "fsm-trans.tlib" =][=
  INCLUDE "fsm-macro.tlib" =][=

  INVOKE  preamble

=]
/*
 *  This file enumerates the states and transition events for a FSM.
 *
 *  te_[=(. pfx)=]_state
 *      The available states.  FSS_INIT is always defined to be zero
 *      and FSS_INVALID and FSS_DONE are always made the last entries.
 *
 *  te_[=(. pfx)=]_event
 *      The transition events.  These enumerate the event values used
 *      to select the next state from the current state.
 *      [=(. PFX)=]_EV_INVALID is always defined at the end.
 */
[=(make-header-guard "autofsm")=][=

FOR extra-header =]
#include "[=extra-header=]"[=
ENDFOR

=]
/**
 *  Finite State machine States
 *
 *  Count of non-terminal states.  The generated states INVALID and DONE
 *  are terminal, but INIT is not  :-).
 */
#define [=(. PFX)=]_STATE_CT  [=(+ 1 (count "state"))=]
typedef enum {
[=
 (shell (string-append
 "${CLexe-columns} --spread=1 -I4 -S, -f'" PFX "_ST_%s' <<_EOF_
INIT
" (stack-up "state") "
INVALID
DONE
_EOF_" )) =]
} te_[=(. pfx)=]_state;

/**
 *  Finite State machine transition Events.
 *
 *  Count of the valid transition events
 */
#define [=(. PFX)=]_EVENT_CT [=(count "event")=]
typedef enum {
[= compute-transitions =][=
  (shellf "${CLexe-columns} --spread=1 -I4 -S, -f'%s_EV_%%s' <<_EOF_
%s
INVALID
_EOF_" PFX (stack-up "event") )=]
} te_[=(. pfx)=]_event;
[=

  CASE method     =][=

  ~*  call|case   =][=

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #
    #   We are implementing the machine.  Declare the external  =][=

    CASE type     =][=

    ~* step|reent =][= make-step-proc mode = "extern " =];[=

    =* loop       =][= make-loop-proc mode = "extern " =];[=

    *             =][=
    (error (string-append "invalid FSM type:  ``" (get "type")
           "'' must be ``looping'', ``stepping'' or ``reentrant''" ))
    =][=
    ESAC          =][=

    #  End external procedure declarations
    #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #
  #   We are *NOT* implementing the machine.  Define the table  =][=

  ==  ""       =][=
    INVOKE enumerate-transitions  use_ifdef = yes  =][=
  =*  no       =][=
    INVOKE enumerate-transitions  use_ifdef = yes  =][=
  *            =][=
    (error (sprintf
        "invalid FSM method:  ``%s'' must be ``callout'', ``case'' or ``none''"
        (get "method"))) =][=
  ESAC         =]

#endif /* [=(. header-guard)=] */[=

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#   C OUTPUT BEGINS HERE
#
=][=

== c =][=

  (if (~ (get "method") "(no.*){0,1}")
      (out-delete)  ) =][=

  INVOKE preamble

=]
#define DEFINE_FSM
#include "[=(. header-file)=]"
#include <stdio.h>
[=IF (exist? "handler-file")=]
#define FSM_USER_HEADERS
#include "[= handler-file =]"
#undef  FSM_USER_HEADERS[=
  ELSE =]
/*
 *  Do not make changes to this file, except between the START/END
 *  comments, or it will be removed the next time it is generated.
 */
[=(extract fsm-source "/* %s === USER HEADERS === %s */")=][=
  ENDIF =]

#ifndef NULL
#  define NULL 0
#endif
[= CASE method          =][=
   =* "case"            =][= enumerate-transitions =][=
   =* "call"            =][= callback-transitions  =][=
   ESAC                 =]
[=IF (=* (get "type") "step")=]
/**
 *  The FSM machine state
 */
static te_[=(. pfx)=]_state [=(. pfx)=]_state = [=(. PFX)=]_ST_INIT;
[=ENDIF=]
[= emit-invalid-msg     =][=

  IF  (=* (get "method") "call") =][=

    IF (exist? "handler-file")   =]
#define FSM_HANDLER_CODE
#include "[= handler-file =]"
#undef  FSM_HANDLER_CODE
[=
    ELSE                =][=
      INVOKE callbacks  =][=
    ENDIF               =][=

  ELSE                  =][=
  ENDIF                 =][=

  CASE type             =][=
  =*   loop             =][= looping-machine  =][=
  ~*   step|reent       =][= stepping-machine =][=
  ESAC                  =][=

ESAC (suffix)

=]
/*
 * Local Variables:
 * mode: C
 * c-file-style: "stroustrup"
 * indent-tabs-mode: nil
 * End:
 * end of [= (out-name) ;; agen5/fsm.tpl =] */
