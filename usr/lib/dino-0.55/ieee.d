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

include "mpi";

ext except {
  class ieee_except () {
    class optype (msg) {}
    class opvalue (msg) {}
    class round_value (msg) {}
    class invalid_operation () {}
    class reserved_operand () {}
    class overflow () {}
    class underflow () {}
    class imprecise_result () {}
    class zero_division () {}
  }
}

// Only one instance of the class should be. Do not call the class.
// Use only instance `ieees' (see below).
final class __ieee_package () {
  var ieee_excepts = excepts.ieee_except ();
  var ignore_excepts = 0;

  private check_mpi, process_except;
  func check_mpi (op) {
    if (type (op) != class () || !inside (op, mpi_package))
      throw ieee_excepts.optype ();
  }

  // The following can be used to form trap mask.
  var final inv = 1; // invalid operation
  var final ro = 2;  // reserved operand
  var final ofl = 4; // overflow
  var final ufl = 8; // underflow
  var final imp = 16;// imprecise result
  var final dz = 32; // divide by zero

  func process_except () {
    var status_bits = get_status_bits ();
    if (ignore_excepts)
      return;
    if (status_bits & inv)
      throw ieee_excepts.invalid_operation ();
    else if (status_bits & ro)
      throw ieee_excepts.reserved_operand ();
    else if (status_bits & ofl)
      throw ieee_excepts.overflow ();
    else if (status_bits & ufl)
      throw ieee_excepts.underflow ();
    else if (status_bits & imp)
      throw ieee_excepts.imprecise_result ();
    else if (status_bits & dz)
      throw ieee_excepts.zero_division ();
  }
  
  // The following are possible values of `round'.
  var final rn = 0; // round to nearest
  var final rni = 1;// round to positive inifinity
  var final rpi = 2;// round to negative inifinity
  var final rz = 3; // round to zero

  public rn, rpi, rni, rz;

  // Functions from ieee.c:
  extern ieee_set_trap_mask (), ieee_get_trap_mask (),
    ieee_set_sticky_status_bits (), ieee_get_sticky_status_bits (),
    ieee_get_status_bits (), ieee_set_round (), ieee_get_round (),
    ieee_reset ();
  private ieee_set_trap_mask, ieee_get_trap_mask, ieee_set_sticky_status_bits,
    ieee_get_sticky_status_bits, ieee_get_status_bits, ieee_set_round,
    ieee_get_round, ieee_reset;

  func set_trap_mask (mask) {
    // 1 means that there will be no corresponding exception fixed.
    if (type (mask) != int)
      throw ieee_excepts.optype ();
    ieee_set_trap_mask (mask);
  }

  func get_trap_mask () {return ieee_get_trap_mask ();}

  func set_sticky_status_bits (mask) {
    if (type (mask) != int)
      throw ieee_excepts.optype ();
    ieee_set_sticky_status_bits (mask);
  }

  // only bits corresponding to masked exceptions fixed are set up.
  func get_sticky_status_bits () {return ieee_get_sticky_status_bits ();}

  // Return exceptions fixed independently from trap mask
  func get_status_bits () {return ieee_get_status_bits ();}

  func set_round (r) {
    if (type (r) != int)
      throw ieee_excepts.optype ();
    if (r < 0 || r > 3)
      throw ieee_excepts.round_value();
    ieee_set_round (r);
  }

  func get_round () {return ieee_get_round ();}
  
  // Reset package state.
  func reset () {ieee_reset ();}

  // Single precision floating point numbers (32 bits):
  final class single (str) {
    private value; var value;
    
    friend double, quad;

    private check;
    func check (op) {
      if (type (op) != class () || !inside (op, ieees.single))
        throw ieee_excepts.optype ();
    }

    extern ieee_single_positive_zero (), ieee_single_negative_zero (),
      ieee_single_nan (), ieee_single_trapping_nan (),
      ieee_single_positive_infinity (), ieee_single_negative_infinity (),
      ieee_single_positive_maximum (), ieee_single_negative_maximum (),
      ieee_single_positive_minimum (), ieee_single_negative_minimum (),
      ieee_is_single_positive_zero (), ieee_is_single_negative_zero (),
      ieee_is_single_nan (),  ieee_is_single_trapping_nan (),
      ieee_is_single_positive_infinity (), ieee_is_single_negative_infinity (),
      ieee_is_single_normalized (), ieee_is_single_denormalized (),
      ieee_add_single (), ieee_subtract_single (),
      ieee_multiply_single (), ieee_divide_single (),
      ieee_eq_single (), ieee_ne_single (), ieee_lt_single (),
      ieee_gt_single (), ieee_le_single (), ieee_ge_single (),
      ieee_single_to_double (), ieee_single_to_quad (),
      ieee_single_from_integer (), ieee_integer_from_single (),
      ieee_single_to_binary_string (), ieee_single_to_string (),
      ieee_single_from_binary_string (), ieee_single_from_string (),
      ieee_single_from_float ();

    private ieee_single_positive_zero, ieee_single_negative_zero,
      ieee_single_nan, ieee_single_trapping_nan, ieee_single_positive_infinity,
      ieee_single_negative_infinity, ieee_single_positive_maximum,
      ieee_single_negative_maximum, ieee_single_positive_minimum,
      ieee_single_negative_minimum, ieee_is_single_positive_zero,
      ieee_is_single_negative_zero, ieee_is_single_nan,
      ieee_is_single_trapping_nan, ieee_is_single_positive_infinity,
      ieee_is_single_negative_infinity, ieee_is_single_normalized,
      ieee_is_single_denormalized, ieee_add_single, ieee_subtract_single,
      ieee_multiply_single, ieee_divide_single, ieee_eq_single,
      ieee_ne_single, ieee_lt_single, ieee_gt_single, ieee_le_single,
      ieee_ge_single, ieee_single_to_double, ieee_single_to_quad,
      ieee_single_from_integer, ieee_integer_from_single,
      ieee_single_to_binary_string, ieee_single_to_string,
      ieee_single_from_binary_string, ieee_single_from_string,
      ieee_single_from_float;

    func pzero () {value = ieee_single_positive_zero ();}
    func nzero () {value = ieee_single_negative_zero ();}
    func nan () {value = ieee_single_nan ();}
    func trap_nan () {value = ieee_single_trapping_nan ();}
    func pinfinity () {value = ieee_single_positive_infinity ();}
    func ninfinity () {value = ieee_single_negative_infinity ();}
    func pmax () {value = ieee_single_positive_maximum ();}
    func nmax () {value = ieee_single_negative_maximum ();}
    func pmin () {value = ieee_single_positive_minimum ();}
    func nmin () {value = ieee_single_negative_minimum ();}

    func is_pzero () {return ieee_is_single_positive_zero (value);}
    func is_nzero () {return ieee_is_single_negative_zero (value);}
    func is_nan () {return ieee_is_single_nan (value);}
    func is_trap_nan () {return ieee_is_single_trapping_nan (value);}
    func is_pinfinity () {return ieee_is_single_positive_infinity (value);}
    func is_ninfinity () {return ieee_is_single_negative_infinity (value);}

    func is_normalized () {return ieee_is_single_normalized (value);}
    func is_denormalized () {return ieee_is_single_denormalized (value);}

    func add (op) {// May generate exception
      check (op);
      var result = single ();
      result.value = ieee_add_single (value, op.value);
      process_except ();
      return result;
    }
    func subtract (op) {// May generate exception
      check (op);
      var result = single ();
      result.value = ieee_subtract_single (value, op.value);
      process_except ();
      return result;
    }
    func multiply (op) {// May generate exception
      check (op);
      var result = single ();
      result.value = ieee_multiply_single (value, op.value);
      process_except ();
      return result;
    }
    func divide (op) {// May generate exception
      check (op);
      var result = single ();
      result.value = ieee_divide_single (value, op.value);
      process_except ();
      return result;
    }
    func eq (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_eq_single (value, op.value);
    }
    func ne (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ne_single (value, op.value);
    }
    func lt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_lt_single (value, op.value);
    }
    func gt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_gt_single (value, op.value);
    }
    func le (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_le_single (value, op.value);
    }
    func ge (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ge_single (value, op.value);
    }
    func to_double () {// May generate exception
      var result = double ();
      result.value = ieee_single_to_double (value);
      process_except ();
      return result;
    }
    func to_quad () {// May generate exception
      var result = quad ();
      result.value = ieee_single_to_quad (value);
      process_except ();
      return result;
    }
    func from_mpi (op) {// May generate exception
      check_mpi (op);
      value = ieee_single_from_integer (op);
      process_except ();
    }
    func to_mpi (size) {// May generate exception
      var result = mpis.from_string (size, "0");
      ieee_integer_from_single (value, result);
      process_except ();
      return result;
    }
    func to_binary_string (base) {// May generate exception
      if (type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      return ieee_single_to_binary_string (value, base);
    }
    func to_string () {return ieee_single_to_string (value);}
    func from_binary_string (str, base) {// May generate exception
      if (type (str) != vector || eltype (str) != char || type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      value = ieee_single_from_binary_string (str, base);
      process_except ();
    }
    func from_string (str) {// May generate exception
      if (type (str) != vector || eltype (str) != char)
	throw ieee_excepts.optype ();
      value = ieee_single_from_string (str);
      process_except ();
    }
    func from_float (f) {
      if (type (f) != float)
	throw ieee_excepts.optype ();
      value = ieee_single_from_float (f);
      process_except ();
    }
    if (str != nil)
      from_string (str);
    else
      value = ieee_single_positive_zero ();
  }

  // Double precision floating point numbers (64 bits):
  final class double (str) {
    private value; var value;
    
    friend single, quad;

    private check;
    func check (op) {
      if (type (op) != class () || !inside (op, ieees.double))
        throw ieee_excepts.optype ();
    }

    extern ieee_double_positive_zero (), ieee_double_negative_zero (),
      ieee_double_nan (), ieee_double_trapping_nan (),
      ieee_double_positive_infinity (), ieee_double_negative_infinity (),
      ieee_double_positive_maximum (), ieee_double_negative_maximum (),
      ieee_double_positive_minimum (), ieee_double_negative_minimum (),
      ieee_is_double_positive_zero (), ieee_is_double_negative_zero (),
      ieee_is_double_nan (), ieee_is_double_trapping_nan (),
      ieee_is_double_positive_infinity (), ieee_is_double_negative_infinity (),
      ieee_is_double_normalized (), ieee_is_double_denormalized (),
      ieee_add_double (), ieee_subtract_double (),
      ieee_multiply_double (), ieee_divide_double (),
      ieee_eq_double (), ieee_ne_double (), ieee_lt_double (),
      ieee_gt_double (), ieee_le_double (), ieee_ge_double (),
      ieee_double_to_single (), ieee_double_to_quad (),
      ieee_double_from_integer (), ieee_integer_from_double (),
      ieee_double_to_binary_string (), ieee_double_to_string (),
      ieee_double_from_binary_string (), ieee_double_from_string (),
      ieee_double_from_float ();

    private ieee_double_positive_zero, ieee_double_negative_zero,
      ieee_double_nan, ieee_double_trapping_nan, ieee_double_positive_infinity,
      ieee_double_negative_infinity, ieee_double_positive_maximum,
      ieee_double_negative_maximum, ieee_double_positive_minimum,
      ieee_double_negative_minimum, ieee_is_double_positive_zero,
      ieee_is_double_negative_zero, ieee_is_double_nan,
      ieee_is_double_trapping_nan, ieee_is_double_positive_infinity,
      ieee_is_double_negative_infinity, ieee_is_double_normalized,
      ieee_is_double_denormalized, ieee_add_double, ieee_subtract_double,
      ieee_multiply_double, ieee_divide_double, ieee_eq_double,
      ieee_ne_double, ieee_lt_double, ieee_gt_double, ieee_le_double,
      ieee_ge_double, ieee_double_to_single, ieee_double_to_quad,
      ieee_double_from_integer, ieee_integer_from_double,
      ieee_double_to_binary_string, ieee_double_to_string,
      ieee_double_from_binary_string, ieee_double_from_string,
      ieee_double_from_float;

    func pzero () {value = ieee_double_positive_zero ();}
    func nzero () {value = ieee_double_negative_zero ();}
    func nan () {value = ieee_double_nan ();}
    func trap_nan () {value = ieee_double_trapping_nan ();}
    func pinfinity () {value = ieee_double_positive_infinity ();}
    func ninfinity () {value = ieee_double_negative_infinity ();}
    func pmax () {value = ieee_double_positive_maximum ();}
    func nmax () {value = ieee_double_negative_maximum ();}
    func pmin () {value = ieee_double_positive_minimum ();}
    func nmin () {value = ieee_double_negative_minimum ();}

    func is_pzero () {return ieee_is_double_positive_zero (value);}
    func is_nzero () {return ieee_is_double_negative_zero (value);}
    func is_nan () {return ieee_is_double_nan (value);}
    func is_trap_nan () {return ieee_is_double_trapping_nan (value);}
    func is_pinfinity () {return ieee_is_double_positive_infinity (value);}
    func is_ninfinity () {return ieee_is_double_negative_infinity (value);}

    func is_normalized () {return ieee_is_double_normalized (value);}
    func is_denormalized () {return ieee_is_double_denormalized (value);}

    func add (op) {// May generate exception
      check (op);
      var result = double ();
      result.value = ieee_add_double (value, op.value);
      process_except ();
      return result;
    }
    func subtract (op) {// May generate exception
      check (op);
      var result = double ();
      result.value = ieee_subtract_double (value, op.value);
      process_except ();
      return result;
    }
    func multiply (op) {// May generate exception
      check (op);
      var result = double ();
      result.value = ieee_multiply_double (value, op.value);
      process_except ();
      return result;
    }
    func divide (op) {// May generate exception
      check (op);
      var result = double ();
      result.value = ieee_divide_double (value, op.value);
      process_except ();
      return result;
    }
    func eq (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_eq_double (value, op.value);
    }
    func ne (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ne_double (value, op.value);
    }
    func lt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_lt_double (value, op.value);
    }
    func gt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_gt_double (value, op.value);
    }
    func le (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_le_double (value, op.value);
    }
    func ge (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ge_double (value, op.value);
    }
    func to_single () {// May generate exception
      var result = single ();
      result.value = ieee_double_to_single (value);
      process_except ();
      return result;
    }
    func to_quad () {// May generate exception
      var result = quad ();
      result.value = ieee_double_to_quad (value);
      process_except ();
      return result;
    }
    func from_mpi (op) {// May generate exception
      check_mpi (op);
      value = ieee_double_from_integer (op);
      process_except ();
    }
    func to_mpi (size) {// May generate exception
      var result = mpis.from_string (size, "0");
      ieee_integer_from_double (value, result);
      process_except ();
      return result;
    }
    func to_binary_string (base) {// May generate exception
      if (type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      return ieee_double_to_binary_string (value, base);
    }
    func to_string () {return ieee_double_to_string (value);}
    func from_binary_string (str, base) {// May generate exception
      if (type (str) != vector || eltype (str) != char || type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      value = ieee_double_from_binary_string (str, base);
      process_except ();
    }
    func from_string (str) {// May generate exception
      if (type (str) != vector || eltype (str) != char)
	throw ieee_excepts.optype ();
      value = ieee_double_from_string (str);
      process_except ();
    }
    func from_float (f) {
      if (type (f) != float)
	throw ieee_excepts.optype ();
      value = ieee_double_from_float (f);
      process_except ();
    }
    if (str != nil)
      from_string (str);
    else
      value = ieee_double_positive_zero ();
  }

  // Quad precision floating point numbers (128 bits):
  final class quad (str) {
    private value; var value;
    
    friend single, double;

    private check;
    func check (op) {
      if (type (op) != class () || !inside (op, ieees.quad))
        throw ieee_excepts.optype ();
    }

    extern ieee_quad_positive_zero (), ieee_quad_negative_zero (),
      ieee_quad_nan (), ieee_quad_trapping_nan (),
      ieee_quad_positive_infinity (), ieee_quad_negative_infinity (),
      ieee_quad_positive_maximum (), ieee_quad_negative_maximum (),
      ieee_quad_positive_minimum (), ieee_quad_negative_minimum (),
      ieee_is_quad_positive_zero (), ieee_is_quad_negative_zero (),
      ieee_is_quad_nan (), ieee_is_quad_trapping_nan (),
      ieee_is_quad_positive_infinity (), ieee_is_quad_negative_infinity (),
      ieee_is_quad_normalized (), ieee_is_quad_denormalized (),
      ieee_add_quad (), ieee_subtract_quad (),
      ieee_multiply_quad (), ieee_divide_quad (),
      ieee_eq_quad (), ieee_ne_quad (), ieee_lt_quad (),
      ieee_gt_quad (), ieee_le_quad (), ieee_ge_quad (),
      ieee_quad_to_single (), ieee_quad_to_double (),
      ieee_quad_from_integer (), ieee_integer_from_quad (),
      ieee_quad_to_binary_string (), ieee_quad_to_string (),
      ieee_quad_from_binary_string (), ieee_quad_from_string (),
      ieee_quad_from_float ();

    private ieee_quad_positive_zero, ieee_quad_negative_zero,
      ieee_quad_nan, ieee_quad_trapping_nan, ieee_quad_positive_infinity,
      ieee_quad_negative_infinity, ieee_quad_positive_maximum,
      ieee_quad_negative_maximum, ieee_quad_positive_minimum,
      ieee_quad_negative_minimum, ieee_is_quad_positive_zero,
      ieee_is_quad_negative_zero, ieee_is_quad_nan,
      ieee_is_quad_trapping_nan, ieee_is_quad_positive_infinity,
      ieee_is_quad_negative_infinity, ieee_is_quad_normalized,
      ieee_is_quad_denormalized, ieee_add_quad, ieee_subtract_quad,
      ieee_multiply_quad, ieee_divide_quad, ieee_eq_quad, ieee_ne_quad,
      ieee_lt_quad, ieee_gt_quad, ieee_le_quad, ieee_ge_quad,
      ieee_quad_to_single, ieee_quad_to_double, ieee_quad_from_integer,
      ieee_integer_from_quad, ieee_quad_to_binary_string, ieee_quad_to_string,
      ieee_quad_from_binary_string, ieee_quad_from_string,
      ieee_quad_from_float;

    func pzero () {value = ieee_quad_positive_zero ();}
    func nzero () {value = ieee_quad_negative_zero ();}
    func nan () {value = ieee_quad_nan ();}
    func trap_nan () {value = ieee_quad_trapping_nan ();}
    func pinfinity () {value = ieee_quad_positive_infinity ();}
    func ninfinity () {value = ieee_quad_negative_infinity ();}
    func pmax () {value = ieee_quad_positive_maximum ();}
    func nmax () {value = ieee_quad_negative_maximum ();}
    func pmin () {value = ieee_quad_positive_minimum ();}
    func nmin () {value = ieee_quad_negative_minimum ();}

    func is_pzero () {return ieee_is_quad_positive_zero (value);}
    func is_nzero () {return ieee_is_quad_negative_zero (value);}
    func is_nan () {return ieee_is_quad_nan (value);}
    func is_trap_nan () {return ieee_is_quad_trapping_nan (value);}
    func is_pinfinity () {return ieee_is_quad_positive_infinity (value);}
    func is_ninfinity () {return ieee_is_quad_negative_infinity (value);}

    func is_normalized () {return ieee_is_quad_normalized (value);}
    func is_denormalized () {return ieee_is_quad_denormalized (value);}

    func add (op) {// May generate exception
      check (op);
      var result = quad ();
      result.value = ieee_add_quad (value, op.value);
      process_except ();
      return result;
    }
    func subtract (op) {// May generate exception
      check (op);
      var result = quad ();
      result.value = ieee_subtract_quad (value, op.value);
      process_except ();
      return result;
    }
    func multiply (op) {// May generate exception
      check (op);
      var result = quad ();
      result.value = ieee_multiply_quad (value, op.value);
      process_except ();
      return result;
    }
    func divide (op) {// May generate exception
      check (op);
      var result = quad ();
      result.value = ieee_divide_quad (value, op.value);
      process_except ();
      return result;
    }
    func eq (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_eq_quad (value, op.value);
    }
    func ne (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ne_quad (value, op.value);
    }
    func lt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_lt_quad (value, op.value);
    }
    func gt (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_gt_quad (value, op.value);
    }
    func le (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_le_quad (value, op.value);
    }
    func ge (op) {// May generate exception
      check (op);
      process_except ();
      return ieee_ge_quad (value, op.value);
    }
    func to_single () {// May generate exception
      var result = single ();
      result.value = ieee_quad_to_single (value);
      process_except ();
      return result;
    }
    func to_double () {// May generate exception
      var result = double ();
      result.value = ieee_quad_to_double (value);
      process_except ();
      return result;
    }
    func from_mpi (op) {// May generate exception
      check_mpi (op);
      value = ieee_quad_from_integer (op);
      process_except ();
    }
    func to_mpi (size) {// May generate exception
      var result = mpis.from_string (size, "0");
      ieee_integer_from_quad (value, result);
      process_except ();
      return result;
    }
    func to_binary_string (base) {// May generate exception
      if (type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      return ieee_quad_to_binary_string (value, base);
    }
    func to_string () {return ieee_quad_to_string (value);}
    func from_binary_string (str, base) {// May generate exception
      if (type (str) != vector || eltype (str) != char || type (base) != int)
	throw ieee_excepts.optype ();
      if (base != 2 && base != 4 && base != 8 &&  base != 16)
 	throw ieee_excepts.opvalue ();
      value = ieee_quad_from_binary_string (str, base);
      process_except ();
    }
    func from_string (str) {// May generate exception
      if (type (str) != vector || eltype (str) != char)
	throw ieee_excepts.optype ();
      value = ieee_quad_from_string (str);
      process_except ();
    }
    func from_float (f) {
      if (type (f) != float)
	throw ieee_excepts.optype ();
      value = ieee_quad_from_float (f);
      process_except ();
    }
    if (str != nil)
      from_string (str);
    else
      value = ieee_quad_positive_zero ();
  }

  reset ();
}

var ieees = __ieee_package ();
