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
  class mpi_except () {
    class mpi_type (msg) {}
    class mpi_size () {}
    class mpi_base () {}
    class mpi_unequal_size () {}
    class mpi_overflow () {}
  }
}
final class mpi_package () {
  var mpi_excepts = excepts.mpi_except ();
  // if you change the value, please change it in mpi.c and check
  // maximal value in arithm.c.
  var final max_mpi_size = 128;
  var mpi_ignore_overflow = 0;
  
  final class mpi (final size) { // The order of vars is important for mpi.c
    private value; var value;
    if (type (size) != int)
      throw mpi_excepts.mpi_type ();
    if (size < 1 || size > max_mpi_size)
      throw mpi_excepts.mpi_size ();
  }
  private check, check2, check_overflow;
  func check (op) {
    if (type (op) != class () || !inside (op, mpi_package))
      throw mpi_excepts.mpi_type();
  }
  func check2 (op1, op2) {
    check (op1); check (op2);
    if (op1.size != op2.size)
      throw mpi_excepts.mpi_unequal_size();
  }
  extern mpi_overflow;
  func check_overflow (op) {
    if (mpi_overflow && !mpi_ignore_overflow)
      throw mpi_excepts.mpi_overflow();
    return op;
  }
  private mpi_add, mpi_unsigned_add, mpi_subtract, mpi_unsigned_subtract,
    mpi_multiply, mpi_unsigned_multiply, mpi_divide, mpi_unsigned_divide,
    mpi_remainder, mpi_unsigned_remainder,
    mpi_shift_right, mpi_unsigned_shift_right,
    mpi_shift_left, mpi_unsigned_shift_left,
    mpi_or, mpi_unsigned_or, mpi_and, mpi_unsigned_and,
    mpi_not, mpi_unsigned_not, mpi_eq, mpi_unsigned_eq,
    mpi_ne, mpi_unsigned_ne, mpi_gt, mpi_unsigned_gt, mpi_lt, mpi_unsigned_lt,
    mpi_ge, mpi_unsigned_ge, mpi_le, mpi_unsigned_le,
    mpi_change_size, mpi_unsigned_change_size,
    mpi_to_based_string, mpi_unsigned_to_based_string,
    mpi_from_based_string, mpi_unsigned_from_based_string;
  extern mpi_add(), mpi_unsigned_add(),
    mpi_subtract(), mpi_unsigned_subtract(),
    mpi_multiply(), mpi_unsigned_multiply(),
    mpi_divide(), mpi_unsigned_divide(),
    mpi_remainder(), mpi_unsigned_remainder(),
    mpi_shift_right(), mpi_unsigned_shift_right(),
    mpi_shift_left(), mpi_unsigned_shift_left(),
    mpi_or(), mpi_unsigned_or(), mpi_and(), mpi_unsigned_and(),
    mpi_not(), mpi_unsigned_not(), mpi_eq(), mpi_unsigned_eq(),
    mpi_ne(), mpi_unsigned_ne(), mpi_gt(), mpi_unsigned_gt(),
    mpi_lt(), mpi_unsigned_lt(), mpi_ge(), mpi_unsigned_ge(),
    mpi_le(), mpi_unsigned_le(), mpi_change_size(), mpi_unsigned_change_size(),
    mpi_to_based_string(), mpi_unsigned_to_based_string(),
    mpi_from_based_string(), mpi_unsigned_from_based_string();
  func add (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_add (op1, op2, new op1));
  }
  func unsigned_add (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_add (op1, op2, new op1));
  }
  func subtract (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_subtract (op1, op2, new op1));
  }
  func unsigned_subtract (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_subtract (op1, op2, new op1));
  }
  func multiply (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_multiply (op1, op2, new op1));
  }
  func unsigned_multiply (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_multiply (op1, op2, new op1));
  }
  func divide (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_divide (op1, op2, new op1));
  }
  func unsigned_divide (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_divide (op1, op2, new op1));
  }
  func remainder (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_remainder (op1, op2, new op1));
  }
  func unsigned_remainder (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_remainder (op1, op2, new op1));
  }
  func shift_right (op, shift) {
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return mpi_shift_right (op, shift, new op);
  }
  func unsigned_shift_right (op, shift) {
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return mpi_unsigned_shift_right (op, shift, new op);
  }
  func shift_left (op, shift) { // Overflow is possible
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return check_overflow (mpi_shift_left (op, shift, new op));
  }
  func unsigned_shift_left (op, shift) { // Overflow is possible
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return check_overflow (mpi_unsigned_shift_left (op, shift, new op));
  }
  func or (op1, op2) {
    check2 (op1, op2);
    return mpi_or (op1, op2, new op1);
  }
  func unsigned_or (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_or (op1, op2, new op1);
  }
  func and (op1, op2) {
    check2 (op1, op2);
    return mpi_and (op1, op2, new op1);
  }
  func unsigned_and (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_and (op1, op2, new op1);
  }
  func not (op) {
    check (op);
    return mpi_not (op, new op);
  }
  func unsigned_not (op) {
    check (op);
    return mpi_unsigned_not (op, new op);
  }
  func eq (op1, op2) {
    check2 (op1, op2);
    return mpi_eq (op1, op2);
  }
  func unsigned_eq (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_eq (op1, op2);
  }
  func ne (op1, op2) {
    check2 (op1, op2);
    return mpi_ne (op1, op2);
  }
  func unsigned_ne (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_ne (op1, op2);
  }
  func gt (op1, op2) {
    check2 (op1, op2);
    return mpi_gt (op1, op2);
  }
  func unsigned_gt (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_gt (op1, op2);
  }
  func lt (op1, op2) {
    check2 (op1, op2);
    return mpi_lt (op1, op2);
  }
  func unsigned_lt (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_lt (op1, op2);
  }
  func ge (op1, op2) {
    check2 (op1, op2);
    return mpi_ge (op1, op2);
  }
  func unsigned_ge (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_ge (op1, op2);
  }
  func le (op1, op2) {
    check2 (op1, op2);
    return mpi_le (op1, op2);
  }
  func unsigned_le (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_le (op1, op2);
  }
  func change_size (op, new_size) { // Overflow is possible
    check (op);
    if (type (new_size) != int)
      throw mpi_excepts.mpi_type();
    if (new_size < 1 || new_size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    return check_overflow (mpi_change_size (op, new_size, new op));
  }
  func unsigned_change_size (op, new_size) { // Overflow is possible
    check (op);
    if (type (new_size) != int)
      throw mpi_excepts.mpi_type();
    if (new_size < 1 || new_size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    return check_overflow (mpi_unsigned_change_size (op, new_size, new op));
  }
  func to_based_string (op, base) {
    if (type (base) != int)
      throw mpi_excepts.mpi_type();
    if (base < 2 || base > 16)
      throw mpi_excepts.mpi_base();
    check (op);
    return mpi_to_based_string (op, base);
  }
  func unsigned_to_based_string (op, base) {
    if (type (base) != int)
      throw mpi_excepts.mpi_type();
    if (base < 2 || base > 16)
      throw mpi_excepts.mpi_base();
    check (op);
    return mpi_unsigned_to_based_string (op, base);
  }
  func to_string (op) {
    return to_based_string (op, 10);
  }
  func unsigned_to_string (op) {
    return unsigned_to_based_string (op, 10);
  }
  func from_based_string (size, string, base) { // Overflow is possible
    if (type (size) != int || type (base) != int
        || type (string) != vector ||  eltype (string) != char)
      throw mpi_excepts.mpi_type();
    if (size < 1 || size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    if (base < 2 || base > 16)
      throw mpi_excepts.mpi_base();
    return check_overflow (mpi_from_based_string (string, mpi (size), base));
  }
  func unsigned_from_based_string (size, string, base) { // Overflow is poss.
    if (type (size) != int || type (base) != int
        || type (string) != vector ||  eltype (string) != char)
      throw mpi_excepts.mpi_type();
    if (size < 1 || size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    if (base < 2 || base > 16)
      throw mpi_excepts.mpi_base();
    return check_overflow (mpi_unsigned_from_based_string (string, mpi (size),
							   base));
  }
  func from_string (size, string) {
    return from_based_string (size, string, 10);
  }
  func unsigned_from_string (size, string) {
    return unsigned_from_based_string (size, string, 10);
  }
}

var mpis = mpi_package ();
