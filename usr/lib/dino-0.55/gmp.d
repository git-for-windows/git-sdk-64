/*
   Copyright (C) 2005, 2006 Vladimir Makarov.

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
  class gmp_except () {
    class gmp_param_val (msg) {}
    class gmp_type (msg) {}
  }
}

final class mpz_package () {
  var gmp_excepts = excepts.gmp_except ();

  extern _z_create(), _z_clear(), _z_set(), _z_set_str(), _z_get_si(),
         _z_get_d(), _z_get_str(), _z_set_bit(), _z_clr_bit(), _z_tst_bit(),
         _z_add(), _z_sub(), _z_neg(), _z_abs(), _z_mul(), _z_tdiv_q(),
         _z_tdiv_r(), _z_pow_ui(), _z_root(), _z_cmp(), _z_ior(), _z_xor(),
         _z_and(), _z_com(), _z_urandomm();
  private _z_create, _z_clear, _z_set, _z_set_str, _z_get_si, _z_get_d,
          _z_get_str, _z_set_bit, _z_clr_bit, _z_tst_bit, _z_add, _z_sub,
          _z_neg, _z_abs, _z_mul, _z_tdiv_q, _z_tdiv_r, _z_cmp, _z_pow_ui,
          _z_root, _z_ior, _z_xor, _z_and, _z_com, _z_urandomm;
  private check, check2, check_ui;

  final class mpz (i) {
    private mpz_val; var mpz_val;
    if (type (i) != int) throw gmp_excepts.gmp_param_val ();
    mpz_val = _z_create (i);
    func destroy () { _z_clear (mpz_val); }
  }
  func check_ui (n) {
    if (type (n) != int || n < 0) throw gmp_excepts.gmp_param_val ();
  }
  func check (op) {
    if (type (op) != class () || ! inside (op, mpz_package))
      throw gmp_excepts.gmp_type ();
  }
  func check2 (op1, op2) {
    if (type (op1) != class () || ! inside (op1, mpz_package)
	|| type (op2) != class () || ! inside (op2, mpz_package))
      throw gmp_excepts.gmp_type ();
  }
  func set (lhs, rhs) {check2 (lhs, rhs);  return _z_set (lhs, rhs);}
  func set_str (lhs, str) {
    check (lhs);
    if (type (str) != vector ||  eltype (str) != char)
      throw gmp_excepts.gmp_type();
    return _z_set_str (lhs, str);
  }
  func get_i (op)  { check (op); return _z_get_si (op); }
  func get_f (op)  { check (op); return _z_get_d (op); }
  func get_str (op, base)  {
    check (op);
    if (type (base) != int
	|| (base != 10 && base != 2 && base != 8 && base != 16))
      throw gmp_excepts.gmp_param_val ();
    return _z_get_str (op, base);
  }
  func set_bit (op, n) { check (op); check_ui (n); return _z_set_bit (op, n); }
  func clr_bit (op, n) { check (op); check_ui (n); return _z_clr_bit (op, n); }
  func tst_bit (op, n) { check (op); check_ui (n); return _z_tst_bit (op, n); }
  func add (op1, op2) { check2 (op1, op2); return _z_add (mpz (0), op1, op2); }
  func sub (op1, op2) { check2 (op1, op2); return _z_sub (mpz (0), op1, op2); }
  func neg (op) { check (op); return _z_neg (mpz (0), op); }
  func abs (op) { check (op); return _z_abs (mpz (0), op); }
  func mul (op1, op2) { check2 (op1, op2); return _z_mul (mpz (0), op1, op2); }
  func div (n, d) { check2 (n, d); return _z_tdiv_q (mpz (0), n, d); }
  func mod (n, d) { check2 (n, d); return _z_tdiv_r (mpz (0), n, d); }
  func pow_ui (base, n) {
    check (base); check_ui (n); return _z_pow_ui (mpz (0), base, n);
  }
  func root (op, n) {
    check (op); check_ui (n); return _z_root (mpz (0), op, n);
  }
  func cmp (op1, op2) { check2 (op1, op2); return _z_cmp (op1, op2); }
  func ior (op1, op2) { check2 (op1, op2); return _z_ior (mpz (0), op1, op2); }
  func xor (op1, op2) { check2 (op1, op2); return _z_xor (mpz (0), op1, op2); }
  func and (op1, op2) { check2 (op1, op2); return _z_and (mpz (0), op1, op2); }
  func com (op) { check (op); return _z_com (mpz (0), op); }
  func rand (n, first) {
    if (type (first) != int) throw gmp_excepts.gmp_param_val ();
    check (n);
    return _z_urandomm (mpz (0), n, first);
  }
}

var mpzs = mpz_package ();
