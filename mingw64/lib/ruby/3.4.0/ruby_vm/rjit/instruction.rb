module RubyVM::RJIT # :nodoc: all
  Instruction = Data.define(:name, :bin, :len, :operands)

  INSNS = {
    0 => Instruction.new(
      name: :nop,
      bin: 0, # BIN(nop)
      len: 1, # insn_len
      operands: [],
    ),
    1 => Instruction.new(
      name: :getlocal,
      bin: 1, # BIN(getlocal)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}, {:decl=>"rb_num_t level", :type=>"rb_num_t", :name=>"level"}],
    ),
    2 => Instruction.new(
      name: :setlocal,
      bin: 2, # BIN(setlocal)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}, {:decl=>"rb_num_t level", :type=>"rb_num_t", :name=>"level"}],
    ),
    3 => Instruction.new(
      name: :getblockparam,
      bin: 3, # BIN(getblockparam)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}, {:decl=>"rb_num_t level", :type=>"rb_num_t", :name=>"level"}],
    ),
    4 => Instruction.new(
      name: :setblockparam,
      bin: 4, # BIN(setblockparam)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}, {:decl=>"rb_num_t level", :type=>"rb_num_t", :name=>"level"}],
    ),
    5 => Instruction.new(
      name: :getblockparamproxy,
      bin: 5, # BIN(getblockparamproxy)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}, {:decl=>"rb_num_t level", :type=>"rb_num_t", :name=>"level"}],
    ),
    6 => Instruction.new(
      name: :getspecial,
      bin: 6, # BIN(getspecial)
      len: 3, # insn_len
      operands: [{:decl=>"rb_num_t key", :type=>"rb_num_t", :name=>"key"}, {:decl=>"rb_num_t type", :type=>"rb_num_t", :name=>"type"}],
    ),
    7 => Instruction.new(
      name: :setspecial,
      bin: 7, # BIN(setspecial)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t key", :type=>"rb_num_t", :name=>"key"}],
    ),
    8 => Instruction.new(
      name: :getinstancevariable,
      bin: 8, # BIN(getinstancevariable)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"IVC ic", :type=>"IVC", :name=>"ic"}],
    ),
    9 => Instruction.new(
      name: :setinstancevariable,
      bin: 9, # BIN(setinstancevariable)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"IVC ic", :type=>"IVC", :name=>"ic"}],
    ),
    10 => Instruction.new(
      name: :getclassvariable,
      bin: 10, # BIN(getclassvariable)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"ICVARC ic", :type=>"ICVARC", :name=>"ic"}],
    ),
    11 => Instruction.new(
      name: :setclassvariable,
      bin: 11, # BIN(setclassvariable)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"ICVARC ic", :type=>"ICVARC", :name=>"ic"}],
    ),
    12 => Instruction.new(
      name: :opt_getconstant_path,
      bin: 12, # BIN(opt_getconstant_path)
      len: 2, # insn_len
      operands: [{:decl=>"IC ic", :type=>"IC", :name=>"ic"}],
    ),
    13 => Instruction.new(
      name: :getconstant,
      bin: 13, # BIN(getconstant)
      len: 2, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}],
    ),
    14 => Instruction.new(
      name: :setconstant,
      bin: 14, # BIN(setconstant)
      len: 2, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}],
    ),
    15 => Instruction.new(
      name: :getglobal,
      bin: 15, # BIN(getglobal)
      len: 2, # insn_len
      operands: [{:decl=>"ID gid", :type=>"ID", :name=>"gid"}],
    ),
    16 => Instruction.new(
      name: :setglobal,
      bin: 16, # BIN(setglobal)
      len: 2, # insn_len
      operands: [{:decl=>"ID gid", :type=>"ID", :name=>"gid"}],
    ),
    17 => Instruction.new(
      name: :putnil,
      bin: 17, # BIN(putnil)
      len: 1, # insn_len
      operands: [],
    ),
    18 => Instruction.new(
      name: :putself,
      bin: 18, # BIN(putself)
      len: 1, # insn_len
      operands: [],
    ),
    19 => Instruction.new(
      name: :putobject,
      bin: 19, # BIN(putobject)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE val", :type=>"VALUE", :name=>"val"}],
    ),
    20 => Instruction.new(
      name: :putspecialobject,
      bin: 20, # BIN(putspecialobject)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t value_type", :type=>"rb_num_t", :name=>"value_type"}],
    ),
    21 => Instruction.new(
      name: :putstring,
      bin: 21, # BIN(putstring)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE str", :type=>"VALUE", :name=>"str"}],
    ),
    22 => Instruction.new(
      name: :putchilledstring,
      bin: 22, # BIN(putchilledstring)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE str", :type=>"VALUE", :name=>"str"}],
    ),
    23 => Instruction.new(
      name: :concatstrings,
      bin: 23, # BIN(concatstrings)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}],
    ),
    24 => Instruction.new(
      name: :anytostring,
      bin: 24, # BIN(anytostring)
      len: 1, # insn_len
      operands: [],
    ),
    25 => Instruction.new(
      name: :toregexp,
      bin: 25, # BIN(toregexp)
      len: 3, # insn_len
      operands: [{:decl=>"rb_num_t opt", :type=>"rb_num_t", :name=>"opt"}, {:decl=>"rb_num_t cnt", :type=>"rb_num_t", :name=>"cnt"}],
    ),
    26 => Instruction.new(
      name: :intern,
      bin: 26, # BIN(intern)
      len: 1, # insn_len
      operands: [],
    ),
    27 => Instruction.new(
      name: :newarray,
      bin: 27, # BIN(newarray)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}],
    ),
    28 => Instruction.new(
      name: :pushtoarraykwsplat,
      bin: 28, # BIN(pushtoarraykwsplat)
      len: 1, # insn_len
      operands: [],
    ),
    29 => Instruction.new(
      name: :duparray,
      bin: 29, # BIN(duparray)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE ary", :type=>"VALUE", :name=>"ary"}],
    ),
    30 => Instruction.new(
      name: :duphash,
      bin: 30, # BIN(duphash)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE hash", :type=>"VALUE", :name=>"hash"}],
    ),
    31 => Instruction.new(
      name: :expandarray,
      bin: 31, # BIN(expandarray)
      len: 3, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}, {:decl=>"rb_num_t flag", :type=>"rb_num_t", :name=>"flag"}],
    ),
    32 => Instruction.new(
      name: :concatarray,
      bin: 32, # BIN(concatarray)
      len: 1, # insn_len
      operands: [],
    ),
    33 => Instruction.new(
      name: :concattoarray,
      bin: 33, # BIN(concattoarray)
      len: 1, # insn_len
      operands: [],
    ),
    34 => Instruction.new(
      name: :pushtoarray,
      bin: 34, # BIN(pushtoarray)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}],
    ),
    35 => Instruction.new(
      name: :splatarray,
      bin: 35, # BIN(splatarray)
      len: 2, # insn_len
      operands: [{:decl=>"VALUE flag", :type=>"VALUE", :name=>"flag"}],
    ),
    36 => Instruction.new(
      name: :splatkw,
      bin: 36, # BIN(splatkw)
      len: 1, # insn_len
      operands: [],
    ),
    37 => Instruction.new(
      name: :newhash,
      bin: 37, # BIN(newhash)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}],
    ),
    38 => Instruction.new(
      name: :newrange,
      bin: 38, # BIN(newrange)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t flag", :type=>"rb_num_t", :name=>"flag"}],
    ),
    39 => Instruction.new(
      name: :pop,
      bin: 39, # BIN(pop)
      len: 1, # insn_len
      operands: [],
    ),
    40 => Instruction.new(
      name: :dup,
      bin: 40, # BIN(dup)
      len: 1, # insn_len
      operands: [],
    ),
    41 => Instruction.new(
      name: :dupn,
      bin: 41, # BIN(dupn)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t n", :type=>"rb_num_t", :name=>"n"}],
    ),
    42 => Instruction.new(
      name: :swap,
      bin: 42, # BIN(swap)
      len: 1, # insn_len
      operands: [],
    ),
    43 => Instruction.new(
      name: :opt_reverse,
      bin: 43, # BIN(opt_reverse)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t n", :type=>"rb_num_t", :name=>"n"}],
    ),
    44 => Instruction.new(
      name: :topn,
      bin: 44, # BIN(topn)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t n", :type=>"rb_num_t", :name=>"n"}],
    ),
    45 => Instruction.new(
      name: :setn,
      bin: 45, # BIN(setn)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t n", :type=>"rb_num_t", :name=>"n"}],
    ),
    46 => Instruction.new(
      name: :adjuststack,
      bin: 46, # BIN(adjuststack)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t n", :type=>"rb_num_t", :name=>"n"}],
    ),
    47 => Instruction.new(
      name: :defined,
      bin: 47, # BIN(defined)
      len: 4, # insn_len
      operands: [{:decl=>"rb_num_t op_type", :type=>"rb_num_t", :name=>"op_type"}, {:decl=>"VALUE obj", :type=>"VALUE", :name=>"obj"}, {:decl=>"VALUE pushval", :type=>"VALUE", :name=>"pushval"}],
    ),
    48 => Instruction.new(
      name: :definedivar,
      bin: 48, # BIN(definedivar)
      len: 4, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"IVC ic", :type=>"IVC", :name=>"ic"}, {:decl=>"VALUE pushval", :type=>"VALUE", :name=>"pushval"}],
    ),
    49 => Instruction.new(
      name: :checkmatch,
      bin: 49, # BIN(checkmatch)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t flag", :type=>"rb_num_t", :name=>"flag"}],
    ),
    50 => Instruction.new(
      name: :checkkeyword,
      bin: 50, # BIN(checkkeyword)
      len: 3, # insn_len
      operands: [{:decl=>"lindex_t kw_bits_index", :type=>"lindex_t", :name=>"kw_bits_index"}, {:decl=>"lindex_t keyword_index", :type=>"lindex_t", :name=>"keyword_index"}],
    ),
    51 => Instruction.new(
      name: :checktype,
      bin: 51, # BIN(checktype)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t type", :type=>"rb_num_t", :name=>"type"}],
    ),
    52 => Instruction.new(
      name: :defineclass,
      bin: 52, # BIN(defineclass)
      len: 4, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"ISEQ class_iseq", :type=>"ISEQ", :name=>"class_iseq"}, {:decl=>"rb_num_t flags", :type=>"rb_num_t", :name=>"flags"}],
    ),
    53 => Instruction.new(
      name: :definemethod,
      bin: 53, # BIN(definemethod)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"ISEQ iseq", :type=>"ISEQ", :name=>"iseq"}],
    ),
    54 => Instruction.new(
      name: :definesmethod,
      bin: 54, # BIN(definesmethod)
      len: 3, # insn_len
      operands: [{:decl=>"ID id", :type=>"ID", :name=>"id"}, {:decl=>"ISEQ iseq", :type=>"ISEQ", :name=>"iseq"}],
    ),
    55 => Instruction.new(
      name: :send,
      bin: 55, # BIN(send)
      len: 3, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}, {:decl=>"ISEQ blockiseq", :type=>"ISEQ", :name=>"blockiseq"}],
    ),
    56 => Instruction.new(
      name: :sendforward,
      bin: 56, # BIN(sendforward)
      len: 3, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}, {:decl=>"ISEQ blockiseq", :type=>"ISEQ", :name=>"blockiseq"}],
    ),
    57 => Instruction.new(
      name: :opt_send_without_block,
      bin: 57, # BIN(opt_send_without_block)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    58 => Instruction.new(
      name: :objtostring,
      bin: 58, # BIN(objtostring)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    59 => Instruction.new(
      name: :opt_ary_freeze,
      bin: 59, # BIN(opt_ary_freeze)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE ary", :type=>"VALUE", :name=>"ary"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    60 => Instruction.new(
      name: :opt_hash_freeze,
      bin: 60, # BIN(opt_hash_freeze)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE hash", :type=>"VALUE", :name=>"hash"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    61 => Instruction.new(
      name: :opt_str_freeze,
      bin: 61, # BIN(opt_str_freeze)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE str", :type=>"VALUE", :name=>"str"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    62 => Instruction.new(
      name: :opt_nil_p,
      bin: 62, # BIN(opt_nil_p)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    63 => Instruction.new(
      name: :opt_str_uminus,
      bin: 63, # BIN(opt_str_uminus)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE str", :type=>"VALUE", :name=>"str"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    64 => Instruction.new(
      name: :opt_duparray_send,
      bin: 64, # BIN(opt_duparray_send)
      len: 4, # insn_len
      operands: [{:decl=>"VALUE ary", :type=>"VALUE", :name=>"ary"}, {:decl=>"ID method", :type=>"ID", :name=>"method"}, {:decl=>"rb_num_t argc", :type=>"rb_num_t", :name=>"argc"}],
    ),
    65 => Instruction.new(
      name: :opt_newarray_send,
      bin: 65, # BIN(opt_newarray_send)
      len: 3, # insn_len
      operands: [{:decl=>"rb_num_t num", :type=>"rb_num_t", :name=>"num"}, {:decl=>"rb_num_t method", :type=>"rb_num_t", :name=>"method"}],
    ),
    66 => Instruction.new(
      name: :invokesuper,
      bin: 66, # BIN(invokesuper)
      len: 3, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}, {:decl=>"ISEQ blockiseq", :type=>"ISEQ", :name=>"blockiseq"}],
    ),
    67 => Instruction.new(
      name: :invokesuperforward,
      bin: 67, # BIN(invokesuperforward)
      len: 3, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}, {:decl=>"ISEQ blockiseq", :type=>"ISEQ", :name=>"blockiseq"}],
    ),
    68 => Instruction.new(
      name: :invokeblock,
      bin: 68, # BIN(invokeblock)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    69 => Instruction.new(
      name: :leave,
      bin: 69, # BIN(leave)
      len: 1, # insn_len
      operands: [],
    ),
    70 => Instruction.new(
      name: :throw,
      bin: 70, # BIN(throw)
      len: 2, # insn_len
      operands: [{:decl=>"rb_num_t throw_state", :type=>"rb_num_t", :name=>"throw_state"}],
    ),
    71 => Instruction.new(
      name: :jump,
      bin: 71, # BIN(jump)
      len: 2, # insn_len
      operands: [{:decl=>"OFFSET dst", :type=>"OFFSET", :name=>"dst"}],
    ),
    72 => Instruction.new(
      name: :branchif,
      bin: 72, # BIN(branchif)
      len: 2, # insn_len
      operands: [{:decl=>"OFFSET dst", :type=>"OFFSET", :name=>"dst"}],
    ),
    73 => Instruction.new(
      name: :branchunless,
      bin: 73, # BIN(branchunless)
      len: 2, # insn_len
      operands: [{:decl=>"OFFSET dst", :type=>"OFFSET", :name=>"dst"}],
    ),
    74 => Instruction.new(
      name: :branchnil,
      bin: 74, # BIN(branchnil)
      len: 2, # insn_len
      operands: [{:decl=>"OFFSET dst", :type=>"OFFSET", :name=>"dst"}],
    ),
    75 => Instruction.new(
      name: :once,
      bin: 75, # BIN(once)
      len: 3, # insn_len
      operands: [{:decl=>"ISEQ iseq", :type=>"ISEQ", :name=>"iseq"}, {:decl=>"ISE ise", :type=>"ISE", :name=>"ise"}],
    ),
    76 => Instruction.new(
      name: :opt_case_dispatch,
      bin: 76, # BIN(opt_case_dispatch)
      len: 3, # insn_len
      operands: [{:decl=>"CDHASH hash", :type=>"CDHASH", :name=>"hash"}, {:decl=>"OFFSET else_offset", :type=>"OFFSET", :name=>"else_offset"}],
    ),
    77 => Instruction.new(
      name: :opt_plus,
      bin: 77, # BIN(opt_plus)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    78 => Instruction.new(
      name: :opt_minus,
      bin: 78, # BIN(opt_minus)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    79 => Instruction.new(
      name: :opt_mult,
      bin: 79, # BIN(opt_mult)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    80 => Instruction.new(
      name: :opt_div,
      bin: 80, # BIN(opt_div)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    81 => Instruction.new(
      name: :opt_mod,
      bin: 81, # BIN(opt_mod)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    82 => Instruction.new(
      name: :opt_eq,
      bin: 82, # BIN(opt_eq)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    83 => Instruction.new(
      name: :opt_neq,
      bin: 83, # BIN(opt_neq)
      len: 3, # insn_len
      operands: [{:decl=>"CALL_DATA cd_eq", :type=>"CALL_DATA", :name=>"cd_eq"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    84 => Instruction.new(
      name: :opt_lt,
      bin: 84, # BIN(opt_lt)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    85 => Instruction.new(
      name: :opt_le,
      bin: 85, # BIN(opt_le)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    86 => Instruction.new(
      name: :opt_gt,
      bin: 86, # BIN(opt_gt)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    87 => Instruction.new(
      name: :opt_ge,
      bin: 87, # BIN(opt_ge)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    88 => Instruction.new(
      name: :opt_ltlt,
      bin: 88, # BIN(opt_ltlt)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    89 => Instruction.new(
      name: :opt_and,
      bin: 89, # BIN(opt_and)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    90 => Instruction.new(
      name: :opt_or,
      bin: 90, # BIN(opt_or)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    91 => Instruction.new(
      name: :opt_aref,
      bin: 91, # BIN(opt_aref)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    92 => Instruction.new(
      name: :opt_aset,
      bin: 92, # BIN(opt_aset)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    93 => Instruction.new(
      name: :opt_aset_with,
      bin: 93, # BIN(opt_aset_with)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE key", :type=>"VALUE", :name=>"key"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    94 => Instruction.new(
      name: :opt_aref_with,
      bin: 94, # BIN(opt_aref_with)
      len: 3, # insn_len
      operands: [{:decl=>"VALUE key", :type=>"VALUE", :name=>"key"}, {:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    95 => Instruction.new(
      name: :opt_length,
      bin: 95, # BIN(opt_length)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    96 => Instruction.new(
      name: :opt_size,
      bin: 96, # BIN(opt_size)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    97 => Instruction.new(
      name: :opt_empty_p,
      bin: 97, # BIN(opt_empty_p)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    98 => Instruction.new(
      name: :opt_succ,
      bin: 98, # BIN(opt_succ)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    99 => Instruction.new(
      name: :opt_not,
      bin: 99, # BIN(opt_not)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    100 => Instruction.new(
      name: :opt_regexpmatch2,
      bin: 100, # BIN(opt_regexpmatch2)
      len: 2, # insn_len
      operands: [{:decl=>"CALL_DATA cd", :type=>"CALL_DATA", :name=>"cd"}],
    ),
    101 => Instruction.new(
      name: :invokebuiltin,
      bin: 101, # BIN(invokebuiltin)
      len: 2, # insn_len
      operands: [{:decl=>"RB_BUILTIN bf", :type=>"RB_BUILTIN", :name=>"bf"}],
    ),
    102 => Instruction.new(
      name: :opt_invokebuiltin_delegate,
      bin: 102, # BIN(opt_invokebuiltin_delegate)
      len: 3, # insn_len
      operands: [{:decl=>"RB_BUILTIN bf", :type=>"RB_BUILTIN", :name=>"bf"}, {:decl=>"rb_num_t index", :type=>"rb_num_t", :name=>"index"}],
    ),
    103 => Instruction.new(
      name: :opt_invokebuiltin_delegate_leave,
      bin: 103, # BIN(opt_invokebuiltin_delegate_leave)
      len: 3, # insn_len
      operands: [{:decl=>"RB_BUILTIN bf", :type=>"RB_BUILTIN", :name=>"bf"}, {:decl=>"rb_num_t index", :type=>"rb_num_t", :name=>"index"}],
    ),
    104 => Instruction.new(
      name: :getlocal_WC_0,
      bin: 104, # BIN(getlocal_WC_0)
      len: 2, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}],
    ),
    105 => Instruction.new(
      name: :getlocal_WC_1,
      bin: 105, # BIN(getlocal_WC_1)
      len: 2, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}],
    ),
    106 => Instruction.new(
      name: :setlocal_WC_0,
      bin: 106, # BIN(setlocal_WC_0)
      len: 2, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}],
    ),
    107 => Instruction.new(
      name: :setlocal_WC_1,
      bin: 107, # BIN(setlocal_WC_1)
      len: 2, # insn_len
      operands: [{:decl=>"lindex_t idx", :type=>"lindex_t", :name=>"idx"}],
    ),
    108 => Instruction.new(
      name: :putobject_INT2FIX_0_,
      bin: 108, # BIN(putobject_INT2FIX_0_)
      len: 1, # insn_len
      operands: [],
    ),
    109 => Instruction.new(
      name: :putobject_INT2FIX_1_,
      bin: 109, # BIN(putobject_INT2FIX_1_)
      len: 1, # insn_len
      operands: [],
    ),
    110 => Instruction.new(
      name: :trace_nop,
      bin: 110, # BIN(trace_nop)
      len: 1, # insn_len
      operands: nil,
    ),
    111 => Instruction.new(
      name: :trace_getlocal,
      bin: 111, # BIN(trace_getlocal)
      len: 3, # insn_len
      operands: nil,
    ),
    112 => Instruction.new(
      name: :trace_setlocal,
      bin: 112, # BIN(trace_setlocal)
      len: 3, # insn_len
      operands: nil,
    ),
    113 => Instruction.new(
      name: :trace_getblockparam,
      bin: 113, # BIN(trace_getblockparam)
      len: 3, # insn_len
      operands: nil,
    ),
    114 => Instruction.new(
      name: :trace_setblockparam,
      bin: 114, # BIN(trace_setblockparam)
      len: 3, # insn_len
      operands: nil,
    ),
    115 => Instruction.new(
      name: :trace_getblockparamproxy,
      bin: 115, # BIN(trace_getblockparamproxy)
      len: 3, # insn_len
      operands: nil,
    ),
    116 => Instruction.new(
      name: :trace_getspecial,
      bin: 116, # BIN(trace_getspecial)
      len: 3, # insn_len
      operands: nil,
    ),
    117 => Instruction.new(
      name: :trace_setspecial,
      bin: 117, # BIN(trace_setspecial)
      len: 2, # insn_len
      operands: nil,
    ),
    118 => Instruction.new(
      name: :trace_getinstancevariable,
      bin: 118, # BIN(trace_getinstancevariable)
      len: 3, # insn_len
      operands: nil,
    ),
    119 => Instruction.new(
      name: :trace_setinstancevariable,
      bin: 119, # BIN(trace_setinstancevariable)
      len: 3, # insn_len
      operands: nil,
    ),
    120 => Instruction.new(
      name: :trace_getclassvariable,
      bin: 120, # BIN(trace_getclassvariable)
      len: 3, # insn_len
      operands: nil,
    ),
    121 => Instruction.new(
      name: :trace_setclassvariable,
      bin: 121, # BIN(trace_setclassvariable)
      len: 3, # insn_len
      operands: nil,
    ),
    122 => Instruction.new(
      name: :trace_opt_getconstant_path,
      bin: 122, # BIN(trace_opt_getconstant_path)
      len: 2, # insn_len
      operands: nil,
    ),
    123 => Instruction.new(
      name: :trace_getconstant,
      bin: 123, # BIN(trace_getconstant)
      len: 2, # insn_len
      operands: nil,
    ),
    124 => Instruction.new(
      name: :trace_setconstant,
      bin: 124, # BIN(trace_setconstant)
      len: 2, # insn_len
      operands: nil,
    ),
    125 => Instruction.new(
      name: :trace_getglobal,
      bin: 125, # BIN(trace_getglobal)
      len: 2, # insn_len
      operands: nil,
    ),
    126 => Instruction.new(
      name: :trace_setglobal,
      bin: 126, # BIN(trace_setglobal)
      len: 2, # insn_len
      operands: nil,
    ),
    127 => Instruction.new(
      name: :trace_putnil,
      bin: 127, # BIN(trace_putnil)
      len: 1, # insn_len
      operands: nil,
    ),
    128 => Instruction.new(
      name: :trace_putself,
      bin: 128, # BIN(trace_putself)
      len: 1, # insn_len
      operands: nil,
    ),
    129 => Instruction.new(
      name: :trace_putobject,
      bin: 129, # BIN(trace_putobject)
      len: 2, # insn_len
      operands: nil,
    ),
    130 => Instruction.new(
      name: :trace_putspecialobject,
      bin: 130, # BIN(trace_putspecialobject)
      len: 2, # insn_len
      operands: nil,
    ),
    131 => Instruction.new(
      name: :trace_putstring,
      bin: 131, # BIN(trace_putstring)
      len: 2, # insn_len
      operands: nil,
    ),
    132 => Instruction.new(
      name: :trace_putchilledstring,
      bin: 132, # BIN(trace_putchilledstring)
      len: 2, # insn_len
      operands: nil,
    ),
    133 => Instruction.new(
      name: :trace_concatstrings,
      bin: 133, # BIN(trace_concatstrings)
      len: 2, # insn_len
      operands: nil,
    ),
    134 => Instruction.new(
      name: :trace_anytostring,
      bin: 134, # BIN(trace_anytostring)
      len: 1, # insn_len
      operands: nil,
    ),
    135 => Instruction.new(
      name: :trace_toregexp,
      bin: 135, # BIN(trace_toregexp)
      len: 3, # insn_len
      operands: nil,
    ),
    136 => Instruction.new(
      name: :trace_intern,
      bin: 136, # BIN(trace_intern)
      len: 1, # insn_len
      operands: nil,
    ),
    137 => Instruction.new(
      name: :trace_newarray,
      bin: 137, # BIN(trace_newarray)
      len: 2, # insn_len
      operands: nil,
    ),
    138 => Instruction.new(
      name: :trace_pushtoarraykwsplat,
      bin: 138, # BIN(trace_pushtoarraykwsplat)
      len: 1, # insn_len
      operands: nil,
    ),
    139 => Instruction.new(
      name: :trace_duparray,
      bin: 139, # BIN(trace_duparray)
      len: 2, # insn_len
      operands: nil,
    ),
    140 => Instruction.new(
      name: :trace_duphash,
      bin: 140, # BIN(trace_duphash)
      len: 2, # insn_len
      operands: nil,
    ),
    141 => Instruction.new(
      name: :trace_expandarray,
      bin: 141, # BIN(trace_expandarray)
      len: 3, # insn_len
      operands: nil,
    ),
    142 => Instruction.new(
      name: :trace_concatarray,
      bin: 142, # BIN(trace_concatarray)
      len: 1, # insn_len
      operands: nil,
    ),
    143 => Instruction.new(
      name: :trace_concattoarray,
      bin: 143, # BIN(trace_concattoarray)
      len: 1, # insn_len
      operands: nil,
    ),
    144 => Instruction.new(
      name: :trace_pushtoarray,
      bin: 144, # BIN(trace_pushtoarray)
      len: 2, # insn_len
      operands: nil,
    ),
    145 => Instruction.new(
      name: :trace_splatarray,
      bin: 145, # BIN(trace_splatarray)
      len: 2, # insn_len
      operands: nil,
    ),
    146 => Instruction.new(
      name: :trace_splatkw,
      bin: 146, # BIN(trace_splatkw)
      len: 1, # insn_len
      operands: nil,
    ),
    147 => Instruction.new(
      name: :trace_newhash,
      bin: 147, # BIN(trace_newhash)
      len: 2, # insn_len
      operands: nil,
    ),
    148 => Instruction.new(
      name: :trace_newrange,
      bin: 148, # BIN(trace_newrange)
      len: 2, # insn_len
      operands: nil,
    ),
    149 => Instruction.new(
      name: :trace_pop,
      bin: 149, # BIN(trace_pop)
      len: 1, # insn_len
      operands: nil,
    ),
    150 => Instruction.new(
      name: :trace_dup,
      bin: 150, # BIN(trace_dup)
      len: 1, # insn_len
      operands: nil,
    ),
    151 => Instruction.new(
      name: :trace_dupn,
      bin: 151, # BIN(trace_dupn)
      len: 2, # insn_len
      operands: nil,
    ),
    152 => Instruction.new(
      name: :trace_swap,
      bin: 152, # BIN(trace_swap)
      len: 1, # insn_len
      operands: nil,
    ),
    153 => Instruction.new(
      name: :trace_opt_reverse,
      bin: 153, # BIN(trace_opt_reverse)
      len: 2, # insn_len
      operands: nil,
    ),
    154 => Instruction.new(
      name: :trace_topn,
      bin: 154, # BIN(trace_topn)
      len: 2, # insn_len
      operands: nil,
    ),
    155 => Instruction.new(
      name: :trace_setn,
      bin: 155, # BIN(trace_setn)
      len: 2, # insn_len
      operands: nil,
    ),
    156 => Instruction.new(
      name: :trace_adjuststack,
      bin: 156, # BIN(trace_adjuststack)
      len: 2, # insn_len
      operands: nil,
    ),
    157 => Instruction.new(
      name: :trace_defined,
      bin: 157, # BIN(trace_defined)
      len: 4, # insn_len
      operands: nil,
    ),
    158 => Instruction.new(
      name: :trace_definedivar,
      bin: 158, # BIN(trace_definedivar)
      len: 4, # insn_len
      operands: nil,
    ),
    159 => Instruction.new(
      name: :trace_checkmatch,
      bin: 159, # BIN(trace_checkmatch)
      len: 2, # insn_len
      operands: nil,
    ),
    160 => Instruction.new(
      name: :trace_checkkeyword,
      bin: 160, # BIN(trace_checkkeyword)
      len: 3, # insn_len
      operands: nil,
    ),
    161 => Instruction.new(
      name: :trace_checktype,
      bin: 161, # BIN(trace_checktype)
      len: 2, # insn_len
      operands: nil,
    ),
    162 => Instruction.new(
      name: :trace_defineclass,
      bin: 162, # BIN(trace_defineclass)
      len: 4, # insn_len
      operands: nil,
    ),
    163 => Instruction.new(
      name: :trace_definemethod,
      bin: 163, # BIN(trace_definemethod)
      len: 3, # insn_len
      operands: nil,
    ),
    164 => Instruction.new(
      name: :trace_definesmethod,
      bin: 164, # BIN(trace_definesmethod)
      len: 3, # insn_len
      operands: nil,
    ),
    165 => Instruction.new(
      name: :trace_send,
      bin: 165, # BIN(trace_send)
      len: 3, # insn_len
      operands: nil,
    ),
    166 => Instruction.new(
      name: :trace_sendforward,
      bin: 166, # BIN(trace_sendforward)
      len: 3, # insn_len
      operands: nil,
    ),
    167 => Instruction.new(
      name: :trace_opt_send_without_block,
      bin: 167, # BIN(trace_opt_send_without_block)
      len: 2, # insn_len
      operands: nil,
    ),
    168 => Instruction.new(
      name: :trace_objtostring,
      bin: 168, # BIN(trace_objtostring)
      len: 2, # insn_len
      operands: nil,
    ),
    169 => Instruction.new(
      name: :trace_opt_ary_freeze,
      bin: 169, # BIN(trace_opt_ary_freeze)
      len: 3, # insn_len
      operands: nil,
    ),
    170 => Instruction.new(
      name: :trace_opt_hash_freeze,
      bin: 170, # BIN(trace_opt_hash_freeze)
      len: 3, # insn_len
      operands: nil,
    ),
    171 => Instruction.new(
      name: :trace_opt_str_freeze,
      bin: 171, # BIN(trace_opt_str_freeze)
      len: 3, # insn_len
      operands: nil,
    ),
    172 => Instruction.new(
      name: :trace_opt_nil_p,
      bin: 172, # BIN(trace_opt_nil_p)
      len: 2, # insn_len
      operands: nil,
    ),
    173 => Instruction.new(
      name: :trace_opt_str_uminus,
      bin: 173, # BIN(trace_opt_str_uminus)
      len: 3, # insn_len
      operands: nil,
    ),
    174 => Instruction.new(
      name: :trace_opt_duparray_send,
      bin: 174, # BIN(trace_opt_duparray_send)
      len: 4, # insn_len
      operands: nil,
    ),
    175 => Instruction.new(
      name: :trace_opt_newarray_send,
      bin: 175, # BIN(trace_opt_newarray_send)
      len: 3, # insn_len
      operands: nil,
    ),
    176 => Instruction.new(
      name: :trace_invokesuper,
      bin: 176, # BIN(trace_invokesuper)
      len: 3, # insn_len
      operands: nil,
    ),
    177 => Instruction.new(
      name: :trace_invokesuperforward,
      bin: 177, # BIN(trace_invokesuperforward)
      len: 3, # insn_len
      operands: nil,
    ),
    178 => Instruction.new(
      name: :trace_invokeblock,
      bin: 178, # BIN(trace_invokeblock)
      len: 2, # insn_len
      operands: nil,
    ),
    179 => Instruction.new(
      name: :trace_leave,
      bin: 179, # BIN(trace_leave)
      len: 1, # insn_len
      operands: nil,
    ),
    180 => Instruction.new(
      name: :trace_throw,
      bin: 180, # BIN(trace_throw)
      len: 2, # insn_len
      operands: nil,
    ),
    181 => Instruction.new(
      name: :trace_jump,
      bin: 181, # BIN(trace_jump)
      len: 2, # insn_len
      operands: nil,
    ),
    182 => Instruction.new(
      name: :trace_branchif,
      bin: 182, # BIN(trace_branchif)
      len: 2, # insn_len
      operands: nil,
    ),
    183 => Instruction.new(
      name: :trace_branchunless,
      bin: 183, # BIN(trace_branchunless)
      len: 2, # insn_len
      operands: nil,
    ),
    184 => Instruction.new(
      name: :trace_branchnil,
      bin: 184, # BIN(trace_branchnil)
      len: 2, # insn_len
      operands: nil,
    ),
    185 => Instruction.new(
      name: :trace_once,
      bin: 185, # BIN(trace_once)
      len: 3, # insn_len
      operands: nil,
    ),
    186 => Instruction.new(
      name: :trace_opt_case_dispatch,
      bin: 186, # BIN(trace_opt_case_dispatch)
      len: 3, # insn_len
      operands: nil,
    ),
    187 => Instruction.new(
      name: :trace_opt_plus,
      bin: 187, # BIN(trace_opt_plus)
      len: 2, # insn_len
      operands: nil,
    ),
    188 => Instruction.new(
      name: :trace_opt_minus,
      bin: 188, # BIN(trace_opt_minus)
      len: 2, # insn_len
      operands: nil,
    ),
    189 => Instruction.new(
      name: :trace_opt_mult,
      bin: 189, # BIN(trace_opt_mult)
      len: 2, # insn_len
      operands: nil,
    ),
    190 => Instruction.new(
      name: :trace_opt_div,
      bin: 190, # BIN(trace_opt_div)
      len: 2, # insn_len
      operands: nil,
    ),
    191 => Instruction.new(
      name: :trace_opt_mod,
      bin: 191, # BIN(trace_opt_mod)
      len: 2, # insn_len
      operands: nil,
    ),
    192 => Instruction.new(
      name: :trace_opt_eq,
      bin: 192, # BIN(trace_opt_eq)
      len: 2, # insn_len
      operands: nil,
    ),
    193 => Instruction.new(
      name: :trace_opt_neq,
      bin: 193, # BIN(trace_opt_neq)
      len: 3, # insn_len
      operands: nil,
    ),
    194 => Instruction.new(
      name: :trace_opt_lt,
      bin: 194, # BIN(trace_opt_lt)
      len: 2, # insn_len
      operands: nil,
    ),
    195 => Instruction.new(
      name: :trace_opt_le,
      bin: 195, # BIN(trace_opt_le)
      len: 2, # insn_len
      operands: nil,
    ),
    196 => Instruction.new(
      name: :trace_opt_gt,
      bin: 196, # BIN(trace_opt_gt)
      len: 2, # insn_len
      operands: nil,
    ),
    197 => Instruction.new(
      name: :trace_opt_ge,
      bin: 197, # BIN(trace_opt_ge)
      len: 2, # insn_len
      operands: nil,
    ),
    198 => Instruction.new(
      name: :trace_opt_ltlt,
      bin: 198, # BIN(trace_opt_ltlt)
      len: 2, # insn_len
      operands: nil,
    ),
    199 => Instruction.new(
      name: :trace_opt_and,
      bin: 199, # BIN(trace_opt_and)
      len: 2, # insn_len
      operands: nil,
    ),
    200 => Instruction.new(
      name: :trace_opt_or,
      bin: 200, # BIN(trace_opt_or)
      len: 2, # insn_len
      operands: nil,
    ),
    201 => Instruction.new(
      name: :trace_opt_aref,
      bin: 201, # BIN(trace_opt_aref)
      len: 2, # insn_len
      operands: nil,
    ),
    202 => Instruction.new(
      name: :trace_opt_aset,
      bin: 202, # BIN(trace_opt_aset)
      len: 2, # insn_len
      operands: nil,
    ),
    203 => Instruction.new(
      name: :trace_opt_aset_with,
      bin: 203, # BIN(trace_opt_aset_with)
      len: 3, # insn_len
      operands: nil,
    ),
    204 => Instruction.new(
      name: :trace_opt_aref_with,
      bin: 204, # BIN(trace_opt_aref_with)
      len: 3, # insn_len
      operands: nil,
    ),
    205 => Instruction.new(
      name: :trace_opt_length,
      bin: 205, # BIN(trace_opt_length)
      len: 2, # insn_len
      operands: nil,
    ),
    206 => Instruction.new(
      name: :trace_opt_size,
      bin: 206, # BIN(trace_opt_size)
      len: 2, # insn_len
      operands: nil,
    ),
    207 => Instruction.new(
      name: :trace_opt_empty_p,
      bin: 207, # BIN(trace_opt_empty_p)
      len: 2, # insn_len
      operands: nil,
    ),
    208 => Instruction.new(
      name: :trace_opt_succ,
      bin: 208, # BIN(trace_opt_succ)
      len: 2, # insn_len
      operands: nil,
    ),
    209 => Instruction.new(
      name: :trace_opt_not,
      bin: 209, # BIN(trace_opt_not)
      len: 2, # insn_len
      operands: nil,
    ),
    210 => Instruction.new(
      name: :trace_opt_regexpmatch2,
      bin: 210, # BIN(trace_opt_regexpmatch2)
      len: 2, # insn_len
      operands: nil,
    ),
    211 => Instruction.new(
      name: :trace_invokebuiltin,
      bin: 211, # BIN(trace_invokebuiltin)
      len: 2, # insn_len
      operands: nil,
    ),
    212 => Instruction.new(
      name: :trace_opt_invokebuiltin_delegate,
      bin: 212, # BIN(trace_opt_invokebuiltin_delegate)
      len: 3, # insn_len
      operands: nil,
    ),
    213 => Instruction.new(
      name: :trace_opt_invokebuiltin_delegate_leave,
      bin: 213, # BIN(trace_opt_invokebuiltin_delegate_leave)
      len: 3, # insn_len
      operands: nil,
    ),
    214 => Instruction.new(
      name: :trace_getlocal_WC_0,
      bin: 214, # BIN(trace_getlocal_WC_0)
      len: 2, # insn_len
      operands: nil,
    ),
    215 => Instruction.new(
      name: :trace_getlocal_WC_1,
      bin: 215, # BIN(trace_getlocal_WC_1)
      len: 2, # insn_len
      operands: nil,
    ),
    216 => Instruction.new(
      name: :trace_setlocal_WC_0,
      bin: 216, # BIN(trace_setlocal_WC_0)
      len: 2, # insn_len
      operands: nil,
    ),
    217 => Instruction.new(
      name: :trace_setlocal_WC_1,
      bin: 217, # BIN(trace_setlocal_WC_1)
      len: 2, # insn_len
      operands: nil,
    ),
    218 => Instruction.new(
      name: :trace_putobject_INT2FIX_0_,
      bin: 218, # BIN(trace_putobject_INT2FIX_0_)
      len: 1, # insn_len
      operands: nil,
    ),
    219 => Instruction.new(
      name: :trace_putobject_INT2FIX_1_,
      bin: 219, # BIN(trace_putobject_INT2FIX_1_)
      len: 1, # insn_len
      operands: nil,
    ),
  }
end
