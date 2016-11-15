#ifndef __ER_d_extern__
#define __ER_d_extern__

#include <stddef.h>

#ifdef offsetof
#define _ER_offsetof(type, field) offsetof (type, field)
#else
#define _ER_offsetof(type, field) ((char *)&((type *) 64)->field - (char *) 64)
#endif


#line 35 "./d_extern.sprut"


#include "d_types.h"

typedef void *hide_t;

typedef struct IR_node *IR_hidden_node_t;

typedef char *char_ptr_t;

/* This enumeration declares all possible states of of heap instance
   which contains function destroy.  Don't change it.  */
enum instance_state
{
  IS_initial = 0,
  IS_to_be_destroyed,
  IS_destroyed
};

typedef enum instance_state instance_state_t;

/* This enumeration declares all possible states of process. */
enum process_status
{
  /* Process is not blocked.  Created process has also this status.
     Remember that we automatically zero all free memory.  Maximum one
     process can have status PS_READY or PS_STARTED.  This state can
     come only from the 3 last states. */
  PS_READY = 1,
  /* It can come from only PS_READY after execution of a stmt. */
  PS_STARTED,
  /* Process is blocked because its quantum has been exhausted.  It can
     come from only PS_STARTED after execution of a stmt. */
  PS_BLOCKED_BY_QUANTUM_SWITCH,
  /* Process is blocked by wait-stmt. It can come from only
     PS_STARTED. */
  PS_BLOCKED_BY_WAIT,
  /* Process is blocked by wait-stmt.  It can come from only PS_READY. */
  PS_NOT_STARTED
};

typedef enum process_status process_status_t;

typedef struct ER_node **stack_ptr_t;

enum _ER_type
{
  ER_T_nil,
  ER_T_hide,
  ER_T_hideblock,
  ER_T_char,
  ER_T_int,
  ER_T_float,
  ER_T_vector,
  ER_T_table,
  ER_T_thread,
  ER_T_func,
  ER_T_class,
  ER_T_instance,
  ER_T_stack,
  ER_T_process,
  ER_T_type
};

typedef enum _ER_type ER_type_t;



#line 83 "d_extern.h"

typedef enum ER_node_mode_enum
{
  ER_NM_heap_obj,
  ER_NM_heap_vect_tab,
  ER_NM_heap_vect,
  ER_NM_heap_pack_vect,
  ER_NM_heap_unpack_vect,
  ER_NM_heap_tab,
  ER_NM_context_heap_obj,
  ER_NM_heap_instance,
  ER_NM_heap_stack,
  ER_NM_heap_process,
  ER_NM_heap_hideblock,
  ER_NM_val,
  ER_NM_nil,
  ER_NM_hide,
  ER_NM_hideblock,
  ER_NM_char,
  ER_NM_int,
  ER_NM_float,
  ER_NM_type,
  ER_NM_vect,
  ER_NM_tab,
  ER_NM_empty_entry,
  ER_NM_deleted_entry,
  ER_NM_instance,
  ER_NM_func,
  ER_NM_class,
  ER_NM_stack,
  ER_NM_process,
  ER_NM_external_var_ref,
  ER_NM__root,
  ER_NM__error
} ER_node_mode_t;

extern short _ER_node_level [];

extern unsigned char _ER_SF_heap_obj [];

extern unsigned char _ER_SF_heap_vect_tab [];

extern unsigned char _ER_SF_heap_vect [];

extern unsigned char _ER_SF_heap_pack_vect [];

extern unsigned char _ER_SF_heap_unpack_vect [];

extern unsigned char _ER_SF_heap_tab [];

extern unsigned char _ER_SF_context_heap_obj [];

extern unsigned char _ER_SF_heap_instance [];

extern unsigned char _ER_SF_heap_stack [];

extern unsigned char _ER_SF_heap_process [];

extern unsigned char _ER_SF_heap_hideblock [];

extern unsigned char _ER_SF_val [];

extern unsigned char _ER_SF_nil [];

extern unsigned char _ER_SF_hide [];

extern unsigned char _ER_SF_hideblock [];

extern unsigned char _ER_SF_char [];

extern unsigned char _ER_SF_int [];

extern unsigned char _ER_SF_float [];

extern unsigned char _ER_SF_type [];

extern unsigned char _ER_SF_vect [];

extern unsigned char _ER_SF_tab [];

extern unsigned char _ER_SF_empty_entry [];

extern unsigned char _ER_SF_deleted_entry [];

extern unsigned char _ER_SF_instance [];

extern unsigned char _ER_SF_func [];

extern unsigned char _ER_SF_class [];

extern unsigned char _ER_SF_stack [];

extern unsigned char _ER_SF_process [];

extern unsigned char _ER_SF_external_var_ref [];

extern unsigned char _ER_SF__root [];

extern unsigned char _ER_SF__error [];

extern unsigned char *_ER_is_type[];

extern void *_ER_class_structure_array [];

typedef struct ER_node *ER_node_t;

typedef struct _ER_double_link *ER_double_link_t;

struct _ER_double_link
{
  ER_node_t field_itself;
  ER_node_t link_owner;
  void (*set_function) (ER_node_t, ER_node_t);
  ER_double_link_t previous_link;
  ER_double_link_t next_link;
};

struct _ER_S_heap_obj
{
  bool_t  immutable;
  bool_t  it_was_processed;
  instance_state_t  state;
  char_ptr_t  new_place;
  size_t  unique_number;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_obj _ER_S_heap_obj;
} _ER_heap_obj;

struct _ER_S_heap_vect_tab
{
  struct _ER_S_heap_obj _ER_M_heap_obj;
  size_t  allocated_length;
  size_t  els_number;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_vect_tab _ER_S_heap_vect_tab;
} _ER_heap_vect_tab;

struct _ER_S_heap_vect
{
  struct _ER_S_heap_vect_tab _ER_M_heap_vect_tab;
  ER_node_mode_t  pack_vect_el_type;
  size_t  disp;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_vect _ER_S_heap_vect;
} _ER_heap_vect;

struct _ER_S_heap_pack_vect
{
  struct _ER_S_heap_vect _ER_M_heap_vect;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_pack_vect _ER_S_heap_pack_vect;
} _ER_heap_pack_vect;

struct _ER_S_heap_unpack_vect
{
  struct _ER_S_heap_vect _ER_M_heap_vect;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_unpack_vect _ER_S_heap_unpack_vect;
} _ER_heap_unpack_vect;

struct _ER_S_heap_tab
{
  struct _ER_S_heap_vect_tab _ER_M_heap_vect_tab;
  size_t  entries_number;
  size_t  deleted_els_number;
  int_t  last_key_index;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_tab _ER_S_heap_tab;
} _ER_heap_tab;

struct _ER_S_context_heap_obj
{
  struct _ER_S_heap_obj _ER_M_heap_obj;
  ER_node_t  context;
  IR_hidden_node_t  block_node;
  int_t  context_number;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_context_heap_obj _ER_S_context_heap_obj;
} _ER_context_heap_obj;

struct _ER_S_heap_instance
{
  struct _ER_S_context_heap_obj _ER_M_context_heap_obj;
  IR_hidden_node_t  instance_class;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_instance _ER_S_heap_instance;
} _ER_heap_instance;

struct _ER_S_heap_stack
{
  struct _ER_S_context_heap_obj _ER_M_context_heap_obj;
  ER_node_t  prev_stack;
  char_ptr_t  ctop;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_stack _ER_S_heap_stack;
} _ER_heap_stack;

struct _ER_S_heap_process
{
  struct _ER_S_context_heap_obj _ER_M_context_heap_obj;
  IR_hidden_node_t  thread_func;
  process_status_t  process_status;
  ER_node_t  father;
  ER_node_t  prev;
  ER_node_t  next;
  ER_node_t  saved_cstack;
  int_t  process_number;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_process _ER_S_heap_process;
} _ER_heap_process;

struct _ER_S_heap_hideblock
{
  struct _ER_S_heap_obj _ER_M_heap_obj;
  int_t  hideblock_length;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_heap_hideblock _ER_S_heap_hideblock;
} _ER_heap_hideblock;

typedef struct
{
  ER_node_mode_t _ER_node_mode;
} _ER_val;

typedef struct
{
  ER_node_mode_t _ER_node_mode;
} _ER_nil;

struct _ER_S_hide
{
  hide_t  hide;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_hide _ER_S_hide;
} _ER_hide;

struct _ER_S_hideblock
{
  ER_node_t  hideblock;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_hideblock _ER_S_hideblock;
} _ER_hideblock;

struct _ER_S_char
{
  char_t  ch;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_char _ER_S_char;
} _ER_char;

struct _ER_S_int
{
  int_t  i;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_int _ER_S_int;
} _ER_int;

struct _ER_S_float
{
  floating_t  f;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_float _ER_S_float;
} _ER_float;

struct _ER_S_type
{
  ER_type_t  type;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_type _ER_S_type;
} _ER_type;

struct _ER_S_vect
{
  ER_node_t  vect;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_vect _ER_S_vect;
} _ER_vect;

struct _ER_S_tab
{
  ER_node_t  tab;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_tab _ER_S_tab;
} _ER_tab;

typedef struct
{
  ER_node_mode_t _ER_node_mode;
} _ER_empty_entry;

typedef struct
{
  ER_node_mode_t _ER_node_mode;
} _ER_deleted_entry;

struct _ER_S_instance
{
  ER_node_t  instance;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_instance _ER_S_instance;
} _ER_instance;

struct _ER_S_func
{
  ER_node_t  func_context;
  int_t  func_no;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_func _ER_S_func;
} _ER_func;

struct _ER_S_class
{
  ER_node_t  class_context;
  int_t  class_no;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_class _ER_S_class;
} _ER_class;

struct _ER_S_stack
{
  ER_node_t  stack;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_stack _ER_S_stack;
} _ER_stack;

struct _ER_S_process
{
  ER_node_t  process;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_process _ER_S_process;
} _ER_process;

struct _ER_S_external_var_ref
{
  char_ptr_t  external_var_ref;
};

typedef struct
{
  ER_node_mode_t _ER_node_mode;
  struct _ER_S_external_var_ref _ER_S_external_var_ref;
} _ER_external_var_ref;

typedef struct ER_node
{
  ER_node_mode_t _ER_node_mode;
} _ER__root;

typedef struct
{
  ER_node_mode_t _ER_node_mode;
} _ER__error;

#define ER_NODE_MODE(t) ((t)->_ER_node_mode)

extern char *ER_node_name[];

extern unsigned char ER_node_size[];

#define ER_NODE_LEVEL(node) _ER_node_level [ER_NODE_MODE (node)]

#define ER_IS_TYPE(type, super) ((_ER_is_type [super] [type /8] >> (type % 8)) & 1)

#define ER_IS_OF_TYPE(node, super)ER_IS_TYPE (ER_NODE_MODE (node), super)

extern void _ER_set_double_field_value
  (ER_double_link_t link, ER_node_t value, int disp, int flag);

extern ER_double_link_t ER__first_double_link (ER_node_t node);

#define ER__next_double_link(prev_double_link) ((prev_double_link)->next_link)

#define ER__previous_double_link(next_double_link) ((next_double_link)->previous_link)

#define ER__owner(link) ((link)->link_owner)

#define ER_immutable(_node) (((_ER_heap_obj *) (_node))->_ER_S_heap_obj.immutable)

#define ER_it_was_processed(_node) (((_ER_heap_obj *) (_node))->_ER_S_heap_obj.it_was_processed)

#define ER_state(_node) (((_ER_heap_obj *) (_node))->_ER_S_heap_obj.state)

#define ER_new_place(_node) (((_ER_heap_obj *) (_node))->_ER_S_heap_obj.new_place)

#define ER_unique_number(_node) (((_ER_heap_obj *) (_node))->_ER_S_heap_obj.unique_number)

#define ER_allocated_length(_node) (((_ER_heap_vect_tab *) (_node))->_ER_S_heap_vect_tab.allocated_length)

#define ER_els_number(_node) (((_ER_heap_vect_tab *) (_node))->_ER_S_heap_vect_tab.els_number)

#define ER_pack_vect_el_type(_node) (((_ER_heap_vect *) (_node))->_ER_S_heap_vect.pack_vect_el_type)

#define ER_disp(_node) (((_ER_heap_vect *) (_node))->_ER_S_heap_vect.disp)

#define ER_entries_number(_node) (((_ER_heap_tab *) (_node))->_ER_S_heap_tab.entries_number)

#define ER_deleted_els_number(_node) (((_ER_heap_tab *) (_node))->_ER_S_heap_tab.deleted_els_number)

#define ER_last_key_index(_node) (((_ER_heap_tab *) (_node))->_ER_S_heap_tab.last_key_index)

#define ER_context(_node) (((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.context)

#define ER_block_node(_node) (((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.block_node)

#define ER_context_number(_node) (((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.context_number)

#define ER_instance_class(_node) (((_ER_heap_instance *) (_node))->_ER_S_heap_instance.instance_class)

#define ER_prev_stack(_node) (((_ER_heap_stack *) (_node))->_ER_S_heap_stack.prev_stack)

#define ER_ctop(_node) (((_ER_heap_stack *) (_node))->_ER_S_heap_stack.ctop)

#define ER_thread_func(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.thread_func)

#define ER_process_status(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.process_status)

#define ER_father(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.father)

#define ER_prev(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.prev)

#define ER_next(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.next)

#define ER_saved_cstack(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.saved_cstack)

#define ER_process_number(_node) (((_ER_heap_process *) (_node))->_ER_S_heap_process.process_number)

#define ER_hideblock_length(_node) (((_ER_heap_hideblock *) (_node))->_ER_S_heap_hideblock.hideblock_length)

#define ER_hide(_node) (((_ER_hide *) (_node))->_ER_S_hide.hide)

#define ER_hideblock(_node) (((_ER_hideblock *) (_node))->_ER_S_hideblock.hideblock)

#define ER_ch(_node) (((_ER_char *) (_node))->_ER_S_char.ch)

#define ER_i(_node) (((_ER_int *) (_node))->_ER_S_int.i)

#define ER_f(_node) (((_ER_float *) (_node))->_ER_S_float.f)

#define ER_type(_node) (((_ER_type *) (_node))->_ER_S_type.type)

#define ER_vect(_node) (((_ER_vect *) (_node))->_ER_S_vect.vect)

#define ER_tab(_node) (((_ER_tab *) (_node))->_ER_S_tab.tab)

#define ER_instance(_node) (((_ER_instance *) (_node))->_ER_S_instance.instance)

#define ER_func_context(_node) (((_ER_func *) (_node))->_ER_S_func.func_context)

#define ER_func_no(_node) (((_ER_func *) (_node))->_ER_S_func.func_no)

#define ER_class_context(_node) (((_ER_class *) (_node))->_ER_S_class.class_context)

#define ER_class_no(_node) (((_ER_class *) (_node))->_ER_S_class.class_no)

#define ER_stack(_node) (((_ER_stack *) (_node))->_ER_S_stack.stack)

#define ER_process(_node) (((_ER_process *) (_node))->_ER_S_process.process)

#define ER_external_var_ref(_node) (((_ER_external_var_ref *) (_node))->_ER_S_external_var_ref.external_var_ref)

extern void ER__set_double_link (ER_double_link_t link, ER_node_t value);

#define ER_set_immutable(_node, _value) ((((_ER_heap_obj *) (_node))->_ER_S_heap_obj.immutable) = (_value))

#define ER_set_it_was_processed(_node, _value) ((((_ER_heap_obj *) (_node))->_ER_S_heap_obj.it_was_processed) = (_value))

#define ER_set_state(_node, _value) ((((_ER_heap_obj *) (_node))->_ER_S_heap_obj.state) = (_value))

#define ER_set_new_place(_node, _value) ((((_ER_heap_obj *) (_node))->_ER_S_heap_obj.new_place) = (_value))

#define ER_set_unique_number(_node, _value) ((((_ER_heap_obj *) (_node))->_ER_S_heap_obj.unique_number) = (_value))

#define ER_set_allocated_length(_node, _value) ((((_ER_heap_vect_tab *) (_node))->_ER_S_heap_vect_tab.allocated_length) = (_value))

#define ER_set_els_number(_node, _value) ((((_ER_heap_vect_tab *) (_node))->_ER_S_heap_vect_tab.els_number) = (_value))

#define ER_set_pack_vect_el_type(_node, _value) ((((_ER_heap_vect *) (_node))->_ER_S_heap_vect.pack_vect_el_type) = (_value))

#define ER_set_disp(_node, _value) ((((_ER_heap_vect *) (_node))->_ER_S_heap_vect.disp) = (_value))

#define ER_set_entries_number(_node, _value) ((((_ER_heap_tab *) (_node))->_ER_S_heap_tab.entries_number) = (_value))

#define ER_set_deleted_els_number(_node, _value) ((((_ER_heap_tab *) (_node))->_ER_S_heap_tab.deleted_els_number) = (_value))

#define ER_set_last_key_index(_node, _value) ((((_ER_heap_tab *) (_node))->_ER_S_heap_tab.last_key_index) = (_value))

#define ER_set_context(_node, _value) ((((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.context) = (_value))

#define ER_set_block_node(_node, _value) ((((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.block_node) = (_value))

#define ER_set_context_number(_node, _value) ((((_ER_context_heap_obj *) (_node))->_ER_S_context_heap_obj.context_number) = (_value))

#define ER_set_instance_class(_node, _value) ((((_ER_heap_instance *) (_node))->_ER_S_heap_instance.instance_class) = (_value))

#define ER_set_prev_stack(_node, _value) ((((_ER_heap_stack *) (_node))->_ER_S_heap_stack.prev_stack) = (_value))

#define ER_set_ctop(_node, _value) ((((_ER_heap_stack *) (_node))->_ER_S_heap_stack.ctop) = (_value))

#define ER_set_thread_func(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.thread_func) = (_value))

#define ER_set_process_status(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.process_status) = (_value))

#define ER_set_father(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.father) = (_value))

#define ER_set_prev(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.prev) = (_value))

#define ER_set_next(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.next) = (_value))

#define ER_set_saved_cstack(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.saved_cstack) = (_value))

#define ER_set_process_number(_node, _value) ((((_ER_heap_process *) (_node))->_ER_S_heap_process.process_number) = (_value))

#define ER_set_hideblock_length(_node, _value) ((((_ER_heap_hideblock *) (_node))->_ER_S_heap_hideblock.hideblock_length) = (_value))

#define ER_set_hide(_node, _value) ((((_ER_hide *) (_node))->_ER_S_hide.hide) = (_value))

#define ER_set_hideblock(_node, _value) ((((_ER_hideblock *) (_node))->_ER_S_hideblock.hideblock) = (_value))

#define ER_set_ch(_node, _value) ((((_ER_char *) (_node))->_ER_S_char.ch) = (_value))

#define ER_set_i(_node, _value) ((((_ER_int *) (_node))->_ER_S_int.i) = (_value))

#define ER_set_f(_node, _value) ((((_ER_float *) (_node))->_ER_S_float.f) = (_value))

#define ER_set_type(_node, _value) ((((_ER_type *) (_node))->_ER_S_type.type) = (_value))

#define ER_set_vect(_node, _value) ((((_ER_vect *) (_node))->_ER_S_vect.vect) = (_value))

#define ER_set_tab(_node, _value) ((((_ER_tab *) (_node))->_ER_S_tab.tab) = (_value))

#define ER_set_instance(_node, _value) ((((_ER_instance *) (_node))->_ER_S_instance.instance) = (_value))

#define ER_set_func_context(_node, _value) ((((_ER_func *) (_node))->_ER_S_func.func_context) = (_value))

#define ER_set_func_no(_node, _value) ((((_ER_func *) (_node))->_ER_S_func.func_no) = (_value))

#define ER_set_class_context(_node, _value) ((((_ER_class *) (_node))->_ER_S_class.class_context) = (_value))

#define ER_set_class_no(_node, _value) ((((_ER_class *) (_node))->_ER_S_class.class_no) = (_value))

#define ER_set_stack(_node, _value) ((((_ER_stack *) (_node))->_ER_S_stack.stack) = (_value))

#define ER_set_process(_node, _value) ((((_ER_process *) (_node))->_ER_S_process.process) = (_value))

#define ER_set_external_var_ref(_node, _value) ((((_ER_external_var_ref *) (_node))->_ER_S_external_var_ref.external_var_ref) = (_value))

extern ER_node_t ER_create_node (ER_node_mode_t node_mode);

extern void ER_start (void);

extern void ER_stop (void);


#line 103 "./d_extern.sprut"


/* Container for all kind of variables. */
typedef union
  {
    _ER_nil nil;
    _ER_hide hide;
    _ER_hideblock hideblock;
    _ER_char ch;
    _ER_int i;
    _ER_float f;
    _ER_type type;
    _ER_vect vect;
    _ER_tab tab;
    _ER_empty_entry ee;
    _ER_deleted_entry de;
    _ER_instance inst;
    _ER_func func;
    _ER_class cl;
    _ER_process process;
    _ER_external_var_ref external_var_ref;
  } val_t;

/* Return type of node representing value. */
ER_type_t node_mode_2_type (ER_node_mode_t node_mode);

/* Type of all external functions. */
typedef val_t external_func_t (int, val_t *);

/* This function must be used only when dynamic libarary is not
   implemented.  In this case, provide get_library_search_function
   (and main) and link it with the dino library to make an extended
   dino interpriter. */

extern void *get_library_search_function (const char *name);

/* This is trick (usage that ER_NODE_MODE is macro)!!!  Therefore the
   following macro must apply to make new node or to make redirections
   or unpacked vector <-> unpacked vector. */
#define ER_SET_MODE(v, m) (ER_NODE_MODE (v) = (m))

/* Align of heap objects. */
#define MAX_ALIGNMENT sizeof (double)

#if defined(NDEBUG)
/* The macro call value is size of memory allocated for the heap
   object with size S accounting for aligning. */
#define ALLOC_SIZE(s) (((s)+MAX_ALIGNMENT-1)/MAX_ALIGNMENT*MAX_ALIGNMENT)
#else
#define ALLOC_SIZE(s) _alloc_size (s)
#endif

#if defined(NDEBUG)
#define ER_hideblock_start(hideblock)\
  ((char *) (hideblock) + ALLOC_SIZE (sizeof (_ER_heap_hideblock)))
#else
#define ER_hideblock_start(hideblock) _hideblock_start (hideblock)
#endif

#if defined(NDEBUG)
#define ER_pack_els(vect)\
  ((char *) (vect) + ALLOC_SIZE (sizeof (_ER_heap_pack_vect)) + ER_disp (vect))
#else
#define ER_pack_els(vect) _pack_els (vect)
#endif

#if defined(NDEBUG)
#define ER_unpack_els(vect)\
  ((ER_node_t) ((char *) (vect) + ALLOC_SIZE (sizeof (_ER_heap_unpack_vect))\
                + ER_disp (vect)))
#else
#define ER_unpack_els(vect) _unpack_els (vect)
#endif

#if defined(NDEBUG)
#define ER_tab_els(tab)\
  ((ER_node_t) ((char *) (tab) + ALLOC_SIZE (sizeof (_ER_heap_tab))))
#else
#define ER_tab_els(tab) _tab_els (tab)
#endif

#if defined(NDEBUG)
#define ER_instance_vars(instance)\
  ((ER_node_t) ((char *) (instance) + ALLOC_SIZE (sizeof (_ER_heap_instance))))
#else
#define ER_instance_vars(instance) _instance_vars (instance)
#endif

#if defined(NDEBUG)
#define ER_stack_vars(stack)\
  ((ER_node_t) ((char *) (stack) + ALLOC_SIZE (sizeof (_ER_heap_stack))))
#else
#define ER_stack_vars(stack) _stack_vars (stack)
#endif

#if defined(NDEBUG)
#define ER_stacks_table(process)\
  ((stack_ptr_t) ((char *) (process) + ALLOC_SIZE (sizeof (_ER_heap_process))))
#else
#define ER_stacks_table(process) _stacks_table (process)
#endif

#if defined(NDEBUG)
#define INDEXED_VAL(first_var, index)\
   ((ER_node_t) ((char *) (first_var) + (index) * (int) sizeof (val_t)))
#else
#define INDEXED_VAL(first_var, index)\
   _indexed_val ((ER_node_t) first_var, index)
#endif

#if defined(NDEBUG)
#define INDEXED_ENTRY_KEY(first_entry, index)\
   ((ER_node_t) ((char *) (first_entry) + 2 * (index) * (int) sizeof (val_t)))
#else
#define INDEXED_ENTRY_KEY(first_entry, index)\
   _indexed_entry_key (first_entry, index)
#endif

#if defined(NDEBUG)
#define INDEXED_ENTRY_VAL(first_entry, index)\
   ((ER_node_t) ((char *) (first_entry) + (2 * (index) + 1) * (int) sizeof (val_t)))
#else
#define INDEXED_ENTRY_VAL(first_entry, index)\
   _indexed_entry_val (first_entry, index)
#endif

#define DECR_TOP(stack, decr)\
   ER_set_ctop ((stack), ER_ctop (stack) - (decr) * (int) sizeof (val_t))

#ifdef NO_OPTIMIZE
#define DECR_CTOP(decr) DECR_TOP (cstack, decr)
#else
#define DECR_CTOP(decr) (ctop = INDEXED_VAL (ctop, -(decr)))
#endif

#ifndef NDEBUG
size_t _alloc_size(size_t s);

char *_hideblock_start (ER_node_t hideblock);

char *_pack_els (ER_node_t vect);

ER_node_t _unpack_els (ER_node_t vect);

ER_node_t _tab_els (ER_node_t tab);

ER_node_t _instance_vars (ER_node_t instance);

ER_node_t _stack_vars (ER_node_t stack);

stack_ptr_t _stacks_table (ER_node_t process);

ER_node_t _indexed_val (ER_node_t first_var, int index);

ER_node_t _indexed_entry_key (ER_node_t first_entry, int index);

ER_node_t _indexed_entry_val (ER_node_t first_entry, int index);

#endif

int eq_val (val_t *val1_ptr, val_t *val2_ptr, size_t number);

size_t instance_size (IR_hidden_node_t class);
size_t type_size (ER_node_mode_t type);
size_t val_displ (ER_node_t var);

ER_node_t create_hideblock (size_t length);

ER_node_t create_empty_vector (void);
ER_node_t create_unpack_vector (size_t els_number);
ER_node_t create_pack_vector (size_t els_number, ER_node_mode_t eltype);
ER_node_t expand_vector (ER_node_t vect, size_t el_number);
ER_node_t unpack_vector (ER_node_t vect);
void pack_vector_if_possible (ER_node_t unpack_vect);
int eq_vector (ER_node_t v1, ER_node_t v2);
ER_node_t copy_vector (ER_node_t vect);
extern ER_node_t create_empty_string (size_t min_length);
extern ER_node_t create_string (const char *string);

int eq_instance (ER_node_t i1, ER_node_t i2);

int eq_table (ER_node_t t1, ER_node_t t2);
ER_node_t create_tab (size_t size);
ER_node_t find_tab_entry (ER_node_t tab, ER_node_t key, int reserve);
int remove_tab_el (ER_node_t tab, ER_node_t key);
ER_node_t copy_tab (ER_node_t tab);
ER_node_t find_next_key (ER_node_t tab, ER_node_t key);
ER_node_t table_to_vector_conversion (ER_node_t tab);
ER_node_t vector_to_table_conversion (ER_node_t vect);

void make_immutable (ER_node_t obj);



#line 928 "d_extern.h"
#endif /* __ER_d_extern__ */
