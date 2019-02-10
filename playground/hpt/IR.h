/*
  abstract program:
  - memory count
  - register count
  - constants ; vector of constant values
  - instruction vector ; contains all instructions
  - block vector ; offset + count into instruction vector
*/
#include <cstdint>
#include <vector>
#include <set>

typedef int32_t simple_type_t;
typedef int32_t tag_t;
typedef int32_t reg_t;
typedef int32_t mem_t;
typedef int32_t intset_id_t;
typedef int32_t block_id_t;

/////////////////////////////////////

// range
struct range_t {
  int32_t from;
  int32_t to;
};

/////////////////////////////////////

// predicate
enum predicate_type {
  PRE_TAG_IN        = 100,
  PRE_TAG_NOT_IN    = 101,
  PRE_VALUE_IN      = 102,
  PRE_VALUE_NOT_IN  = 103,
};

struct predicate_t {
  enum predicate_type type;
  union {
    intset_id_t   tag_set_id;
    range_t       range;
  };
};

/////////////////////////////////////

// condition
enum condition_type {
  CON_NODE_TYPE_EXISTS  = 200,
  CON_SIMLE_TYPE_EXISTS = 201,
  CON_NOT_IN            = 202,
  CON_ANY               = 203,
};

struct condition_t {
  enum condition_type type;
  union {
    tag_t         tag;
    simple_type_t simple_type;
    intset_id_t   tag_set_id;
    predicate_t   predicate;
  };
};

/////////////////////////////////////

// selector
enum selector_type {
  SEL_NODE_ITEM             = 300,
  SEL_CONDITION_AS_SELECTOR = 301,
  SEL_ALL_FIELDS            = 302,
};

struct selector_t {
  enum selector_type type;
  union {
    struct {
      tag_t     node_tag;
      int32_t   item_index;
    };
    condition_t condition;
  };
};

/////////////////////////////////////

// constant
enum constant_type {
  CONST_SIMPLE_TYPE   = 400,
  CONST_HEAP_LOCATION = 401,
  CONST_NODE_TYPE     = 402,
  CONST_NODE_ITEM     = 403,
};

struct constant_t {
  enum constant_type type;
  union {
    simple_type_t simple_type;
    mem_t mem;
    struct {
      tag_t     node_tag;
      int32_t   item_index; // index or arity
      int32_t   item_value;
    };
    condition_t condition;
  };
};

/////////////////////////////////////

// instructions
enum cmd_type {
  CMD_IF                  = 500,
  CMD_PROJECT             = 501,
  CMD_EXTEND              = 502,
  CMD_MOVE                = 503,
  CMD_RESTRICTED_MOVE     = 504,
  CMD_CONDITIONAL_MOVE    = 505,
  CMD_FETCH               = 506,
  CMD_STORE               = 507,
  CMD_UPDATE              = 508,
  CMD_RESTRICTED_UPDATE   = 509,
  CMD_CONDITIONAL_UPDATE  = 510,
  CMD_SET                 = 511,
};

struct cmd_t {

  enum cmd_type type;

  union {

    struct {
      condition_t   condition;
      reg_t         src_reg;
      block_id_t    block_id;
    } cmd_if;

    struct {
      selector_t    src_selector;
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_project;

    struct {
      reg_t         src_reg;
      selector_t    dst_selector;
      reg_t         dst_reg;
    } cmd_extend;

    struct {
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_move;

    struct {
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_restricted_move;

    struct {
      reg_t         src_reg;
      predicate_t   predicate;
      reg_t         dst_reg;
    } cmd_conditional_move;

    struct {
      reg_t         address_reg;
      reg_t         dst_reg;
    } cmd_fetch;

    struct {
      reg_t         src_reg;
      mem_t         address;
    } cmd_store;

    struct {
      reg_t         src_reg;
      reg_t         address_reg;
    } cmd_update;

    struct {
      reg_t         src_reg;
      reg_t         address_reg;
    } cmd_restricted_update;

    struct {
      reg_t         src_reg;
      predicate_t   predicate;
      reg_t         address_reg;
    } cmd_conditional_update;

    struct {
      reg_t         dst_reg;
      constant_t    constant;
    } cmd_set;

  };
};

struct abstract_program_t {
  int32_t   memory_count;
  int32_t   register_count;

  std::vector<cmd_t>              cmd;
  std::vector<range_t>            block;
  std::vector<std::set<int32_t>>  intset;
};


/*
  binary format:
  everything is int32

struct range_t {
struct predicate_t {
struct condition_t {
struct selector_t {
struct cmd_t {

  memory count    i32
  register count  i32
  cmd count       i32
  cmds ...
  block count     i32
  blocks (ranges) ...
  intset count    i32
    set size      i32
    set elems ... [i32]
*/


struct ctx_t {
  int32_t buffer[];
  int32_t buffer_size;
  int32_t *ptr;
  bool error;
};

void read_error(ctx_t &ctx) {
  ctx.error = true;
}

void read_range(ctx_t &ctx, range_t &range) {
  if (ctx.error) return;

  range.from = *ctx.ptr++;
  range.to = *ctx.ptr++;
}

void read_predicate(ctx_t &ctx, predicate_t &predicate) {
  if (ctx.error) return;

  predicate.type = (predicate_type) (*ctx.ptr++);
  switch (predicate.type) {
    case PRE_TAG_IN:
    case PRE_TAG_NOT_IN:
      predicate.tag_set_id = *ctx.ptr++;
      break;
    case PRE_VALUE_IN:
    case PRE_VALUE_NOT_IN:
      read_range(ctx, predicate.range);
      break;
    default:
      read_error(ctx);
  }
}

void read_condition(ctx_t &ctx, condition_t &condition) {
  if (ctx.error) return;

  condition.type = (condition_type) (*ctx.ptr++);
  switch (condition.type) {
    case CON_NODE_TYPE_EXISTS:
      condition.tag = *ctx.ptr++;
      break;
    case CON_SIMLE_TYPE_EXISTS:
      condition.simple_type = *ctx.ptr++;
      break;
    case CON_NOT_IN:
      condition.tag_set_id = *ctx.ptr++;
    case CON_ANY:
      read_predicate(ctx, condition.predicate);
      break;
    default:
      read_error(ctx);
  }
}

void read_selector(ctx_t &ctx, selector_t &selector) {
  if (ctx.error) return;

  selector.type = (selector_type) (*ctx.ptr++);
  switch (selector.type) {
    case SEL_NODE_ITEM:
      selector.node_tag = *ctx.ptr++;
      selector.item_index = *ctx.ptr++;
      break;
    case SEL_CONDITION_AS_SELECTOR:
      read_condition(ctx, selector.condition);
      break;
    case SEL_ALL_FIELDS:
      break;
    default:
      read_error(ctx);
  }
}

void read_constant(ctx_t &ctx, constant_t &constant) {
  if (ctx.error) return;

  constant.type = (constant_type) (*ctx.ptr++);
  switch (constant.type) {
    case CONST_SIMPLE_TYPE:
      constant.simple_type = *ctx.ptr++;
    case CONST_HEAP_LOCATION:
      constant.mem = *ctx.ptr++;
    case CONST_NODE_TYPE:
      constant.node_tag = *ctx.ptr++;
      constant.item_index = *ctx.ptr++;
    case CONST_NODE_ITEM:
      constant.node_tag = *ctx.ptr++;
      constant.item_index = *ctx.ptr++;
      constant.item_value = *ctx.ptr++;
    default:
      read_error(ctx);
      break;
  }
}

void read_cmd(ctx_t &ctx, cmd_t &c) {
  if (ctx.error) return;

  c.type = (cmd_type) (*ctx.ptr++);
  switch (c.type) {
    case CMD_IF:
      read_condition(ctx, c.cmd_if.condition);
      c.cmd_if.src_reg = *ctx.ptr++;
      c.cmd_if.block_id = *ctx.ptr++;
      break;
    case CMD_PROJECT:
      read_selector(ctx, c.cmd_project.src_selector);
      c.cmd_project.src_reg = *ctx.ptr++;
      c.cmd_project.dst_reg = *ctx.ptr++;
      break;
    case CMD_EXTEND:
      c.cmd_extend.src_reg = *ctx.ptr++;
      read_selector(ctx, c.cmd_extend.dst_selector);
      c.cmd_extend.dst_reg = *ctx.ptr++;
      break;
    case CMD_MOVE:
      c.cmd_move.src_reg = *ctx.ptr++;
      c.cmd_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_RESTRICTED_MOVE:
      c.cmd_restricted_move.src_reg = *ctx.ptr++;
      c.cmd_restricted_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_CONDITIONAL_MOVE:
      c.cmd_conditional_move.src_reg = *ctx.ptr++;
      read_predicate(ctx, c.cmd_conditional_move.predicate);
      c.cmd_conditional_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_FETCH:
      c.cmd_fetch.address_reg = *ctx.ptr++;
      c.cmd_fetch.dst_reg = *ctx.ptr++;
      break;
    case CMD_STORE:
      c.cmd_store.src_reg = *ctx.ptr++;
      c.cmd_store.address = *ctx.ptr++;
      break;
    case CMD_UPDATE:
      c.cmd_update.src_reg = *ctx.ptr++;
      c.cmd_update.address_reg = *ctx.ptr++;
      break;
    case CMD_RESTRICTED_UPDATE:
      c.cmd_restricted_update.src_reg = *ctx.ptr++;
      c.cmd_restricted_update.address_reg = *ctx.ptr++;
      break;
    case CMD_CONDITIONAL_UPDATE:
      c.cmd_conditional_update.src_reg = *ctx.ptr++;
      read_predicate(ctx, c.cmd_conditional_update.predicate);
      c.cmd_conditional_update.address_reg = *ctx.ptr++;
      break;
    case CMD_SET:
      c.cmd_set.dst_reg = *ctx.ptr++;
      read_constant(ctx, c.cmd_set.constant);
      break;
    default:
      read_error(ctx);
  }
}

abstract_program_t* read_abstract_program(ctx_t &ctx) {
  int32_t count, i, size, j;

  abstract_program_t *prg = new abstract_program_t();

  // memory and register count
  prg->memory_count = *ctx.ptr++;
  prg->register_count = *ctx.ptr++;

  // commands
  count = *ctx.ptr++;
  prg->cmd.resize(count);
  for (i = 0; i < count; i++) {
    read_cmd(ctx, prg->cmd[i]);
    if (ctx.error) return prg;
  }

  // blocks
  count = *ctx.ptr++;
  prg->block.resize(count);
  for (i = 0; i < count; i++) {
    read_range(ctx, prg->block[i]);
  }

  // int sets
  count = *ctx.ptr++;
  prg->intset.resize(count);
  for (i = 0; i < count; i++) {
    size = *ctx.ptr++;
    for (j = 0; j < size; j++) {
      prg->intset[i].insert(*ctx.ptr++);
    }
  }

  return prg;
}