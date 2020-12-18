#ifndef PNBC_OSC_TRIGGER_COMMON_H
#define PNBC_OSC_TRIGGER_COMMON_H

typedef int FLAG_t;

enum TRIGGER_ACTION_STATE {
  TRIGGER_PENDING=-1,
  ACTION_SUCCESS=0,
  ACTION_PROBLEM=1,
};

// used by trigger_single and
// used by trigger_array 'all'
typedef enum TRIGGER_ACTION_STATE
  (*trigger_test_all_fn_t)
  (FLAG_t*, void*);
typedef enum TRIGGER_ACTION_STATE
  (*trigger_action_all_cb_fn_t)
  (void*);
typedef void
  (*trigger_reset_all_fn_t)
  (FLAG_t*, FLAG_t);

// used by trigger_array 'one'
typedef enum TRIGGER_ACTION_STATE
  (*trigger_test_one_fn_t)
  (FLAG_t*, int, void*);
typedef enum TRIGGER_ACTION_STATE
  (*trigger_action_one_cb_fn_t)
  (int, void*);
typedef void
  (*trigger_reset_one_fn_t)
  (FLAG_t*, int, FLAG_t);

/* some built-in trigger functions */
int triggered_all_bynonzero_int(FLAG_t *trigger, void *cbstate);
int triggered_all_byzero_int(FLAG_t *trigger, void *cbstate);
void reset_all_to_zero_int(FLAG_t *trigger, FLAG_t value);
enum TRIGGER_ACTION_STATE action_all_noop(void *cbstate);

/* some more built-in trigger functions */
int triggered_one_bynonzero_int(FLAG_t *trigger, int index, void *cbstate);
int triggered_one_byzero_int(FLAG_t *trigger, int index, void* cbstate);
void reset_one_to_zero_int(FLAG_t *trigger, int index, FLAG_t value);
enum TRIGGER_ACTION_STATE action_one_noop(int index, void *cbstate);

#endif
