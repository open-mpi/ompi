#include "pnbc_osc_trigger.h"

typedef int
  (*trigger_triggered_one_fn_t)
  (int, int*);
typedef enum TRIGGER_ACTION_STATE
  (*trigger_action_one_cb_fn_t)
  (int, void*);
typedef void
  (*trigger_reset_one_fn_t)
  (int, int*);

struct triggerable_array {
  trigger_triggered_fn_t triggered_all;
  trigger_triggered_one_fn_t triggered_one;
  int progress;
  int num_triggers;
  int (*triggers)[];
  trigger_action_cb_fn_t action_all;
  void *action_all_cbstate;
  trigger_action_one_cb_fn_t action_one;
  void *action_one_cbstate;
  trigger_reset_fn_t reset_all;
  trigger_reset_one_fn_t reset_one;
  int auto_reset;
};
typedef struct triggerable_array triggerable_array;

void trigger_reset_all(triggerable_array *array);
void trigger_reset_one(triggerable_array *array, int index);

enum TRIGGER_ACTION_STATE trigger_test_all(triggerable_array *array);
enum TRIGGER_ACTION_STATE trigger_test_one(triggerable_array *array, int index);

/* some more built-in trigger functions */
int triggered_one_bynonzero_int(int index, int *trigger);
int triggered_one_byzero_int(int index, int *trigger);
void reset_one_to_zero_int(int index, int *trigger);
enum TRIGGER_ACTION_STATE action_one_noop(int index, void *cbstate);
