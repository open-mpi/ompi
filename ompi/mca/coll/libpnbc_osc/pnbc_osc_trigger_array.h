#ifndef PNBC_OSC_TRIGGER_ARRAY_H
#define PNBC_OSC_TRIGGER_ARRAY_H

#include "pnbc_osc_trigger_common.h"

struct triggerable_array {
  trigger_triggered_all_fn_t    triggered_all;
  trigger_triggered_one_fn_t    triggered_one;
  int                           progress;
  int                           num_triggers;
  FLAG_t                      (*triggers)[];
  trigger_action_all_cb_fn_t    action_all;
  void                         *action_all_cbstate;
  trigger_action_one_cb_fn_t    action_one;
  void                         *action_one_cbstate;
  trigger_reset_all_fn_t        reset_all;
  trigger_reset_one_fn_t        reset_one;
  int                           auto_reset;
};
typedef struct triggerable_array triggerable_array;

void trigger_reset_all(triggerable_array *array);
void trigger_reset_one(triggerable_array *array, int index);

enum TRIGGER_ACTION_STATE trigger_test_all(triggerable_array *array);
enum TRIGGER_ACTION_STATE trigger_test_one(triggerable_array *array, int index);

#endif
