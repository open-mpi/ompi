#ifndef PNBC_OSC_TRIGGER_SINGLE_H
#define PNBC_OSC_TRIGGER_SINGLE_H

#include "pnbc_osc_trigger_common.h"

struct triggerable_t {
  trigger_triggered_all_fn_t  triggered;
  FLAG_t                     *trigger;
  trigger_action_all_cb_fn_t  action;
  void                       *action_cbstate;
  trigger_reset_all_fn_t      reset;
  int                         auto_reset;
};
typedef struct triggerable_t triggerable_t;

void trigger_reset(triggerable_t thing);

enum TRIGGER_ACTION_STATE trigger_test(triggerable_t thing);

#endif
