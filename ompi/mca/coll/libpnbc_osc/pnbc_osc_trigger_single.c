#include "pnbc_osc_trigger_single.h"

void trigger_reset(triggerable_t thing) {
  thing.reset(thing.trigger);
}

enum TRIGGER_ACTION_STATE trigger_test(triggerable_t thing) {
  enum TRIGGER_ACTION_STATE ret = TRIGGER_PENDING;
  if (thing.triggered(thing.trigger)) {
    ret = thing.action(thing.action_cbstate);
    if (thing.auto_reset && ACTION_SUCCESS == ret)
      trigger_reset(thing);
  }
  return ret;
}

