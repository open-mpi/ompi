#include "pnbc_osc_trigger_single.h"

void trigger_reset(triggerable_t thing) {
  thing.reset(thing.trigger, thing.reset_value);
}

enum TRIGGER_ACTION_STATE trigger_test(triggerable_t thing) {
  enum TRIGGER_ACTION_STATE ret = TRIGGER_PENDING;
  if (thing.test(thing.trigger, thing.test_cbstate)) {
    ret = thing.action(thing.action_cbstate);
    if (thing.auto_reset && ACTION_SUCCESS == ret)
      trigger_reset(thing);
  }
  return ret;
}

enum TRIGGER_ACTION_STATE trigger_action(triggerable_t thing) {
  return thing.action(thing.action_cbstate);
}

