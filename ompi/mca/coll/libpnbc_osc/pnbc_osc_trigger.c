#include "pnbc_osc_trigger.h"

void trigger_reset(triggerable_thing thing) {
  thing.reset(thing.trigger);
}

enum TRIGGER_ACTION_STATE trigger_test(triggerable_thing thing) {
  enum TRIGGER_ACTION_STATE ret = TRIGGER_PENDING;
  if (thing.triggered(thing.trigger)) {
    ret = thing.action(thing.action_cbstate);
    if (thing.auto_reset && ACTION_SUCCESS == ret)
      trigger_reset(thing);
  }
  return ret;
}

int triggered_bynonzero_int(void *trigger) {
  return (*(int*)trigger);
}
int triggered_byzero_int(void *trigger) {
  return !(*(int*)trigger);
}

void reset_to_zero_int(void *trigger) {
  *(int*)trigger = 0;
}

enum TRIGGER_ACTION_STATE action_noop(void *cbstate) {
  return ACTION_SUCCESS;
}

