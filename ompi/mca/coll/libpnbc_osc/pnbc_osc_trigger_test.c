#include <stdio.h>
#include "pnbc_osc_trigger_single.h"

int custom_test(FLAG_t *trigger, void* cbstate) {
  if (123 == *(int*)trigger)
    return !0;
  else
    return 0;
}

int main() {
  FLAG_t trigger = 555;
  triggerable_t mything;
  mything.trigger = &trigger;
  mything.test = &custom_test;
  mything.test = &triggered_all_bynonzero_int;
  mything.action = &action_all_noop;
  mything.reset = &reset_all_to_zero_int;
  mything.auto_reset = !0;
  trigger_reset(mything);

  enum TRIGGER_ACTION_STATE val;

  val = trigger_test(mything); // TRIGGER_PENDING
  printf("After first test (should be %d), val = %d\n", TRIGGER_PENDING, val);

  // some reomte action changes state of trigger
  trigger = 23;

  val = trigger_test(mything); // ACTION_SUCCESS
  printf("After second test (should be %d), val = %d\n", ACTION_SUCCESS, val);

  val = trigger_test(mything); // TRIGGER_PENDING
  printf("After third test (should be %d), val = %d\n", TRIGGER_PENDING, val);
}
