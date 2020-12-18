#include "pnbc_osc_trigger_common.h"

// some built-in tests resets and actions for 'all' in triggerable_array
// also usable with triggerable_single

int triggered_all_bynonzero_int(FLAG_t *trigger, void *cbstate) {
  return (*(int*)trigger);
}
int triggered_all_byzero_int(FLAG_t *trigger, void *cbstate) {
  return !(*(int*)trigger);
}

void reset_all_to_zero_int(FLAG_t *trigger, FLAG_t value) {
  *(int*)trigger = 0;
}

enum TRIGGER_ACTION_STATE action_all_noop(void *cbstate) {
  return ACTION_SUCCESS;
}

// some built-in tests resets and actions for 'one' in triggerable_array

int triggered_one_bynonzero_int(FLAG_t *trigger, int index, void *cbstate) {
  return *trigger;
}
int triggered_one_byzero_int(FLAG_t *trigger, int index, void* cbstate) {
  return !(*trigger);
}
void reset_one_to_zero_int(FLAG_t *trigger, int index, FLAG_t value) {
  *trigger = 0;
}
enum TRIGGER_ACTION_STATE action_one_noop(int index, void *cbstate) {
//  printf("ACTION for trigger %d\n", index);
  return ACTION_SUCCESS;
}

