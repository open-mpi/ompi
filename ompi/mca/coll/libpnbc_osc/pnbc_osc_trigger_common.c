#include "pnbc_osc_trigger_common.h"

int triggered_all_bynonzero_int(FLAG_t *trigger) {
  return (*(int*)trigger);
}
int triggered_all_byzero_int(FLAG_t *trigger) {
  return !(*(int*)trigger);
}

void reset_all_to_zero_int(FLAG_t *trigger) {
  *(int*)trigger = 0;
}

enum TRIGGER_ACTION_STATE action_all_noop(void *cbstate) {
  return ACTION_SUCCESS;
}

