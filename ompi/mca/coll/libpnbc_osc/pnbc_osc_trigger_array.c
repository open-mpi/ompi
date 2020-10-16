//#include <stdio.h>
#include "pnbc_osc_trigger_array.h"

void trigger_reset_one(triggerable_array *array, int index) {
  array->reset_one(index, &((*array->triggers)[index]));
}

void trigger_reset_all(triggerable_array *array) {
  for (int index=0;index<array->num_triggers;++index) {
    trigger_reset_one(array, index);
  }
  array->progress = 0;
//  printf("Progress reset to %d\n", array->progress);
}

enum TRIGGER_ACTION_STATE trigger_test_one(triggerable_array *array, int index) {
  enum TRIGGER_ACTION_STATE ret = TRIGGER_PENDING;
  if (array->triggered_one(index, &((*array->triggers)[index]))) {
    ret = array->action_one(index, array->action_one_cbstate);
    if (ACTION_SUCCESS == ret) {
      trigger_reset_one(array, index);
//      printf("Progress was %d ", array->progress);
      array->progress += 1;
//      printf("Progress is now %d\n", array->progress);
    }
  }
  return ret;
}

enum TRIGGER_ACTION_STATE trigger_test_all(triggerable_array *array) {
  enum TRIGGER_ACTION_STATE ret = TRIGGER_PENDING;
  for (int index=0;index<array->num_triggers;++index) {
    if (ACTION_PROBLEM == trigger_test_one(array, index))
      break;
  }
//  printf("Progress target is %d, progress is actually %d\n", array->num_triggers, array->progress);
  if (array->progress == array->num_triggers) {
    ret = array->action_all(array->action_all_cbstate);
    if (ACTION_SUCCESS == ret && array->auto_reset) {
      trigger_reset_all(array);
    }
  }
  return ret;
}

int triggered_one_bynonzero_int(int index, int *trigger) {
  return *trigger;
}
int triggered_one_byzero_int(int index, int *trigger) {
  return !(*trigger);
}
void reset_one_to_zero_int(int index, int *trigger) {
  *trigger = 0;
}
enum TRIGGER_ACTION_STATE action_one_noop(int index, void *cbstate) {
//  printf("ACTION for trigger %d\n", index);
  return ACTION_SUCCESS;
}

