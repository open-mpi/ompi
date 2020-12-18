#include <stdio.h>
#include "pnbc_osc_trigger_array.h"
#include "pnbc_osc_action_put.h"

int custom_test_one(int *trigger, int index, void* cbstate) {
//  printf("In custom_test_one with index %d and *trigger %d ", index, *trigger);
  if ((100*index+23) == *trigger) {
    printf("TRIGGERED action %d\n", index);
    return !0;
  } else {
//    printf("PENDING\n");
    return 0;
  }
}

int main() {
  FLAG_t flags[2] ={555,666};
  triggerable_array myarray;
  myarray.num_triggers = 2;
  myarray.progress = 333;
  myarray.triggers = &flags;
  myarray.test_one = &custom_test_one;
  myarray.action_one = &action_one_noop;
  myarray.action_one = action_one_put_p;
  myarray.reset_one = &reset_one_to_zero_int;
  myarray.auto_reset = !0;
  myarray.action_all = &action_all_noop;

  enum TRIGGER_ACTION_STATE val;
  int errors = 0;

  val = trigger_test_all(&myarray); // GARBAGE
  if (TRIGGER_PENDING!=val) errors++;
  printf("Before reset (should be %d), val = %d\n", TRIGGER_PENDING, val);
  if (333!=myarray.progress) errors++;
  printf("Before reset (should be 333), progress = %d\n", myarray.progress);

  trigger_reset_all(&myarray);

  val = trigger_test_all(&myarray); // TRIGGER_PENDING
  if (TRIGGER_PENDING!=val) errors++;
  printf("After first test (should be %d), val = %d\n", TRIGGER_PENDING, val);
  if (0!=myarray.progress) errors++;
  printf("After first test (should be 0), progress = %d\n", myarray.progress);

  // some reomte action changes state of trigger
  flags[0] =  23;

  val = trigger_test_all(&myarray); // ACTION_SUCCESS
  if (TRIGGER_PENDING!=val) errors++;
  printf("After second test (should be %d), val = %d\n", TRIGGER_PENDING, val);
  if (1!=myarray.progress) errors++;
  printf("After second test (should be 1), progress = %d\n", myarray.progress);

  // some reomte action changes state of trigger
  flags[1] = 123;

  val = trigger_test_all(&myarray); // TRIGGER_PENDING
  if (ACTION_SUCCESS!=val) errors++;
  printf("After third test (should be %d), val = %d\n", ACTION_SUCCESS, val);
  if (0!=myarray.progress) errors++;
  printf("After third test (should be 0), progress = %d\n", myarray.progress);

  // some reomte action changes state of trigger
  flags[1] = 123;

  val = trigger_test_all(&myarray); // TRIGGER_PENDING
  if (TRIGGER_PENDING!=val) errors++;
  printf("After fourth test (should be %d), val = %d\n", TRIGGER_PENDING, val);
  if (1!=myarray.progress) errors++;
  printf("After fourth test (should be 1), progress = %d\n", myarray.progress);

  // some reomte action changes state of trigger
  flags[0] =  23;

  val = trigger_test_all(&myarray); // TRIGGER_PENDING
  if (ACTION_SUCCESS!=val) errors++;
  printf("After fifth test (should be %d), val = %d\n", ACTION_SUCCESS, val);
  if (0!=myarray.progress) errors++;
  printf("After fifth test (should be 0), progress = %d\n", myarray.progress);

  if (errors)
    printf("ERRORS: %d\n", errors);
  else
    printf("SUCCESS\n");
}
