#include "pnbc_osc_action_put.h"

enum TRIGGER_ACTION_STATE action_put(int index, put_args_t *put_args) {
  int ret = ACTION_SUCCESS;
  return ret;
}

trigger_action_one_cb_fn_t action_put_p = (trigger_action_one_cb_fn_t)action_put;

