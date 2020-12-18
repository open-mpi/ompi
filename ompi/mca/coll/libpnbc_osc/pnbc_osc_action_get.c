#include "pnbc_osc_action_get.h"

static enum TRIGGER_ACTION_STATE action_one_get(int index, get_args_t *get_args) {
  int ret = ACTION_SUCCESS;
  return ret;
}

trigger_action_one_cb_fn_t action_one_get_p = (trigger_action_one_cb_fn_t)action_one_get;

static enum TRIGGER_ACTION_STATE action_all_get(get_args_t *get_args) {
  int ret = ACTION_SUCCESS;
  return ret;
}

trigger_action_all_cb_fn_t action_all_get_p = (trigger_action_all_cb_fn_t)action_all_get;

