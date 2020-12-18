#ifndef PNBC_OSC_ACTION_DECREMENT_H
#define PNBC_OSC_ACTION_DECREMENT_H

#include "pnbc_osc_trigger_common.h"

typedef int dec_args_t;

//static enum TRIGGER_ACTION_STATE action_all_decrement_int(dec_args_t *dec_args);
extern trigger_action_all_cb_fn_t action_all_decrement_int_p;

//static enum TRIGGER_ACTION_STATE action_one_decrement_int(int index, dec_args_t *dec_args);
extern trigger_action_one_cb_fn_t action_one_decrement_int_p;

#endif
