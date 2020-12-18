#ifndef PNBC_OSC_ACTION_PUT_H
#define PNBC_OSC_ACTION_PUT_H

#include "pnbc_osc_trigger_common.h"

struct put_args_t {
};
typedef struct put_args_t put_args_t;

enum TRIGGER_ACTION_STATE action_all_put(put_args_t *put_args);
extern trigger_action_all_cb_fn_t action_all_put_p;

enum TRIGGER_ACTION_STATE action_one_put(int index, put_args_t *put_args);
extern trigger_action_one_cb_fn_t action_one_put_p;

#endif
