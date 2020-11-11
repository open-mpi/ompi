#ifndef PNBC_OSC_ACTION_PUT_H
#define PNBC_OSC_ACTION_PUT_H

#include "pnbc_osc_trigger_array.h"

struct put_args_t {
};
typedef struct put_args_t put_args_t;

enum TRIGGER_ACTION_STATE action_put(int index, put_args_t *put_args);
extern trigger_action_one_cb_fn_t action_put_p;

#endif
