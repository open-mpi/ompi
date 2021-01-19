#ifndef PNBC_OSC_ACTION_PUT_H
#define PNBC_OSC_ACTION_PUT_H

#include "pnbc_osc_trigger_common.h"
#include "ompi/request/request.h"
#include "ompi/win/win.h"

struct put_args_t {
  const void *buf;
  int origin_count;
  MPI_Datatype origin_datatype;
  int target;
  MPI_Aint target_displ;
  int target_count;
  MPI_Datatype target_datatype;
  MPI_Win win;
  MPI_Request *request;
};
typedef struct put_args_t put_args_t;


//static enum TRIGGER_ACTION_STATE action_all_put(put_args_t *put_args);
extern trigger_action_all_cb_fn_t action_all_put_p;

//static enum TRIGGER_ACTION_STATE action_one_put(int index, put_args_t *put_args);
extern trigger_action_one_cb_fn_t action_one_put_p;

#endif
