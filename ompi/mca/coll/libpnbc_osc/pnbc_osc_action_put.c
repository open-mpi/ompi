#include "pnbc_osc_debug.h"
#include "pnbc_osc_action_put.h"

static enum TRIGGER_ACTION_STATE action_all_put(put_args_t *put_args){
  int ret = ACTION_SUCCESS;

PNBC_OSC_DEBUG(5,"*buf: %p, origin count: %i, origin type: %p, target: %i, target count: %i, target type: %p, target displ: %lu)\n",
                     put_args->buf, put_args->origin_count, put_args->origin_datatype, put_args->target,
                     put_args->target_count, put_args->target_datatype, put_args->target_displ);

#ifdef PNBC_OSC_TIMING
      Iput_time -= MPI_Wtime();
#endif

  ret = put_args->win->w_osc_module->osc_rput(put_args->buf,
                                              put_args->origin_count, put_args->origin_datatype,
                                              put_args->target, put_args->target_displ,
                                              put_args->target_count, put_args->target_datatype,
                                              put_args->win, put_args->request);


  if (OMPI_SUCCESS != ret) {
    PNBC_OSC_Error("Error in osc_rput(%p, %i, %p, %i, %lu, %i, %p) (%i)",
                   put_args->buf, put_args->origin_count, 
		   put_args->origin_datatype, put_args->target, 
		   put_args->target_displ, put_args->target_count, 
		   put_args->target_datatype, ret);
  }


#ifdef PNBC_OSC_TIMING
      Iput_time += MPI_Wtime();
#endif



  return ret;
}

trigger_action_all_cb_fn_t action_all_put_p = (trigger_action_all_cb_fn_t)action_all_put;

static enum TRIGGER_ACTION_STATE action_one_put(put_args_t *put args) {
  return action_all_put(put_args);
}

trigger_action_one_cb_fn_t action_one_put_p = (trigger_action_one_cb_fn_t)action_one_put;

