/* 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "communicator/communicator.h"

extern lam_list_t mca_coll_base_available;

/* 
 * Public variables
 */
char *mca_coll_base_override = NULL;

/*
 * Local variables
 */

static mca_coll_1_0_0_t lb_functions;

/*
 * Local functions
 */


#if 0
static int check_module_name(lam_communicator_t *comm, char *name);
#endif

static int check_all_modules(lam_communicator_t *comm);
static int check_module(lam_communicator_t *comm, mca_base_module_t *coll,
                        const mca_coll_1_0_0_t **actions);
static const mca_coll_1_0_0_t *
  query_1_0_0(mca_base_module_t *coll, lam_communicator_t *comm, int *priority);
static int init_module(mca_base_module_t *module, lam_communicator_t *comm, 
                       const mca_coll_1_0_0_t **actions);
static int init_1_0_0(lam_communicator_t *comm, 
                      const mca_coll_1_0_0_t **actions);
static void replace_null_with_lam_basic(mca_coll_1_0_0_t *sel_module);


/*
 * This function is called at the initialization time of every
 * communicator.  It is used to select which coll module will be
 * active for a given communicator.
 *
 * Note that this is not thread safe.  It's probably ok to just put a
 * lock around this function, though -- this is only called at the end
 * of a communicator constructor, so making a few threads wait while
 * we construct is probably ok. 
 */
int
mca_coll_base_init_comm(lam_communicator_t *comm)
{
  int i;
#if 0
  int found;
  char name[BUFSIZ];
  char *module_name;
#endif

  /* Announce */

#if 0
  if (mca_coll_verbose >= 10) {
    if (comm->c_name[0] != '\0')
      snprintf(name, sizeof(name), "%s (cid %d)", comm->c_name, 
	       comm->c_contextid);
    else
      snprintf(name, sizeof(name), "<no name> (cid %d)", comm->c_contextid);
    name[sizeof(name) - 1] = '\0';
    lam_debug(mca_coll_did, "init_comm: new communicator: %s", name);
  }

  /* WARNING: This will go away someday.  It is *only* here for a
     special case during MPI_Init of IMPI-enabled jobs.  It is not
     thread-safe, and will not be necessary when/if IMPI is
     re-architected.  So don't use it.  Ever. */

  if (mca_coll_base_override != NULL)
    i = check_module_name(comm, mca_coll_base_override);

  /* See if a specific module was requested by the magic keyval */

  else {

    MPI_Comm_get_attr(comm, LAM_MPI_SSI_COLL, &module_name, &found);
    if (found == 1)
      i = check_module_name(comm, module_name);
  
    /* Nope -- a specific one was not selected.  Go choose one. */
  
    else
#endif

	/* VPS: Right now only checking all modules, no user selective
	   check  */
	i = check_all_modules(comm);
    
#if 0
  }
#endif

  /* If we have no collective modules available, it's an error.
     Thanks for playing! */

  if (i == LAM_ERROR) {
#if 0
      if (mca_coll_verbose >= 10)
      lam_debug(mca_coll_did, "init_comm: No modules available!");
    show_help("ssi-coll", "none-available", NULL);
    return LAM_ERROR;
  }

  /* Otherwise, announce the winner */
  
  if (mca_coll_verbose > 0)
    lam_debug(mca_coll_did, "init_comm: Selected coll module %s", 
	      mca_coll_modules[i]->ssi_module_name);
  
#endif
  return LAM_ERROR;
  }
return 0;
}


#if 0
/*
 * A specific module was selected on the command line.  If a module by
 * that name is found, call its open (if it exists) and init
 * functions.  If they both return happiness, that module is selected.
 * Otherwise, call its finalize and close functions and return an
 * error (i.e., don't try to find any other available modules).
 */
static int
check_module_name(lam_communicator_t *comm, char *name)
{
  mca_module_t *module;
  mca_base_module_t *coll;
  const mca_coll_1_0_0_t *actions = NULL;

  /* Find the target module by its name */

  for (module = al_top(lam_ssi_coll_base_available); module != NULL;
       module = al_next(lam_ssi_coll_base_available, module)) {
    coll = module->lsm_module;
    if (strcmp(coll->ssi_module_name, name) == 0) {

      /* Found it.  Now check and see if that module wants to run */

      if (check_module(comm, coll, &actions) >= 0) {

	/* Otherwise, we have a winner.  Assign all the function
	   pointers in the comm to that module, and call its init
	   function. */
	
	comm->c_ssi_coll = *actions;
	actions = NULL;
        if (init_module(coll, comm, &actions) != 0)
	  return LAM_ERROR;
	if (actions != NULL)
	  comm->c_ssi_coll = *actions;

	/* Some logic is required if we decide to use lam_basic pointers 
	   for the NULL pointers */
        return 0;
      }

      /* We found the right module, but it didn't want to run.  Too
	 bad.  So sad. */

      break;
    }
  }

  return LAM_ERROR;
}

#endif


/*
 * Call open on all the available modules (if it exists).  If open
 * returns happiness (or doesn't exist), call the init function to see
 * if that module wants to run.  If it does, do the priority matchup
 * and remember the one with the best priority.  Fill the global
 * structs with the data from the winner.  Call finalize and close on
 * all the losers that we invoked initialize on.
 *
 * Return LAM_ERROR if there are no modules available.
 */
static int 
check_all_modules(lam_communicator_t *comm)
{
  int priority, best_priority = -1;
  mca_base_module_t *best_module;
  lam_list_item_t *module;
  const mca_coll_1_0_0_t *cur, *best = NULL;

  /* Call the query function in every collective module and see if
     they want to run on this communicator */

  for (best_priority = -1, 
	   module = lam_list_get_first(&mca_coll_base_available); 
       module != lam_list_get_end(&mca_coll_base_available);
       module = lam_list_get_next(module)) {
    priority = check_module(comm, 
			    ((mca_base_module_priority_list_item_t *)
			    module)->mpli_module,
			    &cur);
    
    if (priority > best_priority) {
      best_priority = priority;
      best_module = ((mca_base_module_priority_list_item_t *)
		     module)->mpli_module;
      best = cur;
    }
  }

  /* If we didn't find any available modules, return an error */

  if (best_priority == -1)
    return LAM_ERROR;

  /* Otherwise, we have a winner.  Assign all the function pointers in
     the comm to that module, and call its init function. */

  comm->c_coll = *best;
  best = NULL;
  if (init_module(best_module, comm, &best) != 0)
    return LAM_ERROR;

  if (best != NULL) 
    comm->c_coll = *best;

  /* Replace all the Non null collective functions by corresponding lam_basic
     ones */
  replace_null_with_lam_basic(&(comm->c_coll));

  return 0;
}


/*
 * Check a single module
 */
static int 
check_module(lam_communicator_t *comm, mca_base_module_t *coll,
             const mca_coll_1_0_0_t **actions)
{
  int priority = -1;

  if (coll->mca_major_version == 1 &&
      coll->mca_minor_version == 0 &&
      coll->mca_release_version == 0)
    *actions = query_1_0_0(coll, comm, &priority);

  if (*actions != NULL) {
    
    /* If the module is basic then store the pointers so that you can
       replace NULL pointers by lam_basic ones */
    if (strcmp(coll->mca_module_name, "basic") == 0) {
      lb_functions = **actions;
    }
    priority = (priority < 100) ? priority : 100;

    /* VPS: add after debug streams done  */
#if 0
    if (lam_ssi_coll_verbose >= 10)
	lam_debug(lam_ssi_coll_did, 
		"init_comm: module available: %s, priority: %d", 
		  coll->ssi_module_name, priority);
    fprintf(stderr, "init_comm: module available: %s, priority: %d", 
	    coll->mca_module_name, priority);
#endif

  } else {
#if 0
    if (lam_ssi_coll_verbose >= 10)
      lam_debug(lam_ssi_coll_did, 
		"init_comm: module not available: %s, priority: %d", 
		coll->ssi_module_name, priority);
    fprintf(stderr, "init_comm: module not available: %s, priority: %d", 
	    coll->mca_module_name, priority);
#endif
  }

  return priority;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Query a 1.0.0 module and convert its returned actions struct to a
 * 1.1.0 actions struct.
 */
static const mca_coll_1_0_0_t *
query_1_0_0(mca_base_module_t *coll, lam_communicator_t *comm, int *priority) 
{
  mca_coll_base_module_1_0_0_t *coll100 = (mca_coll_base_module_1_0_0_t *)coll;

  return coll100->collm_comm_query(comm, priority);
}


/**************************************************************************
 * Init functions
 **************************************************************************/

/*
 * Initialize a module
 */
static int
init_module(mca_base_module_t *module, lam_communicator_t *comm, 
            const mca_coll_1_0_0_t **actions)
{
  if (module->mca_major_version == 1 &&
      module->mca_minor_version == 0 &&
      module->mca_release_version == 0)
    return init_1_0_0(comm, actions);
  else
    return LAM_ERROR;
}


/*
 * Initialize a 1.0.0 module, and if it returns a new actions struct,
 * convert it before returning.
 */
static int 
init_1_0_0(lam_communicator_t *comm, const mca_coll_1_0_0_t **actions)
{
  const mca_coll_1_0_0_t *actions100;
  mca_coll_base_init_1_0_0_fn_t init100;

  /* The lsca_init function is actually the 1.0.0 function, but it was
     cast to be the 1.1.0 type in the convert() function (below) in
     order to make the assignment properly.  So cast it back to the
     1.0.0 type and invoke it. */

  init100 = (mca_coll_base_init_1_0_0_fn_t) comm->c_coll.coll_init;

  if (init100(comm, &actions100) != 0)
    return LAM_ERROR;

  return 0;
}


/* 
 * Replace the NULL pointers by corresponsing lam_basic pointers 
 */

static void 
replace_null_with_lam_basic(mca_coll_1_0_0_t *selected_module) {
  if (selected_module->coll_allgather_intra == NULL)
    selected_module->coll_allgather_intra = lb_functions.coll_allgather_intra;
  if (selected_module->coll_allgatherv_intra == NULL)
    selected_module->coll_allgatherv_intra = 
	lb_functions.coll_allgatherv_intra;
  if (selected_module->coll_allreduce_intra == NULL)
    selected_module->coll_allreduce_intra = lb_functions.coll_allreduce_intra;
  if (selected_module->coll_alltoall_intra == NULL)
    selected_module->coll_alltoall_intra = lb_functions.coll_alltoall_intra;
  if (selected_module->coll_alltoallv_intra == NULL)
    selected_module->coll_alltoallv_intra = lb_functions.coll_alltoallv_intra;
  if (selected_module->coll_alltoallw_intra == NULL)
    selected_module->coll_alltoallw_intra = lb_functions.coll_alltoallw_intra;
  if (selected_module->coll_barrier_intra == NULL)
    selected_module->coll_barrier_intra = lb_functions.coll_barrier_intra;
  if (selected_module->coll_bcast_intra == NULL)
    selected_module->coll_bcast_intra = lb_functions.coll_bcast_intra;
  if (selected_module->coll_exscan_intra == NULL)
    selected_module->coll_exscan_intra = lb_functions.coll_exscan_intra;
  if (selected_module->coll_gather_intra == NULL)
    selected_module->coll_gather_intra = lb_functions.coll_gather_intra;
  if (selected_module->coll_gatherv_intra == NULL)
    selected_module->coll_gatherv_intra = lb_functions.coll_gatherv_intra;
  if (selected_module->coll_reduce_intra == NULL)
    selected_module->coll_reduce_intra = lb_functions.coll_reduce_intra;
  if (selected_module->coll_reduce_scatter_intra == NULL)
    selected_module->coll_reduce_scatter_intra = 
      lb_functions.coll_reduce_scatter_intra;
  if (selected_module->coll_scan_intra == NULL)
    selected_module->coll_scan_intra = lb_functions.coll_scan_intra;
  if (selected_module->coll_scatter_intra == NULL)
    selected_module->coll_scatter_intra = lb_functions.coll_scatter_intra;
  if (selected_module->coll_scatterv_intra == NULL)
    selected_module->coll_scatterv_intra = lb_functions.coll_scatterv_intra;
}
