/* -*- C -*-
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes
 * within the universe. Each universe will have one name server
 * running within the seed daemon.  This is done to prevent the
 * inadvertent duplication of names.
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "include/constants.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_base_component_t mca_ns_replica_module = {
  {
    MCA_NS_BASE_VERSION_1_0_0,

    "replica", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ns_replica_open,  /* module open */
    mca_ns_replica_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_ns_replica_init,    /* module init */
  mca_ns_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_ns_t mca_ns_replica = {
    ns_replica_create_cellid,
    ns_replica_create_jobid,
    ns_base_create_process_name,
    ns_replica_reserve_range,
    ns_replica_free_name,
    ns_base_get_proc_name_string,
    ns_base_get_vpid_string,
    ns_base_get_jobid_string,
    ns_base_get_cellid_string,
    ns_base_get_vpid,
    ns_base_get_jobid,
    ns_base_get_cellid,
    ns_base_compare
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;


/* constructor - used to initialize state of name_tracker instance */
static void ompi_name_tracker_construct(mca_ns_replica_name_tracker_t* name_tracker)
{
    name_tracker->job = 0;
    name_tracker->last_used_vpid = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_name_tracker_destructor(mca_ns_replica_name_tracker_t* name_tracker)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_ns_replica_name_tracker_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_name_tracker_construct, /* constructor */
		   ompi_name_tracker_destructor); /* destructor */

/*
 * globals needed within replica component
 */
ompi_process_id_t mca_ns_replica_last_used_cellid;
ompi_process_id_t mca_ns_replica_last_used_jobid;
ompi_list_t mca_ns_replica_name_tracker;

/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_ns_replica_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int mca_ns_replica_close(void)
{
    return OMPI_SUCCESS;
}

mca_ns_t* mca_ns_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    /* If we're the seed, then we want to be selected, so do all the
       setup and return the module */

    if (ompi_process_info.seed) {

      mca_ns_replica_last_used_cellid = 0;
      mca_ns_replica_last_used_jobid = 0;

      /* Return a module (choose an arbitrary, positive priority --
         it's only relevant compared to other ns components).  If
         we're not the seed, then we don't want to be selected, so
         return NULL. */

      *priority = 50;

      /* We allow multi user threads but don't have any hidden threads */

      *allow_multi_user_threads = true;
      *have_hidden_threads = false;

      /* initialize the name tracker */

      OBJ_CONSTRUCT(&mca_ns_replica_name_tracker, ompi_list_t);

      /* Return the module */

      initialized = true;
      return &mca_ns_replica;
    } else {
      return NULL;
    }
}

/*
 * finalize routine
 */
int mca_ns_replica_finalize(void)
{
  /* free all tracking storage, but only if this component was initialized */

  if (initialized) {
    OBJ_DESTRUCT(&mca_ns_replica_name_tracker);

    initialized = false;
  }

  /* All done */

  return OMPI_SUCCESS;
}

mca_oob_callback_fn_t mca_ns_replica_recv(int status, const ompi_process_name_t *sender,
					  const struct iovec *msg, size_t count,
					  void *cbdata)
{
    ompi_ns_msg_buffer_t *cmd, answer;
    ompi_process_id_t tmp1;
    struct iovec reply;
    int i;

    for (i=0; i<count; i++) {  /* loop through all included commands */
	cmd = (ompi_ns_msg_buffer_t*)msg->iov_base;
	if (OMPI_NS_CREATE_CELLID == cmd->command) { /* got create_cellid command */
	    tmp1 = ompi_name_server.create_cellid();
	    answer.command = cmd->command;
	    answer.buflen = sizeof(tmp1);
	    answer.buf = (uint8_t*)&tmp1;

	    reply.iov_base = (char*)&answer;
	    reply.iov_len = sizeof(answer);
	    mca_oob_send(sender, &reply, 1, 0);
	}
    }
    return OMPI_SUCCESS;
}
