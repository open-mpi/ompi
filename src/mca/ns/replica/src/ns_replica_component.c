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
#include "mca/base/mca_base_param.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_base_component_t mca_ns_replica_component = {
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
static mca_ns_base_module_t mca_ns_replica = {
    ns_replica_create_cellid,
    ns_base_assign_cellid_to_process,
    ns_replica_create_jobid,
    ns_base_create_process_name,
    ns_base_copy_process_name,
    ns_base_convert_string_to_process_name,
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
mca_ns_base_cellid_t mca_ns_replica_last_used_cellid;
mca_ns_base_jobid_t mca_ns_replica_last_used_jobid;
ompi_list_t mca_ns_replica_name_tracker;
int mca_ns_replica_debug;

/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_ns_replica_open(void)
{
    int id;

    id = mca_base_param_register_int("ns", "replica", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &mca_ns_replica_debug);

    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int mca_ns_replica_close(void)
{
    return OMPI_SUCCESS;
}

mca_ns_base_module_t* mca_ns_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    /* If we are to host a replica, then we want to be selected, so do all the
       setup and return the module */

    if (NULL == ompi_process_info.ns_replica) {

      int rc;
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

      /* issue non-blocking receive for call_back function */
      rc = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_NS, 0, mca_ns_replica_recv, NULL);
      if(rc != OMPI_SUCCESS && rc != OMPI_ERR_NOT_IMPLEMENTED) {
          ompi_output(0, "mca_ns_replica_init: unable to post non-blocking recv\n");
          return NULL;
      }

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
    if (mca_ns_replica_debug) {
	ompi_output(0, "finalizing ns replica");
    }

  /* free all tracking storage, but only if this component was initialized */

/*   if (initialized) { */
/*     OBJ_DESTRUCT(&mca_ns_replica_name_tracker); */

    initialized = false;
/*   } */

  /* All done */

  return OMPI_SUCCESS;
}


/* 
 * handle message from proxies
 */

void mca_ns_replica_recv(int status, ompi_process_name_t* sender,
			 ompi_buffer_t buffer, int tag,
			 void* cbdata)
{
    ompi_buffer_t answer, error_answer;
    mca_ns_cmd_flag_t command;
    mca_ns_base_cellid_t cell;
    mca_ns_base_jobid_t job;
    mca_ns_base_vpid_t vpid, range;

    if (OMPI_SUCCESS != ompi_unpack(buffer, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
    }

    if (MCA_NS_CREATE_CELLID_CMD == command) {   /* got a command to create a cellid */
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}
	cell = ompi_name_server.create_cellid();
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&cell, 1, MCA_NS_OOB_PACK_CELLID)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}
    } else if (MCA_NS_CREATE_JOBID_CMD == command) {   /* got command to create jobid */
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}
	job = ompi_name_server.create_jobid();
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&job, 1, MCA_NS_OOB_PACK_JOBID)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}
    } else if (MCA_NS_RESERVE_RANGE_CMD == command) {  /* got command to reserve vpid range */
	if (OMPI_SUCCESS != ompi_unpack(buffer, (void*)&job, 1, MCA_NS_OOB_PACK_JOBID)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, (void*)&range, 1, MCA_NS_OOB_PACK_VPID)) {
	    goto RETURN_ERROR;
	}

	vpid = ompi_name_server.reserve_range(job, range);
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&vpid, 1, MCA_NS_OOB_PACK_VPID)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}
    ompi_buffer_free(answer);
    } else {  /* got an unrecognized command */
    RETURN_ERROR:
	ompi_buffer_init(&error_answer, 8);
	command = MCA_NS_ERROR;
	ompi_pack(error_answer, (void*)&command, 1, MCA_NS_OOB_PACK_CMD);
	mca_oob_send_packed(sender, error_answer, tag, 0);
    ompi_buffer_free(error_answer);
    }

    /* reissue the non-blocking receive */
    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_NS, 0, mca_ns_replica_recv, NULL);
}

