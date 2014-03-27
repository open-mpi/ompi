/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/dss/dss_internal.h"
#include "opal/class/opal_object.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "sbgp_basesmsocket.h"

#include "ompi/patterns/comm/coll_ops.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_basesmsocket_component_version_string =
    "Open MPI sbgp - basesmsocket collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int basesmsocket_register(void);
static int basesmsocket_open(void);
static int basesmsocket_close(void);
static mca_sbgp_base_module_t *mca_sbgp_basesmsocket_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in,
        struct ompi_communicator_t *comm,
        char *key,
        void *output_data
        );
static int mca_sbgp_basesmsocket_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);
/*----end local functions ----*/

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_sbgp_basesmsocket_component_t mca_sbgp_basesmsocket_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_SBGP_BASE_VERSION_2_0_0,

            /* Component name and version */

            "basesmsocket",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            basesmsocket_open,
            basesmsocket_close,
            NULL,
            basesmsocket_register
        },

    mca_sbgp_basesmsocket_init_query,
    mca_sbgp_basesmsocket_select_procs,

    /* (default) priority */
    0
    }

};

/*
 * Register the component
 */
static int basesmsocket_register(void)
{
    mca_sbgp_basesmsocket_component_t *cs = &mca_sbgp_basesmsocket_component;

    cs->super.priority = 90;
    (void) mca_base_component_var_register(&mca_sbgp_basesmsocket_component.super.sbgp_version,
                                           "priority", "Priority for the sbgp basesmsocket component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->super.priority);

    return OMPI_SUCCESS;
}

/*
 * Open the component
 */
static int basesmsocket_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
static int basesmsocket_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_sbgp_basesmsocket_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}

#if 0
/* NTH: this is no longer used but may be used if we can determine the binding policy*/
static int mca_sbgp_map_to_logical_socket_id(int *socket)
{
    int ret = OMPI_SUCCESS;
    hwloc_obj_t obj;
    hwloc_obj_t first_pu_object;
    hwloc_bitmap_t good;
    int pu_os_index = -1, my_logical_socket_id = -1;
    int this_pus_logical_socket_id = -1;

    *socket = my_logical_socket_id;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    good = hwloc_bitmap_alloc();
    if (NULL == good) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* get this process' CPU binding */
    if( 0 !=  hwloc_get_cpubind(opal_hwloc_topology,good, 0)){
        /* report some error */
        BASESMSOCKET_VERBOSE(10, "The global variable opal_hwloc_topology appears not to have been initialized\n");
        hwloc_bitmap_free(good);
        return OMPI_ERROR;
    }

    /* find the first logical PU object in the hwloc tree */
    first_pu_object = hwloc_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_PU, 0);


    /* get the next bit in the bitmap (note: if pu_os_index == -1, then the 
     * first bit is returned 
     */
     /* traverse the hwloc tree */
     while( -1 != (pu_os_index = hwloc_bitmap_next(good, pu_os_index) ) ) {
         /* Traverse all PUs in the machine in logical order, in the simple case 
          * there should only be a single PU that this process is bound to, right?
          *
          */
          for( obj = first_pu_object; obj != NULL; obj = obj->next_cousin ) {/* WTF is a "next_cousin" ? */ 
              /* is this PU the same as the bit I pulled off the mask? */
              if( obj->os_index == (unsigned int) pu_os_index) {
                  /* Then I found it, break out of for loop */
                  break;
              }
          }

          if( NULL != obj) {
              /* if we found the PU, then go upward in the tree
               * looking for the enclosing socket 
               */
               while( (NULL != obj) && ( HWLOC_OBJ_SOCKET != obj->type) ){
                   obj = obj->parent;
               }

               if( NULL == obj ) {
                   /* then we couldn't find an enclosing socket, report this */
               } else {
                   /* We found the enclosing socket */
                   if( -1 == my_logical_socket_id ){
                       /* this is the first PU that I'm bound to */
                       this_pus_logical_socket_id = obj->logical_index;
                       my_logical_socket_id = this_pus_logical_socket_id;
                   } else {
                       /* this is not the first PU that I'm bound to. 
                        * Seems I'm bound to more than a single PU. Question
                        * is, am I bound to the same socket?? 
                        */
                       /* in order to get rid of the compiler warning, I had to cast 
                        * "this_pus_logical_socket_id", at a glance this seems ok, 
                        * but if subgrouping problems arise, maybe look here. I shall 
                        * tag this line with the "mark of the beast" for grepability
                        * 666
                        */  
                        if( (unsigned int) this_pus_logical_socket_id != obj->logical_index ){
                            /* 666 */
                            /* Then we're bound to more than one socket...fail */
                            this_pus_logical_socket_id = -1;
                            my_logical_socket_id = -1;
                            break;
                        }
                   }
               }

          }

          /* end while */
     }
     *socket = my_logical_socket_id;
     hwloc_bitmap_free(good);

     return ret;

}
#endif

/* This routine is used to find the list of procs that run on the
** same host as the calling process.
*/

static mca_sbgp_base_module_t *mca_sbgp_basesmsocket_select_procs(struct ompi_proc_t ** procs,
    int n_procs_in,
    struct ompi_communicator_t *comm,
    char *key,
    void *output_data
    )
{
    /* local variables */
    mca_sbgp_basesmsocket_module_t *module;
    int proc, cnt, n_local_peers;

    /* initialize data */
    for (proc = 0, n_local_peers = 0 ; proc < n_procs_in ; ++proc) {
        if (OPAL_PROC_ON_LOCAL_SOCKET(procs[proc]->proc_flags)) {
	    n_local_peers++;
        }
    }

    /* we need to return a module even if there is only one local peer. this
     * covers the case where there may be a basesmsocket module on one rank
     * but not another */
    if (0 == n_local_peers) {
	return NULL;
    }

    /* create a new module */
    module = OBJ_NEW(mca_sbgp_basesmsocket_module_t);
    if (!module) {
        return NULL;
    }

    module->super.group_size = n_local_peers;
    module->super.group_comm = comm;
    module->super.group_list = NULL;
    module->super.group_net = OMPI_SBGP_SOCKET;

    /* allocate memory and fill in the group_list */
    module->super.group_list = (int *) calloc (n_local_peers, sizeof(int));
    if (NULL == module->super.group_list) {
	OBJ_RELEASE(module);
	return NULL;
    }

    for (proc = 0, cnt = 0 ; proc < n_procs_in ; ++proc) {
	if (OPAL_PROC_ON_LOCAL_SOCKET(procs[proc]->proc_flags)) {
	    module->super.group_list[cnt++] = proc;
	}
    }

    /* Return the module */
    return (mca_sbgp_base_module_t *) module;
}
