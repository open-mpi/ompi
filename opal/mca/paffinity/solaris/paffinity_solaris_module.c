/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Solaris, where we are
   guaranteed to have these headers */
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/procset.h>
#include <unistd.h>
#include <sys/pset.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_solaris.h"

/*
 * Local functions
 */
static int solaris_module_init(void);
static int solaris_module_set(opal_paffinity_base_cpu_set_t cpuset);
static int solaris_module_get(opal_paffinity_base_cpu_set_t *cpuset);
static int solaris_module_finalize(void);
/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t module = {

    /* Initialization function */

    solaris_module_init,

    /* Module function pointers */

    solaris_module_set,
    solaris_module_get,
    opal_paffinity_base_map_to_processor_id,
    opal_paffinity_base_map_to_socket_core,
    opal_paffinity_base_max_processor_id,
    opal_paffinity_base_max_socket,
    opal_paffinity_base_max_core,
    solaris_module_finalize
};


const opal_paffinity_base_module_1_1_0_t *
opal_paffinity_solaris_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "solaris", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}

/*
   we are commenting out code in the solaris section - it is not clear
   from man pages exactly what we want. In particular the following global
   is not good programming practice, but it seems like it may be needed to
   be able to have a persistent pset under solaris

static psetid_t opal_pset;
*/

static int solaris_module_init(void)
{
    int ierr;

    /* We need to set up a processor set  
     * Actual setting/getting of processors will use pset_assign calls
     * A failure here will return an error ( one likely reason for failure 
     * is insufficient privilege to create a pset) */

    /* ierr = pset_create(&opal_pset);
    if(0 == ierr) {
        return OPAL_SUCCESS;
    } else {
        return OPAL_ERR_IN_ERRNO;
    } */
    return OPAL_SUCCESS;
}

/*
   Solaris_module_set will need to assign processors in the mask to a pset. This
   pset needs to be created (and persist until solaris_module_finalize
   is called - hence the global variable above). 

   there is some ambiguity in the available documentation as to
   the order in which pset_bind and pset_assign calls must be made.

   My reading is that the LWP must be bound to a pset (even an empty
   pset) before processors can be assigned. This doesn't make a lot of
   sense though, and it may be that the pset_assign calls need to be done
   prior to the pset_bind call.

   Documentation is located at http://docs.sun.com/app/docs/doc/819-2241/6n4huc7m7?a=view
   */

static int solaris_module_set(opal_paffinity_base_cpu_set_t mask)
{
/*
    int loopindex;
    psetid_t temp_pset;
*/
    /* Bind process to opal_pset  - have to bind to a pset before
     * assigning processors */
    /* 
    if (0 != pset_bind(PS_QUERY, P_PID, P_MYID, &temp_pset)) {
        return OPAL_ERR_IN_ERRNO;
    }
*/
    /* Assign all processors in mask to opal_pset */
/*
    for(loopindex=0;loopindex< sizeof(mask); loopindex++) {
        if(OPAL_PAFFINITY_CPU_ISSET(loopindex,mask)) {
            pset_assign(PS_MYID,(processor_t) loopindex, NULL);

        } 
    } */
    return OPAL_SUCCESS;
}

/*
   Solaris_module_get is a bit easier - we do a  pset_info call and then loop through
   the array of processor ids and set them in the mask structure

   */

static int solaris_module_get(opal_paffinity_base_cpu_set_t *mask) 
{
    /*
    processorid_t solar_cpulist;
    uint_t solar_numcpus;
    int loopindex, type; */
    /*
     * pset_info returns an array of processor_t elements for the
     * processors that are in this pset. There are solar_numcpus in the
     * array
     */
/*    if (0 != pset_info(temp_pset, &type, &solar_numcpus, &solar_cpulist)) {
        return OPAL_ERR_IN_ERRNO;
    }
    if (PS_NONE == type) { */
     /* do the right thing */
/*    }
    for (loopindex = 0; loopindex < solar_numcpus; loopindex++) {
        OPAL_PAFFINITY_CPU_SET((int) solar_cpulist[loopindex],*mask);
    } */
    return OPAL_SUCCESS;
}
static int solaris_module_finalize(void)
{
    /*
    int ierr;
    ierr = pset_destroy(opal_pset);
    if(0 == ierr) {
        return OPAL_SUCCESS;
    } else {
        return OPAL_ERR_IN_ERRNO;
    } */
    return OPAL_SUCCESS;
}

