/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/orte_constants.h"
#include "include/types.h"
#include "opal/class/opal_list.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pls/base/base.h"

#include <sys/bproc.h>
#include "pls_bproc_seed.h"

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_pls_bproc_seed_component_t mca_pls_bproc_seed_component = {
    {
        {
        ORTE_PLS_BASE_VERSION_1_0_0,

        "bproc_seed", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_pls_bproc_seed_component_open,  /* component open */
        orte_pls_bproc_seed_component_close /* component close */
        },
        {
        false /* checkpoint / restart */
        },
        orte_pls_bproc_seed_init    /* component init */ 
    }
};


int orte_pls_bproc_seed_component_open(void)
{
    int id;
    mca_base_component_t *c = &mca_pls_bproc_seed_component.super.pls_version;
    /* init globals */
    OBJ_CONSTRUCT(&mca_pls_bproc_seed_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_bproc_seed_component.condition, opal_condition_t);
    mca_pls_bproc_seed_component.num_children = 0;

    /* init parameters */
    mca_base_param_reg_int(c, "debug", NULL, false, false, 0, 
                           &mca_pls_bproc_seed_component.debug);
    mca_base_param_reg_int(c, "reap", NULL, false, false, 1, 
                           &mca_pls_bproc_seed_component.reap);
    mca_base_param_reg_int(c, "image_frag_size", NULL, false, false, 1*1024*1024,
                           (int *)&mca_pls_bproc_seed_component.image_frag_size);
    mca_base_param_reg_int(c, "priority", NULL, false, false, 75,
                           &mca_pls_bproc_seed_component.priority);
    mca_base_param_reg_int(c, "terminate_sig", NULL, false, false, 9,
                           &mca_pls_bproc_seed_component.terminate_sig);

    id = mca_base_param_find("nds", "pipe", "fd");
    if(id > 0) {
         mca_base_param_lookup_int(id, &mca_pls_bproc_seed_component.name_fd);
    } else {
         mca_pls_bproc_seed_component.name_fd = 3;
    }
    return ORTE_SUCCESS;
}


int orte_pls_bproc_seed_component_close(void)
{
    if(mca_pls_bproc_seed_component.reap) {
        OPAL_THREAD_LOCK(&mca_pls_bproc_seed_component.lock);
        while(mca_pls_bproc_seed_component.num_children > 0) {
            opal_condition_wait(&mca_pls_bproc_seed_component.condition, 
                &mca_pls_bproc_seed_component.lock);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_pls_bproc_seed_component.lock);
    return ORTE_SUCCESS;
}


orte_pls_base_module_t* orte_pls_bproc_seed_init(
    int *priority)
{
    int ret;
    struct bproc_version_t version;

 
    /* are we the seed */
    if(orte_process_info.seed == false)
        return NULL;

    /* okay, we are in a daemon - now check to see if BProc is running here */
    ret = bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }
    
    /* only launch from the master node */
    if (bproc_currnode() != BPROC_NODE_MASTER) {
        return NULL;
    }

    *priority = mca_pls_bproc_seed_component.priority;
    return &orte_pls_bproc_seed_module;
}

