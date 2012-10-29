/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/mca/dfs/base/base.h"

#include "orte/mca/dfs/base/static-components.h"

/*
 * Globals
 */
opal_list_t orte_dfs_base_components_available;

orte_dfs_base_t orte_dfs_base;

orte_dfs_base_module_t orte_dfs = {
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_dfs_base_open(void)
{
    /* Only pass this way once */
    if( orte_dfs_base.initialized ) {
        return ORTE_SUCCESS;
    }

    orte_dfs_base.output = opal_output_open(NULL);

    /*
     * Open up all available components
     */
    if (ORTE_SUCCESS != 
        mca_base_components_open("dfs",
                                 orte_dfs_base.output,
                                 mca_dfs_base_static_components, 
                                 &orte_dfs_base.components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    orte_dfs_base.initialized = true;
    
    return ORTE_SUCCESS;
}


/* instantiate classes */
static void trk_con(orte_dfs_tracker_t *trk)
{
    trk->host_daemon.jobid = ORTE_JOBID_INVALID;
    trk->host_daemon.vpid = ORTE_VPID_INVALID;
    trk->filename = NULL;
    trk->location = 0;
}
static void trk_des(orte_dfs_tracker_t *trk)
{
    if (NULL != trk->filename) {
        free(trk->filename);
    }
}
OBJ_CLASS_INSTANCE(orte_dfs_tracker_t,
                   opal_list_item_t,
                   trk_con, trk_des);
static void req_const(orte_dfs_request_t *dfs)
{
    dfs->id = 0;
    dfs->uri = NULL;
    dfs->local_fd = -1;
    dfs->remote_fd = -1;
    dfs->read_length = -1;
    dfs->read_buffer = NULL;
    dfs->open_cbfunc = NULL;
    dfs->close_cbfunc = NULL;
    dfs->size_cbfunc = NULL;
    dfs->seek_cbfunc = NULL;
    dfs->read_cbfunc = NULL;
    dfs->post_cbfunc = NULL;
    dfs->fm_cbfunc = NULL;
    dfs->load_cbfunc = NULL;
    dfs->purge_cbfunc = NULL;
    dfs->cbdata = NULL;
}
static void req_dest(orte_dfs_request_t *dfs)
{
    if (NULL != dfs->uri) {
        free(dfs->uri);
    }
}
OBJ_CLASS_INSTANCE(orte_dfs_request_t,
                   opal_list_item_t,
                   req_const, req_dest);

static void jobfm_const(orte_dfs_jobfm_t *fm)
{
    OBJ_CONSTRUCT(&fm->maps, opal_list_t);
}
static void jobfm_dest(orte_dfs_jobfm_t *fm)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&fm->maps))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&fm->maps);
}
OBJ_CLASS_INSTANCE(orte_dfs_jobfm_t,
                   opal_list_item_t,
                   jobfm_const, jobfm_dest);

static void vpidfm_const(orte_dfs_vpidfm_t *fm)
{
    fm->fm.bytes = NULL;
    fm->fm.size = 0;
    fm->num_entries = 0;
}
static void vpidfm_dest(orte_dfs_vpidfm_t *fm)
{
    if (NULL != fm->fm.bytes) {
        free(fm->fm.bytes);
    }
}
OBJ_CLASS_INSTANCE(orte_dfs_vpidfm_t,
                   opal_list_item_t,
                   vpidfm_const, vpidfm_dest);
