/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/constants.h"

#include "opal/mca/base/mca_base_var.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "crs_criu.h"

/* CRIU module */
static opal_crs_base_module_t criu_module = {
    /* Initialization Function */
    opal_crs_criu_module_init,
    /* Finalization Function */
    opal_crs_criu_module_finalize,

    /* Checkpoint interface */
    opal_crs_criu_checkpoint,

    /* Restart Command Access */
    opal_crs_criu_restart,

    /* Disable checkpoints */
    opal_crs_criu_disable_checkpoint,
    /* Enable checkpoints */
    opal_crs_criu_enable_checkpoint,

    /* Prelaunch */
    opal_crs_criu_prelaunch,

    /* Register Thread */
    opal_crs_criu_reg_thread
};

/* Snapshot Class Functions */
OBJ_CLASS_DECLARATION(opal_crs_criu_snapshot_t);

struct opal_crs_criu_snapshot_t {
    /* Base CRS snapshot type */
    opal_crs_base_snapshot_t super;
    char *context_filename;
};
typedef struct opal_crs_criu_snapshot_t opal_crs_criu_snapshot_t;

void opal_crs_criu_construct(opal_crs_criu_snapshot_t *obj);
void opal_crs_criu_destruct(opal_crs_criu_snapshot_t *obj);

OBJ_CLASS_INSTANCE(opal_crs_criu_snapshot_t,
                   opal_crs_base_snapshot_t,
                   opal_crs_criu_construct,
                   opal_crs_criu_destruct);

void opal_crs_criu_construct(opal_crs_criu_snapshot_t *snapshot)
{
    snapshot->context_filename = NULL;
    snapshot->super.component_name = strdup(mca_crs_criu_component.super.base_version.mca_component_name);
}

void opal_crs_criu_destruct(opal_crs_criu_snapshot_t *snapshot)
{
    if (NULL != snapshot->context_filename) {
        free(snapshot->context_filename);
        snapshot->context_filename = NULL;
    }
}

int opal_crs_criu_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: component_query()");

    *priority = mca_crs_criu_component.super.priority;
    *module = (mca_base_module_t *)&criu_module;

    return OPAL_SUCCESS;
}

int opal_crs_criu_module_init(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: module_init()");

    return OPAL_SUCCESS;
}

int opal_crs_criu_module_finalize(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: module_finalize()");

    return OPAL_SUCCESS;
}

int opal_crs_criu_checkpoint(pid_t pid, opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    int ret;
    opal_crs_criu_snapshot_t *snapshot = NULL;

    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: checkpoint(%d, ---)", pid);

    snapshot = (opal_crs_criu_snapshot_t *)base_snapshot;
    snapshot->super.component_name = strdup(mca_crs_criu_component.super.base_version.mca_component_name);

    ret = criu_init_opts();

    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: criu_init_opts() returned %d", ret);

    return OPAL_SUCCESS;
}

int opal_crs_criu_restart(opal_crs_base_snapshot_t *snapshot,
                          bool spawn_child, pid_t *child_pid)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: %s", __func__);
    return OPAL_SUCCESS;
}

int opal_crs_criu_disable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: %s", __func__);
    return OPAL_SUCCESS;
}

int opal_crs_criu_enable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: %s", __func__);
    return OPAL_SUCCESS;
}

int opal_crs_criu_prelaunch(int32_t rank, char *base_snapshot_dir,
                            char **app, char **cwd, char ***argv,
                            char ***env)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: %s", __func__);
    return OPAL_SUCCESS;
}

int opal_crs_criu_reg_thread(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: %s", __func__);
    return OPAL_SUCCESS;
}
