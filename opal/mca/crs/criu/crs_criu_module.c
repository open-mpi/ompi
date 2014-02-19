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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

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
    snapshot->super.component_name = strdup(mca_crs_criu_component.super.base_version.mca_component_name);
}

void opal_crs_criu_destruct(opal_crs_criu_snapshot_t *snapshot)
{
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

static void criu_error(int ret, pid_t pid)
{
    switch (ret) {
    case -EBADE:
        opal_output(0, "crs:criu:(PID:%d):RPC has returned fail", pid);
        break;
    case -ECONNREFUSED:
        opal_output(0, "crs:criu:(PID:%d):Unable to connect to CRIU", pid);
        break;
    case -ECOMM:
        opal_output(0, "crs:criu:(PID:%d):Unable to send/recv msg to/from CRIU", pid);
        break;
    case -EINVAL:
        opal_output(0, "crs:criu:(PID:%d):CRIU doesn't support this type of request."
                    "You should probably update CRIU", pid);
        break;
    case -EBADMSG:
        opal_output(0, "crs:criu:(PID:%d):Unexpected response from CRIU."
                    "You should probably update CRIU", pid);
        break;
    default:
        opal_output(0, "crs:criu:(PID:%d):Unknown error type code."
                    "You should probably update CRIU", pid);
    }
}

int opal_crs_criu_checkpoint(pid_t pid, opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    int ret;
    int fd = 0;
    int oh = mca_crs_criu_component.super.output_handle;
    opal_crs_criu_snapshot_t *snapshot = NULL;
    char *dest = NULL;

    opal_output_verbose(10, oh, "crs:criu: checkpoint(%d, ---)", pid);

    snapshot = (opal_crs_criu_snapshot_t *)base_snapshot;
    snapshot->super.component_name = strdup(mca_crs_criu_component.super.base_version.mca_component_name);

    if (NULL == snapshot->super.metadata) {
        if (NULL == (snapshot->super.metadata = fopen(snapshot->super.metadata_filename, "a"))) {
            opal_output(oh, "crs:criu: checkpoint(): Error: Unable to open the file (%s)",
                        snapshot->super.metadata_filename);
            *state = OPAL_CRS_ERROR;
            goto cleanup;
        }
    }
    fprintf(snapshot->super.metadata, "%s%s\n", CRS_METADATA_COMP, snapshot->super.component_name);

    fclose(snapshot->super.metadata);
    snapshot->super.metadata = NULL;

    ret = criu_init_opts();

    if (ret < 0) {
        criu_error(ret, pid);
        *state = OPAL_CRS_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, oh, "crs:criu: criu_init_opts() returned %d", ret);

    dest = snapshot->super.snapshot_directory;
    opal_output_verbose(10, oh, "crs:criu: opening snapshot directory %s", dest);
    fd = open(dest, O_DIRECTORY);

    if (fd < 0) {
        *state = OPAL_CRS_ERROR;
        opal_output(oh, "crs:criu: checkpoint(): Error: Unable to open checkpoint "
                    "directory (%s) for pid (%d)", dest, pid);
        goto cleanup;
    }

    /* http://criu.org/C_API */
    criu_set_images_dir_fd(fd);
    criu_set_pid(pid);

    criu_set_log_file(mca_crs_criu_component.log_file);
    criu_set_log_level(mca_crs_criu_component.log_level);
    criu_set_tcp_established(mca_crs_criu_component.tcp_established);
    criu_set_shell_job(mca_crs_criu_component.shell_job);
    criu_set_ext_unix_sk(mca_crs_criu_component.ext_unix_sk);
    criu_set_leave_running(mca_crs_criu_component.leave_running);
    ret = criu_dump();

    if (ret < 0) {
        criu_error(ret, pid);
        *state = OPAL_CRS_ERROR;
        goto cleanup;
    }

    *state = OPAL_CRS_CONTINUE;

 cleanup:

    if (fd > 0) {
        close(fd);
    }

    if (OPAL_CRS_ERROR == *state) {
        return OPAL_ERROR;
    }
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
