/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * CRIU CRS component - support checkpoint/restart using CRIU
 */

#ifndef MCA_CRS_CRIU_EXPORT_H
#define MCA_CRS_CRIU_EXPORT_H

#include "opal_config.h"


#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/base/base.h"

#include <criu/criu.h>

BEGIN_C_DECLS

#define LOG_FILE ("criu.log")

/* Local Component structures */
struct opal_crs_criu_component_t {
    /* Base CRS component */
    opal_crs_base_component_t super;

    /* criu log file */
    char *log_file;
    /* criu log level */
    int log_level;
    /* criu tcp established */
    bool tcp_established;
    /* criu shell job */
    bool shell_job;
    /* criu external unix sockets */
    bool ext_unix_sk;
    /* criu leave tasks in running state after checkpoint */
    bool leave_running;
};
typedef struct opal_crs_criu_component_t opal_crs_criu_component_t;

OPAL_MODULE_DECLSPEC extern opal_crs_criu_component_t mca_crs_criu_component;

int opal_crs_criu_component_query(mca_base_module_t **module, int *priority);

/*
 * Module functions
 */
int opal_crs_criu_module_init(void);
int opal_crs_criu_module_finalize(void);
int opal_crs_criu_checkpoint(pid_t pid, opal_crs_base_snapshot_t *snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state);

int opal_crs_criu_restart(opal_crs_base_snapshot_t *snapshot,
                          bool spawn_child, pid_t *child_pid);

int opal_crs_criu_disable_checkpoint(void);
int opal_crs_criu_enable_checkpoint(void);

int opal_crs_criu_prelaunch(int32_t rank, char *base_snapshot_dir, char **app,
                            char **cwd, char ***argv, char ***env);

int opal_crs_criu_reg_thread(void);


END_C_DECLS

#endif /* MCA_CRS_CRIU_EXPORT_H */
