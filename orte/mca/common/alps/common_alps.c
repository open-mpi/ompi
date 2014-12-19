/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal/types.h"

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/mca/common/alps/common_alps.h"

#include <stdio.h>
#include <unistd.h>
#include <sys/syscall.h>


/*
 * determine whether or not calling process is in a Cray PAGG container
 */

int orte_common_alps_proc_in_pagg(bool *flag)
{
    int rc = ORTE_SUCCESS;
    const char proc_job_file[]="/proc/job";
    FILE *fd = NULL, *fd_task_is_app = NULL;
    char task_is_app_fname[PATH_MAX];

    if (flag == NULL) {
        return ORTE_ERR_BAD_PARAM;
    }

    fd = fopen(proc_job_file, "r");
    if (fd == NULL) {
        *flag = 0;
    } else {
        snprintf(task_is_app_fname,sizeof(task_is_app_fname),
                 "/proc/self/task/%ld/task_is_app",syscall(SYS_gettid));
        fd_task_is_app = fopen(task_is_app_fname, "r");
        if (fd_task_is_app != NULL) {   /* okay we're in a PAGG container, 
                                           and we are an app task (not just a process
                                           running on a mom node, for example), */
            *flag = 1;
            fclose(fd_task_is_app);
        } else {
            *flag = 0;
        }
        fclose(fd);
    }

    return rc;
}

