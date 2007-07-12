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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTEPROBE_H
#define ORTEPROBE_H

#include "orte_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "opal/util/cmd_line.h"
#include "opal/mca/mca.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Globals
 */

typedef struct {
    bool help;
    bool verbose;
    bool debug;
    char* name_string;
    char* requestor_string;
    opal_mutex_t mutex;
    opal_condition_t condition;
    bool exit_condition;
} orteprobe_globals_t;

extern orteprobe_globals_t orteprobe_globals;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTEPROBE_H */
