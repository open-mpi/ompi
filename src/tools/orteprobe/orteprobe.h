/*
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
 */

#ifndef ORTEPROBE_H
#define ORTEPROBE_H

#include "orte_config.h"

#include <string.h>

#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "threads/condition.h"

#include "util/cmd_line.h"
#include "mca/mca.h"

/*
 * Definitions needed for communication
 */
#define ORTE_DAEMON_CMD        ORTE_INT16

#define ORTE_DAEMON_HOSTFILE_CMD        0x01
#define ORTE_DAEMON_SCRIPTFILE_CMD      0x02
#define ORTE_DAEMON_CONTACT_QUERY_CMD   0x03
#define ORTE_DAEMON_HEARTBEAT_CMD       0xfe
#define ORTE_DAEMON_EXIT_CMD            0xff


/*
 * Globals
 */

typedef uint16_t orte_daemon_cmd_flag_t;

typedef struct {
    bool help;
    bool version;
    bool debug;
    char* name_string;
    char* requestor_string;
    ompi_mutex_t mutex;
    ompi_condition_t condition;
    bool exit_condition;
} orteprobe_globals_t;

extern orteprobe_globals_t orteprobe_globals;

#endif /* ORTEPROBE_H */
