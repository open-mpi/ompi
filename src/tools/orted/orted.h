/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTED_H
#define ORTED_H

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
    bool probe;
    char* name;
    int bootproxy;
    ompi_mutex_t mutex;
    ompi_condition_t condition;
    bool exit_condition;
} orted_globals_t;

extern orted_globals_t orted_globals;

/*
 * Internal functions
 */
int orte_daemon_bootproxy(void);

/*
 * Version-related strings and functions
 */

/* extern const char *ver_full; */
/* extern const char *ver_major; */
/* extern const char *ver_minor; */
/* extern const char *ver_release; */
/* extern const char *ver_alpha; */
/* extern const char *ver_beta; */
/* extern const char *ver_svn; */

/* void do_version(bool want_all, ompi_cmd_line_t *cmd_line); */
/* void show_ompi_version(const char *scope); */

/*
 * Parameter/configuration-related functions
 */

/* extern char *param_all; */

/* extern char *path_prefix; */
/* extern char *path_bindir; */
/* extern char *path_libdir; */
/* extern char *path_incdir; */
/* extern char *path_pkglibdir; */
/* extern char *path_sysconfdir; */


#endif /* ORTED_H */
