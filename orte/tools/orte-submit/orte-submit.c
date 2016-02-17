/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/orted/orted_submit.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

/*
 * Globals
 */
typedef struct {
    int status;
    volatile bool active;
    orte_job_t *jdata;
} orte_submit_status_t;

static void launched(int index, orte_job_t *jdata, int ret, void *cbdata);
static void completed(int index, orte_job_t *jdata, int ret, void *cbdata);


static opal_cmd_line_init_t cmd_line_init[] = {
    { "orte_execute_quiet", 'q', NULL, "quiet", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },

    { NULL, '\0', "report-pid", "report-pid", 1,
      &orte_cmd_line.report_pid, OPAL_CMD_LINE_TYPE_STRING,
      "Printout pid on stdout [-], stderr [+], or a file [anything else]" },
    { NULL, '\0', "report-uri", "report-uri", 1,
      &orte_cmd_line.report_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Printout URI on stdout [-], stderr [+], or a file [anything else]" },

    /* exit status reporting */
    { "orte_report_child_jobs_separately", '\0', "report-child-jobs-separately", "report-child-jobs-separately", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Return the exit status of the primary job only" },

    /* select XML output */
    { "orte_xml_output", '\0', "xml", "xml", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Provide all output in XML format" },
    { "orte_xml_file", '\0', "xml-file", "xml-file", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide all output in XML format to the specified file" },

    { "orte_xterm", '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there" },

    /* tell the dvm to terminate */
    { NULL, '\0', "terminate", "terminate", 0,
      &orte_cmd_line.terminate_dvm, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the DVM" },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char *argv[])
{
    int rc;
    orte_submit_status_t launchst, completest;
    opal_cmd_line_t cmd_line;

    orte_cmd_line.terminate_dvm = NULL;
    /* setup our cmd line */
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);

    /* initialize the RTE */
    if (ORTE_SUCCESS != (rc = orte_submit_init(argc, argv, &cmd_line))) {
        fprintf(stderr, "Init failed due to duplicate command options\n");
        exit(rc);
    }

    /* if this is the terminate command, just send it */
    if (orte_cmd_line.terminate_dvm) {
        rc = orte_submit_halt();
        /* just loop the event library - the errmgr
         * will exit us when the connection to our
         * HNP closes */
         while (1) {
            opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
         }
    }

    /* launch whatever job we were given */
    memset(&launchst, 0, sizeof(launchst));
    memset(&completest, 0, sizeof(completest));
    launchst.active = true;
    completest.active = true;
    if (ORTE_SUCCESS != (rc = orte_submit_job(argv, NULL,
                                              launched, &launchst,
                                              completed, &completest))) {
        if (ORTE_ERR_OP_IN_PROGRESS == rc) {
            /* terminate command was given */
            goto waiting;
        }
        opal_output(0, "JOB FAILED TO LAUNCH WITH ERROR %d:%s",
                    rc, ORTE_ERROR_NAME(rc));
        goto DONE;
    }

    // wait for response and unpack the status, jobid
    while (launchst.active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }
    if (orte_debug_flag) {
        opal_output(0, "Job %s has launched", ORTE_JOBID_PRINT(launchst.jdata->jobid));
    }
    if (ORTE_SUCCESS != launchst.status) {
        goto DONE;
    }

  waiting:
    while (completest.active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

  DONE:
    /* cleanup and leave */
    orte_submit_finalize();

    if (orte_debug_flag) {
        fprintf(stderr, "exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
}

static void launched(int index, orte_job_t *jdata, int ret, void *cbdata)
{
    orte_submit_status_t *launchst = (orte_submit_status_t*)cbdata;
    launchst->status = ret;
    ORTE_UPDATE_EXIT_STATUS(ret);
    OBJ_RETAIN(jdata);
    launchst->jdata = jdata;
    launchst->active = false;
}
static void completed(int index, orte_job_t *jdata, int ret, void *cbdata)
{
    orte_submit_status_t *completest = (orte_submit_status_t*)cbdata;
    completest->status = ret;
    ORTE_UPDATE_EXIT_STATUS(ret);
    OBJ_RETAIN(jdata);
    completest->jdata = jdata;
    completest->active = false;
}
