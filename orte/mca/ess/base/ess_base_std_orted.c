/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/event/event.h"
#include "orte/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/mca/pstat/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/sysinfo/base/base.h"
#include "opal/util/os_path.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/notifier/base/base.h"
#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/db/base/base.h"
#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/ess/base/base.h"

/* local globals */
static bool plm_in_use=false;
static bool signals_set=false;
static opal_event_t term_handler;
static opal_event_t int_handler;
static opal_event_t epipe_handler;
#ifndef __WINDOWS__
static opal_event_t sigusr1_handler;
static opal_event_t sigusr2_handler;
#endif  /* __WINDOWS__ */
char *log_path = NULL;
static void shutdown_signal(int fd, short flags, void *arg);
static void signal_callback(int fd, short flags, void *arg);
static void epipe_signal_callback(int fd, short flags, void *arg);

int orte_ess_base_orted_setup(char **hosts)
{
    int ret;
    int fd;
    char log_file[PATH_MAX];
    char *jobidstring;
    char *error = NULL;
    char *plm_to_use;
    int value;

#ifndef __WINDOWS__
    /* setup callback for SIGPIPE */
    opal_event_signal_set(opal_event_base, &epipe_handler, SIGPIPE,
                          epipe_signal_callback, &epipe_handler);
    opal_event_signal_add(&epipe_handler, NULL);
    /* Set signal handlers to catch kill signals so we can properly clean up
     * after ourselves. 
     */
    opal_event_set(opal_event_base, &term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   shutdown_signal, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(opal_event_base, &int_handler, SIGINT, OPAL_EV_SIGNAL,
                   shutdown_signal, NULL);
    opal_event_add(&int_handler, NULL);

    /** setup callbacks for signals we should ignore */
    opal_event_signal_set(opal_event_base, &sigusr1_handler, SIGUSR1,
                          signal_callback, &sigusr1_handler);
    opal_event_signal_add(&sigusr1_handler, NULL);
    opal_event_signal_set(opal_event_base, &sigusr2_handler, SIGUSR2,
                          signal_callback, &sigusr2_handler);
    opal_event_signal_add(&sigusr2_handler, NULL);
#endif  /* __WINDOWS__ */

    signals_set = true;
    
    /* initialize the global list of local children and job data */
    OBJ_CONSTRUCT(&orte_local_children, opal_list_t);
    OBJ_CONSTRUCT(&orte_local_jobdata, opal_list_t);
    
    /* determine the topology info */
    if (0 == orte_default_num_sockets_per_board) {
        /* we weren't given a number, so try to determine it */
        if (OPAL_SUCCESS != opal_paffinity_base_get_socket_info(&value)) {
            /* can't get any info - default to 1 */
            value = 1;
        }
        orte_default_num_sockets_per_board = (uint8_t)value;
    }
    if (0 == orte_default_num_cores_per_socket) {
        /* we weren't given a number, so try to determine it */
        if (OPAL_SUCCESS != opal_paffinity_base_get_core_info(0, &value)) {
            /* don't have topo info - can we at least get #processors? */
            if (OPAL_SUCCESS != opal_paffinity_base_get_processor_info(&value)) {
                /* can't get any info - default to 1 */
                value = 1;
            }
        }
        orte_default_num_cores_per_socket = (uint8_t)value;
    }
    
    /* open and setup the opal_pstat framework so we can provide
     * process stats if requested
     */
    if (ORTE_SUCCESS != (ret = opal_pstat_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_pstat_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_pstat_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pstat_base_select";
        goto error;
    }
    
    /* open and setup the local resource discovery framework */
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_select";
        goto error;
    }
    
    /* some environments allow remote launches - e.g., ssh - so
     * open the PLM and select something -only- if we are given
     * a specific module to use
     */
    mca_base_param_reg_string_name("plm", NULL,
                                   "Which plm component to use (empty = none)",
                                   false, false,
                                   NULL, &plm_to_use);
    
    if (NULL == plm_to_use) {
        plm_in_use = false;
    } else {
        plm_in_use = true;
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_open";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_select";
            goto error;
        }
    }

    /* Setup the communication infrastructure */
    
    /* Runtime Messaging Layer - this opens/selects the OOB as well */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }

    /* Routed system */
    if (ORTE_SUCCESS != (ret = orte_routed_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_select";
        goto error;
    }

    /* multicast */
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_select";
        goto error;
    }
    
    /*
     * Group communications
     */
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_select";
        goto error;
    }
    
    /* Open/select the odls */
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
    /* enable communication with the rml */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.enable_comm";
        goto error;
    }
    
    /* set the communication function */
    orte_comm = orte_global_comm;
    
    /* open/select the errmgr */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }

    /* initialize the nidmaps */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    /* if we are using static ports, then we need to setup
     * the daemon info so the RML can function properly
     * without requiring a wireup stage. This must be done
     * after we enable_comm as that function determines our
     * own port, which we need in order to construct the nidmap
     */
    if (orte_static_ports) {
        if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_util_nidmap_init";
            goto error;
        }
        /* extract the node info from the environment and
         * build a nidmap from it
         */
        if (ORTE_SUCCESS != (ret = orte_util_build_daemon_nidmap(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "construct daemon map from static ports";
            goto error;
        }
    }
    /* be sure to update the routing tree so the initial "phone home"
     * to mpirun goes through the tree if static ports were enabled - still
     * need to do it anyway just to initialize things
     */
    if (ORTE_SUCCESS != (ret = orte_routed.update_routing_tree(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ret);
        error = "failed to update routing tree";
        goto error;
    }

    /* Now provide a chance for the PLM
     * to perform any module-specific init functions. This
     * needs to occur AFTER the communications are setup
     * as it may involve starting a non-blocking recv
     * Do this only if a specific PLM was given to us - the
     * orted has no need of the proxy PLM at all
     */
    if (plm_in_use) {
        if (ORTE_SUCCESS != (ret = orte_plm.init())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_init";
            goto error;
        }
    }
    
    /* setup my session directory */
    if (orte_create_session_dirs) {
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s setting up session dir with\n\ttmpdir: %s\n\thost %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == orte_process_info.tmpdir_base) ? "UNDEF" : orte_process_info.tmpdir_base,
                             orte_process_info.nodename));
        
        if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                    orte_process_info.tmpdir_base,
                                                    orte_process_info.nodename, NULL,
                                                    ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_session_dir";
            goto error;
        }
        /* Once the session directory location has been established, set
           the opal_output env file location to be in the
           proc-specific session directory. */
        opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                         "output-", NULL, NULL);

        /* setup stdout/stderr */
        if (orte_debug_daemons_file_flag) {
            /* if we are debugging to a file, then send stdout/stderr to
             * the orted log file
             */

            /* get my jobid */
            if (ORTE_SUCCESS != (ret = orte_util_convert_jobid_to_string(&jobidstring,
                                                                         ORTE_PROC_MY_NAME->jobid))) {
                ORTE_ERROR_LOG(ret);
                error = "convert_jobid";
                goto error;
            }

            /* define a log file name in the session directory */
            snprintf(log_file, PATH_MAX, "output-orted-%s-%s.log",
                     jobidstring, orte_process_info.nodename);
            log_path = opal_os_path(false,
                                    orte_process_info.tmpdir_base,
                                    orte_process_info.top_session_dir,
                                    log_file,
                                    NULL);

            fd = open(log_path, O_RDWR|O_CREAT|O_TRUNC, 0640);
            if (fd < 0) {
                /* couldn't open the file for some reason, so
                 * just connect everything to /dev/null
                 */
                fd = open("/dev/null", O_RDWR|O_CREAT|O_TRUNC, 0666);
            } else {
                dup2(fd, STDOUT_FILENO);
                dup2(fd, STDERR_FILENO);
                if(fd != STDOUT_FILENO && fd != STDERR_FILENO) {
                    close(fd);
                }
            }
        }
    }
    
    /* setup the routed info - the selected routed component
     * will know what to do. 
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed.init_routes";
        goto error;
    }
    
    /* setup I/O forwarding system - must come after we init routes */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
    /* setup the FileM */
    if (ORTE_SUCCESS != (ret = orte_filem_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_filem_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_select";
        goto error;
    }
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = orte_snapc_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(ORTE_PROC_IS_HNP, !ORTE_PROC_IS_DAEMON))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }
    
    /* For daemons, ORTE doesn't need the OPAL CR stuff */
    opal_cr_set_enabled(false);
#else
    opal_cr_set_enabled(false);
#endif
    
    /*
     * Initalize the CR setup
     * Note: Always do this, even in non-FT builds.
     * If we don't some user level tools may hang.
     */
    if (ORTE_SUCCESS != (ret = orte_cr_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_cr_init";
        goto error;
    }
    
    /* setup the notifier system */
    if (ORTE_SUCCESS != (ret = orte_notifier_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_notifier_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_select";
        goto error;
    }
    
    /* setup the db framework */
    if (ORTE_SUCCESS != (ret = orte_db_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_db_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_db_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_db_select";
        goto error;
    }
    
    /* setup the SENSOR framework */
    if (ORTE_SUCCESS != (ret = orte_sensor_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sensor_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_sensor_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "ortesensor_select";
        goto error;
    }
    /* start the local sensors */
    orte_sensor.start(ORTE_PROC_MY_NAME->jobid);

    /* Execute the post-startup errmgr code */
    if (ORTE_SUCCESS != (ret = orte_errmgr.post_startup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr.post_startup";
        goto error;
    }

    return ORTE_SUCCESS;
    
 error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}

int orte_ess_base_orted_finalize(void)
{
    orte_errmgr.pre_shutdown();

    /* stop the local sensors */
    orte_sensor.stop(ORTE_PROC_MY_NAME->jobid);

    if (signals_set) {
        /* Release all local signal handlers */
        opal_event_del(&epipe_handler);
        opal_event_del(&term_handler);
        opal_event_del(&int_handler);
#ifndef __WINDOWS__
        opal_event_signal_del(&sigusr1_handler);
        opal_event_signal_del(&sigusr2_handler);
#endif  /* __WINDOWS__ */
    }

    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }
    
    /* make sure our local procs are dead */
    orte_odls.kill_local_procs(NULL);
    
    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    orte_sensor_base_close();
    orte_db_base_close();
    orte_notifier_base_close();
    
    orte_cr_finalize();
    
#if OPAL_ENABLE_FT_CR == 1
    orte_snapc_base_close();
#endif
    orte_filem_base_close();
    
    orte_odls_base_close();
    
    orte_wait_finalize();
    orte_iof_base_close();

    /* finalize selected modules */
    if (plm_in_use) {
        orte_plm_base_close();
    }
    
    /* now can close the rml and its friendly group comm */
    orte_grpcomm_base_close();
    /* close the multicast */
    orte_rmcast_base_close();
    orte_routed_base_close();
    orte_rml_base_close();

    /* cleanup any lingering session directories */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* handle the orted-specific OPAL stuff */
    opal_sysinfo_base_close();
    opal_pstat_base_close();
    
    return ORTE_SUCCESS;    
}

static void shutdown_signal(int fd, short flags, void *arg)
{
    /* trigger the call to shutdown callback to protect
     * against race conditions - the trigger event will
     * check the one-time lock
     */
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
    orte_quit();
}

/**
 * Deal with sigpipe errors
 */
static void epipe_signal_callback(int fd, short flags, void *arg)
{
    /* for now, we just announce and ignore them */
    opal_output(0, "%s reports a SIGPIPE error on fd %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd);
    return;
}

static void signal_callback(int fd, short event, void *arg)
{
    /* just ignore these signals */
}
