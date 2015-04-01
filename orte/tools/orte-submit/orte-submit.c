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
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/show_help.h"
#include "opal/util/fd.h"
#include "opal/sys/atomic.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_info_support.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmaps/base/base.h"

#include "orte/mca/schizo/schizo.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_quit.h"
#include "orte/util/show_help.h"

/*
 * Globals
 */
static char **global_mca_env = NULL;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;
volatile bool mywait = true;
volatile bool myspawn = true;

/*
 * Globals
 */
static struct {
    bool help;
    bool version;
    char *report_pid;
    char *stdin_target;
    bool index_argv;
    bool preload_binaries;
    char *preload_files;
    char *appfile;
    int num_procs;
    char *hnp;
    char *wdir;
    bool set_cwd_to_session_dir;
    char *path;
    bool enable_recovery;
    char *personality;
    char *basename;
    char *prefix;
    bool terminate;
    bool nolocal;
    bool no_oversubscribe;
    bool oversubscribe;
    int cpus_per_proc;
    bool pernode;
    int npernode;
    bool use_hwthreads_as_cpus;
    int npersocket;
    char *mapping_policy;
    char *ranking_policy;
    char *binding_policy;
    bool report_bindings;
    char *slot_list;
    bool debug;
    bool run_as_root;
} myglobals;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, 'h', NULL, "help", 0,
      &myglobals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, 'V', NULL, "version", 0,
      &myglobals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },

    { NULL, '\0', "report-pid", "report-pid", 1,
      &myglobals.report_pid, OPAL_CMD_LINE_TYPE_STRING,
      "Printout pid on stdout [-], stderr [+], or a file [anything else]" },
    
    /* select stdin option */
    { NULL, '\0', "stdin", "stdin", 1,
      &myglobals.stdin_target, OPAL_CMD_LINE_TYPE_STRING,
      "Specify procs to receive stdin [rank, all, none] (default: 0, indicating rank 0)" },
    
    /* request that argv[0] be indexed */
    { NULL, '\0', "index-argv-by-rank", "index-argv-by-rank", 0,
      &myglobals.index_argv, OPAL_CMD_LINE_TYPE_BOOL,
      "Uniquely index argv[0] for each process using its rank" },

    /* Preload the binary on the remote machine */
    { NULL, 's', NULL, "preload-binary", 0,
      &myglobals.preload_binaries, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process." },

    /* Preload files on the remote machine */
    { NULL, '\0', NULL, "preload-files", 1,
      &myglobals.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process." },

    /* Use an appfile */
    { NULL, '\0', NULL, "app", 1,
      &myglobals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, 'c', "np", "np", 1,
      &myglobals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, '\0', "n", "n", 1,
      &myglobals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    
    /* uri of Open MPI HNP, or at least where to get it */
    { NULL, '\0', "hnp", "hnp", 1,
      &myglobals.hnp, OPAL_CMD_LINE_TYPE_STRING,
      "Specify the URI of the Open MPI server, or the name of the file (specified as file:filename) that contains that info" },
    
    /* uri of Open MPI HNP, or at least where to get it */
    { NULL, '\0', "terminate", "terminate", 0,
      &myglobals.terminate, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the DVM" },
    

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

      /* Mapping controls */
    { NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },
    { NULL, '\0', "nolocal", "nolocal", 0,
      &myglobals.nolocal, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },
    { NULL, '\0', "nooversubscribe", "nooversubscribe", 0,
      &myglobals.no_oversubscribe, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { NULL, '\0', "oversubscribe", "oversubscribe", 0,
      &myglobals.oversubscribe, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are allowed to be oversubscribed, even on a managed system, and overloading of processing elements"},
    { NULL, '\0', "cpus-per-proc", "cpus-per-proc", 1,
      &myglobals.cpus_per_proc, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]" },

    /* Nperxxx options that do not require topology and are always
     * available - included for backwards compatibility
     */
    { NULL, '\0', "pernode", "pernode", 0,
      &myglobals.pernode, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node" },
    { NULL, '\0', "npernode", "npernode", 1,
      &myglobals.npernode, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per node on all allocated nodes" },
    { NULL, '\0', "N", NULL, 1,
      &myglobals.npernode, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes (synonym for npernode)" },

#if OPAL_HAVE_HWLOC
    /* declare hardware threads as independent cpus */
    { NULL, '\0', "use-hwthread-cpus", "use-hwthread-cpus", 0,
      &myglobals.use_hwthreads_as_cpus, OPAL_CMD_LINE_TYPE_BOOL,
      "Use hardware threads as independent cpus" },

    /* include npersocket for backwards compatibility */
    { NULL, '\0', "npersocket", "npersocket", 1,
      &myglobals.npersocket, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per socket on all allocated nodes" },

    /* Mapping options */
    { NULL, '\0', NULL, "map-by", 1,
      &myglobals.mapping_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping Policy [slot | hwthread | core | socket (default) | numa | board | node]" },

      /* Ranking options */
    { NULL, '\0', NULL, "rank-by", 1,
      &myglobals.ranking_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Ranking Policy [slot (default) | hwthread | core | socket | numa | board | node]" },

      /* Binding options */
    { NULL, '\0', NULL, "bind-to", 1,
      &myglobals.binding_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Policy for binding processes. Allowed values: none, hwthread, core, l1cache, l2cache, l3cache, socket, numa, board (\"none\" is the default when oversubscribed, \"core\" is the default when np<=2, and \"socket\" is the default when np>2). Allowed qualifiers: overload-allowed, if-supported" },

    { NULL, '\0', "report-bindings", "report-bindings", 0,
      &myglobals.report_bindings, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr" },

    /* slot list option */
    { NULL, '\0', "slot-list", "slot-list", 1,
      &myglobals.slot_list, OPAL_CMD_LINE_TYPE_STRING,
      "List of processor IDs to bind processes to [default=NULL]"},

#else
    /* Mapping options */
    { NULL, '\0', NULL, "map-by", 1,
      &myglobals.mapping_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping Policy [slot (default) | node]" },

      /* Ranking options */
    { NULL, '\0', NULL, "rank-by", 1,
      &myglobals.ranking_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Ranking Policy [slot (default) | node]" },
#endif

    /* mpiexec-like arguments */
    { NULL, '\0', "wdir", "wdir", 1,
      &myglobals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, '\0', "wd", "wd", 1,
      &myglobals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, '\0', "set-cwd-to-session-dir", "set-cwd-to-session-dir", 0,
      &myglobals.set_cwd_to_session_dir, OPAL_CMD_LINE_TYPE_BOOL,
      "Set the working directory of the started processes to their session directory" },
    { NULL, '\0', "path", "path", 1,
      &myglobals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },

    { NULL, '\0', "enable-recovery", "enable-recovery", 0,
      &myglobals.enable_recovery, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable recovery (resets all recovery options to on)" },

    { NULL, '\0', "personality", "personality", 1,
      &myglobals.personality, OPAL_CMD_LINE_TYPE_STRING,
      "Programming model/language being used (default=\"ompi\")" },

    { NULL, 'd', "debug-devel", "debug-devel", 0,
      &myglobals.debug, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { NULL, '\0', "allow-run-as-root", "allow-run-as-root", 0,
      &myglobals.run_as_root, OPAL_CMD_LINE_TYPE_BOOL,
      "Allow execution as root (STRONGLY DISCOURAGED)" },

/* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Local functions
 */
static int create_app(int argc, char* argv[],
                      orte_job_t *jdata,
                      orte_app_context_t **app,
                      bool *made_app, char ***app_env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line);
static int parse_locals(orte_job_t *jdata, int argc, char* argv[]);
static void set_classpath_jar_file(orte_app_context_t *app, int index, char *jarfile);
static int parse_appfile(orte_job_t *jdata, char *filename, char ***env);
static void orte_timeout_wakeup(int sd, short args, void *cbdata);
static void local_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata);
static void spawn_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata);


int main(int argc, char *argv[])
{
    int rc, i;
    opal_cmd_line_t cmd_line;
    char *param;
    orte_job_t *jdata=NULL;
    opal_buffer_t *req;
    orte_daemon_cmd_flag_t cmd = ORTE_DAEMON_SPAWN_JOB_CMD;
    
    /* Setup and parse the command line */
    memset(&myglobals, 0, sizeof(myglobals));
    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    myglobals.basename = opal_basename(argv[0]);

    
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (myglobals.version) {
        char *str;
        str = opal_info_make_version_str("all", 
                                         OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, 
                                         OPAL_RELEASE_VERSION, 
                                         OPAL_GREEK_VERSION,
                                         OPAL_REPO_REV);
        if (NULL != str) {
            fprintf(stdout, "%s %s\n\nReport bugs to %s\n",
                    myglobals.basename, str, PACKAGE_BUGREPORT);
            free(str);
        }
        exit(0);
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning flag
     */
    if (0 == geteuid() && !myglobals.run_as_root) {
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        if (myglobals.help) {
            fprintf(stderr, "%s cannot provide the help message when run as root\n", myglobals.basename);
        } else {
            /* show_help is not yet available, so print an error manually */
            fprintf(stderr, "%s has detected an attempt to run as root.\n", myglobals.basename);
        }
        fprintf(stderr, " This is *strongly* discouraged as any mistake (e.g., in defining TMPDIR) or bug can\n");
        fprintf(stderr, "result in catastrophic damage to the OS file system, leaving\n");
        fprintf(stderr, "your system in an unusable state.\n\n");
        fprintf(stderr, "You can override this protection by adding the --allow-run-as-root\n");
        fprintf(stderr, "option to your cmd line. However, we reiterate our strong advice\n");
        fprintf(stderr, "against doing so - please do so at your own risk.\n");
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        exit(1);
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    if (OPAL_SUCCESS != mca_base_cmd_line_process_args(&cmd_line, &environ, &environ)) {
        exit(1);
    }
    
    /* Ensure that enough of OPAL is setup for us to be able to run */
    /*
     * NOTE: (JJH)
     *  We need to allow 'mca_base_cmd_line_process_args()' to process command
     *  line arguments *before* calling opal_init_util() since the command
     *  line could contain MCA parameters that affect the way opal_init_util()
     *  functions. AMCA parameters are one such option normally received on the
     *  command line that affect the way opal_init_util() behaves.
     *  It is "safe" to call mca_base_cmd_line_process_args() before 
     *  opal_init_util() since mca_base_cmd_line_process_args() does *not*
     *  depend upon opal_init_util() functionality.
     */
    /* Need to initialize OPAL so that install_dirs are filled in */
    if (OPAL_SUCCESS != opal_init(&argc, &argv)) {
        exit(1);
    }
    
    /* Check for some "global" command line params */
    parse_globals(argc, argv, &cmd_line);
    
    /* if they didn't point us at an HNP, that's an error */
    if (NULL == myglobals.hnp) {
        fprintf(stderr, "orte-submit: required option --hnp not provided\n");
        exit(1);
    }
    OBJ_DESTRUCT(&cmd_line);

    if (0 == strncasecmp(myglobals.hnp, "file", strlen("file"))) {
        char input[1024], *filename;
        FILE *fp;
            
        /* it is a file - get the filename */
        filename = strchr(myglobals.hnp, ':');
        if (NULL == filename) {
            /* filename is not correctly formatted */
            orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "uri", myglobals.hnp);
            exit(1);
        }
        ++filename; /* space past the : */
            
        if (0 >= strlen(filename)) {
            /* they forgot to give us the name! */
            orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "uri", myglobals.hnp);
            exit(1);
        }
            
        /* open the file and extract the uri */
        fp = fopen(filename, "r");
        if (NULL == fp) { /* can't find or read file! */
            orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-access", true, myglobals.hnp);
            exit(1);
        }
        if (NULL == fgets(input, 1024, fp)) {
            /* something malformed about file */
            fclose(fp);
            orte_show_help("help-orte-top.txt", "orte-top:hnp-file-bad", true, myglobals.hnp);
            exit(1);
        }
        fclose(fp);
        input[strlen(input)-1] = '\0';  /* remove newline */
        /* construct the target hnp info */
        opal_setenv("OMPI_MCA_orte_hnp_uri", input, true, &environ);
    } else {
        /* should just be the uri itself - construct the target hnp info */
        opal_setenv("OMPI_MCA_orte_hnp_uri", myglobals.hnp, true, &environ);
    }

    /* Setup MCA params */
    orte_register_params();

    /* flag that I am a TOOL */
    orte_process_info.proc_type = ORTE_PROC_TOOL;

    /* we are never allowed to operate as a distributed tool,
     * so insist on the ess/tool component */
    opal_setenv("OMPI_MCA_ess", "tool", true, &environ);
    
    if (myglobals.debug) {
        orte_devel_level_output = true;
    }

   /* Intialize our Open RTE environment
     * Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        /* cannot call ORTE_ERROR_LOG as it could be the errmgr
         * never got loaded!
         */
        return rc;
    }
    /* finalize OPAL. As it was opened again from orte_init->opal_init
     * we continue to have a reference count on it. So we have to finalize it twice...
     */
    opal_finalize();

    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp(environ[i], "OMPI", 4)) {
            fprintf(stderr, "%s\n", environ[i]);
        }
    }
    
    /* set the info in our contact table */
    orte_rml.set_contact_info(orte_process_info.my_hnp_uri);
    /* extract the name */
    if (ORTE_SUCCESS != orte_rml_base_parse_uris(orte_process_info.my_hnp_uri, ORTE_PROC_MY_HNP, NULL)) {
        orte_show_help("help-orte-top.txt", "orte-top:hnp-uri-bad", true, orte_process_info.my_hnp_uri);
        exit(1);
    }
    /* set the route to be direct */
    if (ORTE_SUCCESS != orte_routed.update_route(ORTE_PROC_MY_HNP, ORTE_PROC_MY_HNP)) {
        orte_show_help("help-orte-top.txt", "orte-top:hnp-uri-bad", true, orte_process_info.my_hnp_uri);
        orte_finalize();
        exit(1);
    }
    
     /* set the target hnp as our lifeline so we will terminate if it exits */
    orte_routed.set_lifeline(ORTE_PROC_MY_HNP);

    /* setup to listen for HNP response to my commands */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_TOOL,
                            ORTE_RML_PERSISTENT, local_recv, NULL);

    /* set a timeout event in case the HNP doesn't answer */
    
    /* if this is the terminate command, just send it */
    if (myglobals.terminate) {
        opal_buffer_t *buf;
        orte_daemon_cmd_flag_t cmd = ORTE_DAEMON_HALT_DVM_CMD;
        buf = OBJ_NEW(opal_buffer_t);
        opal_dss.pack(buf, &cmd, 1, ORTE_DAEMON_CMD);
        orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                ORTE_RML_TAG_DAEMON,
                                orte_rml_send_callback, NULL);
        goto waiting;
    }
    
    /* default our personality to OMPI */
    if (NULL == myglobals.personality) {
        myglobals.personality = strdup("ompi");
    }

    /* create a new job object to hold the info for this one - the
     * jobid field will be filled in by the PLM when the job is
     * launched
     */
    jdata = OBJ_NEW(orte_job_t);
    if (NULL == jdata) {
        /* cannot call ORTE_ERROR_LOG as the errmgr
         * hasn't been loaded yet!
         */
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    jdata->personality = strdup(myglobals.personality);
    
    /* check what user wants us to do with stdin */
    if (NULL != myglobals.stdin_target) {
        if (0 == strcmp(myglobals.stdin_target, "all")) {
            jdata->stdin_target = ORTE_VPID_WILDCARD;
        } else if (0 == strcmp(myglobals.stdin_target, "none")) {
            jdata->stdin_target = ORTE_VPID_INVALID;
        } else {
            jdata->stdin_target = strtoul(myglobals.stdin_target, NULL, 10);
        }
    }
    
    /* if we want the argv's indexed, indicate that */
    if (myglobals.index_argv) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_INDEX_ARGV, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }

    /* Parse each app, adding it to the job object */
    parse_locals(jdata, argc, argv);

    /* create the map object to communicate policies */
    jdata->map = OBJ_NEW(orte_job_map_t);
    
    if (NULL != myglobals.mapping_policy) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_set_mapping_policy(&jdata->map->mapping, NULL, myglobals.mapping_policy))) {
            ORTE_ERROR_LOG(rc);
            exit(rc);
        }
    }
    if (NULL != myglobals.ranking_policy) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_set_ranking_policy(&jdata->map->ranking,
                                                                     jdata->map->mapping,
                                                                     myglobals.ranking_policy))) {
            ORTE_ERROR_LOG(rc);
            exit(rc);
        }
    }
#if OPAL_HAVE_HWLOC
    if (NULL != myglobals.binding_policy) {
        if (ORTE_SUCCESS != (rc = opal_hwloc_base_set_binding_policy(&jdata->map->binding,
                                                                     myglobals.binding_policy))) {
            ORTE_ERROR_LOG(rc);
            exit(rc);
        }
    }
#endif /* OPAL_HAVE_HWLOC */
    
    /* if they asked for nolocal, mark it so */
    if (myglobals.nolocal) {
        ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_USE_LOCAL);
    }
    if (myglobals.no_oversubscribe) {
        ORTE_UNSET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
    }
    if (myglobals.oversubscribe) {
        ORTE_UNSET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
    }
    if (myglobals.report_bindings) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_REPORT_BINDINGS, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }
    if (myglobals.slot_list) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_SLOT_LIST, ORTE_ATTR_GLOBAL, myglobals.slot_list, OPAL_STRING);
    }
    if (NULL == myglobals.personality) {
        /* default to ompi */
        jdata->personality = strdup("ompi");
    } else {
        jdata->personality = strdup(myglobals.personality);
    }
    
    if (0 == jdata->num_apps) {
        /* This should never happen -- this case should be caught in
           create_app(), but let's just double check... */
        orte_show_help("help-orterun.txt", "orterun:nothing-to-do",
                       true, myglobals.basename);
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* check for a job timeout specification, to be provided in seconds
     * as that is what MPICH used
     */
    if (NULL != (param = getenv("MPIEXEC_TIMEOUT"))) {
        if (NULL == (orte_mpiexec_timeout = OBJ_NEW(orte_timer_t))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERR_OUT_OF_RESOURCE);
            goto DONE;
        }
        orte_mpiexec_timeout->tv.tv_sec = strtol(param, NULL, 10);
        orte_mpiexec_timeout->tv.tv_usec = 0;
        opal_event_evtimer_set(orte_event_base, orte_mpiexec_timeout->ev,
                               orte_timeout_wakeup, jdata);
        opal_event_set_priority(orte_mpiexec_timeout->ev, ORTE_ERROR_PRI);
        opal_event_evtimer_add(orte_mpiexec_timeout->ev, &orte_mpiexec_timeout->tv);
    }

    /* if recovery was disabled on the cmd line, do so */
    if (myglobals.enable_recovery) {
        ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_RECOVERABLE);
    }

    /* ask the HNP to spawn the job for us */
    // post recv on tag_confirm_spawn, pass jdata as cbdata
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_CONFIRM_SPAWN,
                            ORTE_RML_PERSISTENT, spawn_recv, jdata);
    // pack the ORTE_DAEMON_SPAWN_JOB_CMD command and job object and send to HNP at tag ORTE_RML_TAG_DAEMON
    req = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(req, &cmd, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        exit(rc);
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(req, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        exit(rc);
    }
    orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, req, ORTE_RML_TAG_DAEMON, orte_rml_send_callback, NULL);
    
    // wait for response and unpack the status, jobid
    ORTE_WAIT_FOR_COMPLETION(myspawn);
    opal_output(0, "Job %s has launched", ORTE_JOBID_PRINT(jdata->jobid));
    
 waiting:
    ORTE_WAIT_FOR_COMPLETION(mywait);

 DONE:
    /* cleanup and leave */
    orte_finalize();

    if (orte_debug_flag) {
        fprintf(stderr, "exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
}

static int init_globals(void)
{
    /* Reset the other fields every time */
    myglobals.help = false;
    myglobals.version = false;
    myglobals.num_procs =  0;
    if (NULL != myglobals.appfile) {
        free(myglobals.appfile);
    }
    myglobals.appfile = NULL;
    if (NULL != myglobals.wdir) {
        free(myglobals.wdir);
    }
    myglobals.set_cwd_to_session_dir = false;
    myglobals.wdir = NULL;
    if (NULL != myglobals.path) {
        free(myglobals.path);
    }
    myglobals.path = NULL;

    myglobals.preload_binaries = false;
    myglobals.preload_files  = NULL;

    /* All done */
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line)
{
    /* check for request to report pid */
    if (NULL != myglobals.report_pid) {
        FILE *fp;
        if (0 == strcmp(myglobals.report_pid, "-")) {
            /* if '-', then output to stdout */
            printf("%d\n", (int)getpid());
        } else if (0 == strcmp(myglobals.report_pid, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%d\n", (int)getpid());
        } else {
            fp = fopen(myglobals.report_pid, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               myglobals.basename, "pid", myglobals.report_pid);
                exit(0);
            }
            fprintf(fp, "%d\n", (int)getpid());
            fclose(fp);
        }
    }
    
    return ORTE_SUCCESS;
}


static int parse_locals(orte_job_t *jdata, int argc, char* argv[])
{
    int i, rc, app_num;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;

    /* Make the apps */
    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
                /** keep track of the number of apps - point this app_context to that index */
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    app->idx = app_num;
                    ++app_num;
                    opal_pointer_array_add(jdata->apps, app);
                    ++jdata->num_apps;
                }

                /* Reset the temps */

                temp_argc = 0;
                temp_argv = NULL;
                opal_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            opal_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (opal_argv_count(temp_argv) > 1) {
        app = NULL;
        rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            app->idx = app_num;
            ++app_num;
            opal_pointer_array_add(jdata->apps, app);
            ++jdata->num_apps;
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

   /* Once we've created all the apps, add the global MCA params to
       each app's environment (checking for duplicates, of
       course -- yay opal_environ_merge()).  */

    if (NULL != global_mca_env) {
        size1 = (size_t)opal_pointer_array_get_size(jdata->apps);
        /* Iterate through all the apps */
        for (j = 0; j < size1; ++j) {
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, j);
            if (NULL != app) {
                /* Use handy utility function */
                env = opal_environ_merge(global_mca_env, app->env);
                opal_argv_free(app->env);
                app->env = env;
            }
        }
    }

    /* Now take a subset of the MCA params and set them as MCA
       overrides here in orterun (so that when we orte_init() later,
       all the components see these MCA params).  Here's how we decide
       which subset of the MCA params we set here in orterun:

       1. If any global MCA params were set, use those
       2. If no global MCA params were set and there was only one app,
          then use its app MCA params
       3. Otherwise, don't set any
    */

    env = NULL;
    if (NULL != global_mca_env) {
        env = global_mca_env;
    } else {
        if (opal_pointer_array_get_size(jdata->apps) >= 1) {
            /* Remember that pointer_array's can be padded with NULL
               entries; so only use the app's env if there is exactly
               1 non-NULL entry */
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, 0);
            if (NULL != app) {
                env = app->env;
                for (j = 1; j < opal_pointer_array_get_size(jdata->apps); ++j) {
                    if (NULL != opal_pointer_array_get_item(jdata->apps, j)) {
                        env = NULL;
                        break;
                    }
                }
            }
        }
    }

    if (NULL != env) {
        size1 = opal_argv_count(env);
        for (j = 0; j < size1; ++j) {
            /* Use-after-Free error possible here.  putenv does not copy
             * the string passed to it, and instead stores only the pointer.
             * env[j] may be freed later, in which case the pointer
             * in environ will now be left dangling into a deallocated
             * region.
             * So we make a copy of the variable.
             */
            char *s = strdup(env[j]);
            
            if (NULL == s) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            putenv(s);
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   orterun --mca foo bar -app appfile
 *
 * That is, we'll need to keep foo=bar, but the presence of the app
 * file will cause an invocation of parse_appfile(), which will cause
 * one or more recursive calls back to create_app().  Since the
 * foo=bar value applies globally to all apps in the appfile, we need
 * to pass in the "base" environment (that contains the foo=bar value)
 * when we parse each line in the appfile.
 *
 * This is really just a special case -- when we have a simple case like:
 *
 *   orterun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[],
                      orte_job_t *jdata,
                      orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    opal_cmd_line_t cmd_line;
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value;
    orte_app_context_t *app = NULL;
    bool cmd_line_made = false;
    bool found = false;
    char *appname;

    *made_app = false;

    /* Pre-process the command line if we are going to parse an appfile later.
     * save any mca command line args so they can be passed
     * separately to the daemons.
     * Use Case:
     *  $ cat launch.appfile
     *  -np 1 -mca aaa bbb ./my-app -mca ccc ddd
     *  -np 1 -mca aaa bbb ./my-app -mca eee fff
     *  $ mpirun -np 2 -mca foo bar --app launch.appfile
     * Only pick up '-mca foo bar' on this pass.
     */
    if (NULL != myglobals.appfile) {
        if (ORTE_SUCCESS != (rc = orte_schizo.parse_cli(myglobals.personality, argc, 0, argv))) {
            goto cleanup;
        }
    }
    
    /* Parse application command line options. */

    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    cmd_line_made = true;
    rc = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    mca_base_cmd_line_process_args(&cmd_line, app_env, &global_mca_env);

    /* Is there an appfile in here? */

    if (NULL != myglobals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(jdata, strdup(myglobals.appfile), app_env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    opal_cmd_line_get_tail(&cmd_line, &count, &app->argv);

    /* See if we have anything left */

    if (0 == count) {
        orte_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, myglobals.basename, myglobals.basename);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * Get mca parameters so we can pass them to the daemons.
     * Use the count determined above to make sure we do not go past
     * the executable name. Example:
     *   mpirun -np 2 -mca foo bar ./my-app -mca bip bop
     * We want to pick up '-mca foo bar' but not '-mca bip bop'
     */
    if (ORTE_SUCCESS != (rc = orte_schizo.parse_cli(myglobals.personality,
                                                    argc, count, argv))) {
        goto cleanup;
    }
    
    /* Grab all OMPI_* environment variables */

    app->env = opal_argv_copy(*app_env);
    if (ORTE_SUCCESS != (rc = orte_schizo.parse_env(myglobals.personality,
                                                    myglobals.path,
                                                    &cmd_line, NULL,
                                                    environ, &app->env))) {
        goto cleanup;
    }
    

    /* Did the user request a specific wdir? */

    if (NULL != myglobals.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(myglobals.wdir)) {
            app->cwd = strdup(myglobals.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                orte_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, myglobals.wdir, NULL);
        }
        orte_set_attribute(&app->attributes, ORTE_APP_USER_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    } else if (myglobals.set_cwd_to_session_dir) {
        orte_set_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
        orte_set_attribute(&app->attributes, ORTE_APP_USER_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            orte_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
    }

    /* if this is the first app_context, check for prefix directions.
     * We only do this for the first app_context because the launchers
     * only look at the first one when setting the prefix - we do NOT
     * support per-app_context prefix settings!
     */
    if (0 == total_num_apps) {
        /* Check to see if the user explicitly wanted to disable automatic
           --prefix behavior */
        
        if (opal_cmd_line_is_taken(&cmd_line, "noprefix")) {
            want_prefix_by_default = false;
        }

        /* Did the user specify a prefix, or want prefix by default? */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") || want_prefix_by_default) {
            size_t param_len;
            /* if both the prefix was given and we have a prefix
             * given above, check to see if they match
             */
            if (opal_cmd_line_is_taken(&cmd_line, "prefix") &&
                NULL != myglobals.prefix) {
                /* if they don't match, then that merits a warning */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
                /* ensure we strip any trailing '/' */
                if (0 == strcmp(OPAL_PATH_SEP, &(param[strlen(param)-1]))) {
                    param[strlen(param)-1] = '\0';
                }
                value = strdup(myglobals.prefix);
                if (0 == strcmp(OPAL_PATH_SEP, &(value[strlen(value)-1]))) {
                    value[strlen(value)-1] = '\0';
                }
                if (0 != strcmp(param, value)) {
                    orte_show_help("help-orterun.txt", "orterun:app-prefix-conflict",
                                   true, myglobals.basename, value, param);
                    /* let the global-level prefix take precedence since we
                     * know that one is being used
                     */
                    free(param);
                    param = strdup(myglobals.prefix);
                }
                free(value);
            } else if (NULL != myglobals.prefix) {
                param = strdup(myglobals.prefix);
            } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
                /* must be --prefix alone */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            } else {
                /* --enable-orterun-prefix-default was given to orterun */
                param = strdup(opal_install_dirs.prefix);
            }

            if (NULL != param) {
                /* "Parse" the param, aka remove superfluous path_sep. */
                param_len = strlen(param);
                while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                    param[param_len-1] = '\0';
                    param_len--;
                    if (0 == param_len) {
                        orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                       true, myglobals.basename, myglobals.basename);
                        free(param);
                        return ORTE_ERR_FATAL;
                    }
                }
                orte_set_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, ORTE_ATTR_GLOBAL, param, OPAL_STRING);
                free(param);
            }
        }
    }

    /* Did the user specify a hostfile. Need to check for both 
     * hostfile and machine file. 
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "hostfile"))) {
        if(1 < j) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, myglobals.basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "hostfile", 0, 0);
            orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, ORTE_ATTR_GLOBAL, value, OPAL_STRING);
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "machinefile"))) {
        if(1 < j || orte_get_attribute(&app->attributes, ORTE_APP_HOSTFILE, NULL, OPAL_STRING)) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, myglobals.basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "machinefile", 0, 0);
            orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, ORTE_ATTR_GLOBAL, value, OPAL_STRING);
        }
    }
 
    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "host"))) {
        char **targ=NULL, *tval;
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(&cmd_line, "host", i, 0);
            opal_argv_append_nosize(&targ, value);
        }
        tval = opal_argv_join(targ, ',');
        orte_set_attribute(&app->attributes, ORTE_APP_DASH_HOST, ORTE_ATTR_GLOBAL, tval, OPAL_STRING);
        opal_argv_free(targ);
        free(tval);
    }

    /* check for bozo error */
    if (0 > myglobals.num_procs) {
        orte_show_help("help-orterun.txt", "orterun:negative-nprocs",
                       true, myglobals.basename, app->argv[0],
                       myglobals.num_procs, NULL);
        return ORTE_ERR_FATAL;
    }

    app->num_procs = (orte_std_cntr_t)myglobals.num_procs;
    total_num_apps++;

    /* Capture any preload flags */
    if (myglobals.preload_binaries) {
        orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_BIN, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }
    /* if we were told to cwd to the session dir and the app was given in
     * relative syntax, then we need to preload the binary to
     * find the app - don't do this for java apps, however, as we
     * can't easily find the class on the cmd line. Java apps have to
     * preload their binary via the preload_files option
     */
    if (!opal_path_is_absolute(app->argv[0]) &&
        NULL == strstr(app->argv[0], "java")) {
        if (myglobals.preload_binaries) {
            orte_set_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
        } else if (orte_get_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, NULL, OPAL_BOOL)) {
            orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_BIN, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
        }
    }
    if (NULL != myglobals.preload_files) {
        orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_FILES, ORTE_ATTR_GLOBAL,
                           myglobals.preload_files, OPAL_STRING);
    }

    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->app = strdup(app->argv[0]);
    if (NULL == app->app) {
        orte_show_help("help-orterun.txt", "orterun:call-failed",
                       true, myglobals.basename, "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* if this is a Java application, we have a bit more work to do. Such
     * applications actually need to be run under the Java virtual machine
     * and the "java" command will start the "executable". So we need to ensure
     * that all the proper java-specific paths are provided
     */
    appname = opal_basename(app->app);
    if (0 == strcmp(appname, "java")) {
        /* see if we were given a library path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                if (NULL == strstr(app->argv[i], opal_install_dirs.libdir)) {
                    /* doesn't appear to - add it to be safe */
                    if (':' == app->argv[i][strlen(app->argv[i]-1)]) {
                        asprintf(&value, "-Djava.library.path=%s%s", app->argv[i], opal_install_dirs.libdir);
                    } else {
                        asprintf(&value, "-Djava.library.path=%s:%s", app->argv[i], opal_install_dirs.libdir);
                    }
                    free(app->argv[i]);
                    app->argv[i] = value;
                }
                break;
            }
        }
        if (!found) {
            /* need to add it right after the java command */
            asprintf(&value, "-Djava.library.path=%s", opal_install_dirs.libdir);
            opal_argv_insert_element(&app->argv, 1, value);
            free(value);
        }
        
        /* see if we were given a class path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "cp") ||
                NULL != strstr(app->argv[i], "classpath")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                /* check if mpi.jar exists - if so, add it */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "mpi.jar");
                }
                free(value);
                /* check for oshmem support */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "shmem.jar");
                }
                free(value);
                /* always add the local directory */
                asprintf(&value, "%s:%s", app->cwd, app->argv[i+1]);
                free(app->argv[i+1]);
                app->argv[i+1] = value;
                break;
            }
        }
        if (!found) {
            /* check to see if CLASSPATH is in the environment */
            found = false;  // just to be pedantic
            for (i=0; NULL != environ[i]; i++) {
                if (0 == strncmp(environ[i], "CLASSPATH", strlen("CLASSPATH"))) {
                    value = strchr(environ[i], '=');
                    ++value; /* step over the = */
                    opal_argv_insert_element(&app->argv, 1, value);
                    /* check for mpi.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "mpi.jar");
                    }
                    free(value);
                    /* check for shmem.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "shmem.jar");
                    }
                    free(value);
                    /* always add the local directory */
                    (void)asprintf(&value, "%s:%s", app->cwd, app->argv[1]);
                    free(app->argv[1]);
                    app->argv[1] = value;
                    opal_argv_insert_element(&app->argv, 1, "-cp");
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* need to add it right after the java command - have
                 * to include the working directory and trust that
                 * the user set cwd if necessary
                 */
                char *str, *str2;
                /* always start with the working directory */
                str = strdup(app->cwd);
                /* check for mpi.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    (void)asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                /* check for shmem.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                opal_argv_insert_element(&app->argv, 1, str);
                free(str);
                opal_argv_insert_element(&app->argv, 1, "-cp");
            }
        }
        /* try to find the actual command - may not be perfect */
        for (i=1; i < opal_argv_count(app->argv); i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                continue;
            } else if (NULL != strstr(app->argv[i], "cp") ||
                       NULL != strstr(app->argv[i], "classpath")) {
                /* skip the next field */
                i++;
                continue;
            }
            /* declare this the winner */
            opal_setenv("OMPI_COMMAND", app->argv[i], true, &app->env);
            /* collect everything else as the cmd line */
            if ((i+1) < opal_argv_count(app->argv)) {
                value = opal_argv_join(&app->argv[i+1], ' ');
                opal_setenv("OMPI_ARGV", value, true, &app->env);
                free(value);
            }
            break;
        }
    } else {
        /* add the cmd to the environment for MPI_Info to pickup */
        opal_setenv("OMPI_COMMAND", appname, true, &app->env);
        if (1 < opal_argv_count(app->argv)) {
            value = opal_argv_join(&app->argv[1], ' ');
            opal_setenv("OMPI_ARGV", value, true, &app->env);
            free(value);
        }
    }
    free(appname);
    
    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (cmd_line_made) {
        OBJ_DESTRUCT(&cmd_line);
    }
    return rc;
}

static void set_classpath_jar_file(orte_app_context_t *app, int index, char *jarfile)
{
    if (NULL == strstr(app->argv[index], jarfile)) {
        /* nope - need to add it */
        char *fmt = ':' == app->argv[index][strlen(app->argv[index]-1)]
                    ? "%s%s/%s" : "%s:%s/%s";
        char *str;
        asprintf(&str, fmt, app->argv[index], opal_install_dirs.libdir, jarfile);
        free(app->argv[index]);
        app->argv[index] = str;
    }
}

static int parse_appfile(orte_job_t *jdata, char *filename, char ***env)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc, app_num;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /*
     * Make sure to clear out this variable so we don't do anything odd in
     * app_create()
     */
    if (NULL != myglobals.appfile) {
        free(myglobals.appfile);
        myglobals.appfile = NULL;
    }

    /* Try to open the file */

    fp = fopen(filename, "r");
    if (NULL == fp) {
        orte_show_help("help-orterun.txt", "orterun:appfile-not-found", true,
                       filename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Read in line by line */

    line[sizeof(line) - 1] = '\0';
    app_num = 0;
    do {

        /* We need a bogus argv[0] (because when argv comes in from
           the command line, argv[0] is "orterun", so the parsing
           logic ignores it).  So create one here rather than making
           an argv and then pre-pending a new argv[0] (which would be
           rather inefficient). */

        line[0] = '\0';
        strcat(line, bogus);

        if (NULL == fgets(line + sizeof(bogus) - 1,
                          sizeof(line) - sizeof(bogus) - 1, fp)) {
            break;
        }

        /* Remove a trailing newline */

        len = strlen(line);
        if (len > 0 && '\n' == line[len - 1]) {
            line[len - 1] = '\0';
            if (len > 0) {
                --len;
            }
        }

        /* Remove comments */

        for (i = 0; i < len; ++i) {
            if ('#' == line[i]) {
                line[i] = '\0';
                break;
            } else if (i + 1 < len && '/' == line[i] && '/' == line[i + 1]) {
                line[i] = '\0';
                break;
            }
        }

        /* Is this a blank line? */

        len = strlen(line);
        for (blank = true, i = sizeof(bogus); i < len; ++i) {
            if (!isspace(line[i])) {
                blank = false;
                break;
            }
        }
        if (blank) {
            continue;
        }

        /* We got a line with *something* on it.  So process it */

        argv = opal_argv_split(line, ' ');
        argc = opal_argv_count(argv);
        if (argc > 0) {

            /* Create a temporary env to use in the recursive call --
               that is: don't disturb the original env so that we can
               have a consistent global env.  This allows for the
               case:

                   orterun --mca foo bar --appfile file

               where the "file" contains multiple apps.  In this case,
               each app in "file" will get *only* foo=bar as the base
               environment from which its specific environment is
               constructed. */

            if (NULL != *env) {
                tmp_env = opal_argv_copy(*env);
                if (NULL == tmp_env) {
                    fclose(fp);
                    opal_argv_free(argv);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, jdata, &app, &made_app, &tmp_env);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
            opal_argv_free(argv);
            if (made_app) {
                app->idx = app_num;
                ++app_num;
                opal_pointer_array_add(jdata->apps, app);
                ++jdata->num_apps;
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    
    return ORTE_SUCCESS;
}

void orte_timeout_wakeup(int sd, short args, void *cbdata)
{
    char *tm;

    /* this function gets called when the job execution time
     * has hit a prescribed limit - so just abort
     */
    tm = getenv("MPIEXEC_TIMEOUT");
    orte_show_help("help-orterun.txt", "orterun:timeout",
                   true, (NULL == tm) ? "NULL" : tm);
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
    exit(orte_exit_status);
}

static void local_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata)
{
    int rc, ret;
    int32_t cnt;

    /* unpack the completion status of the job */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_UPDATE_EXIT_STATUS(rc);
    }
    /* update our exit status to match */
    ORTE_UPDATE_EXIT_STATUS(ret);

    exit(orte_exit_status);
}

static void spawn_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata)
{
    orte_job_t *jdata = (orte_job_t*)cbdata;
    int32_t cnt;

    // extract the returned jobid
    cnt = 1;
    opal_dss.unpack(buffer, &jdata->jobid, &cnt, ORTE_JOBID);

    // release the wait
    myspawn = false;
}
