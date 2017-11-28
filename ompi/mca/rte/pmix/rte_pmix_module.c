/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2012-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 */
#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif  /* HAVE_PWD_H */

#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/threads/threads.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "ompi/mca/rte/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"

/* instantiate a debugger-required value */
volatile int MPIR_being_debugged = 0;

extern ompi_rte_component_t mca_rte_pmix_component;

/* storage to support OMPI */
opal_process_name_t pmix_name_wildcard = {UINT32_MAX-1, UINT32_MAX-1};
opal_process_name_t pmix_name_invalid = {UINT32_MAX, UINT32_MAX};
opal_process_name_t pmix_proc_my_name = {0, 0};
hwloc_cpuset_t ompi_proc_applied_binding = NULL;
pmix_process_info_t pmix_process_info = {0};
bool pmix_proc_is_bound = false;

static bool pmix_in_parallel_debugger = false;
static bool added_transport_keys = false;
static bool added_num_procs = false;
static bool added_app_ctx = false;
static char* pre_condition_transports_print(uint64_t *unique_key);
static int _setup_job_session_dir(char **sdir);

#define ORTE_SCHEMA_DELIMITER_CHAR      '.'
#define ORTE_SCHEMA_WILDCARD_CHAR       '*'
#define ORTE_SCHEMA_WILDCARD_STRING     "*"
#define ORTE_SCHEMA_INVALID_CHAR        '$'
#define ORTE_SCHEMA_INVALID_STRING      "$"

int ompi_rte_compare_name_fields(ompi_rte_cmp_bitmask_t fields,
                                 const opal_process_name_t* name1,
                                 const opal_process_name_t* name2)
{
    /* handle the NULL pointer case */
    if (NULL == name1 && NULL == name2) {
        return OPAL_EQUAL;
    } else if (NULL == name1) {
        return OPAL_VALUE2_GREATER;
    } else if (NULL == name2) {
        return OPAL_VALUE1_GREATER;
    }

    /* in this comparison function, we check for exact equalities.
     * In the case of wildcards, we check to ensure that the fields
     * actually match those values - thus, a "wildcard" in this
     * function does not actually stand for a wildcard value, but
     * rather a specific value - UNLESS the CMP_WILD bitmask value
     * is set
    */

    /* check job id */
    if (OMPI_RTE_CMP_JOBID & fields) {
        if (OMPI_RTE_CMP_WILD & fields &&
            (pmix_name_wildcard.jobid == name1->jobid ||
             pmix_name_wildcard.jobid == name2->jobid)) {
            goto check_vpid;
        }
        if (name1->jobid < name2->jobid) {
            return OPAL_VALUE2_GREATER;
        } else if (name1->jobid > name2->jobid) {
            return OPAL_VALUE1_GREATER;
        }
    }

    /* get here if jobid's are equal, or not being checked
     * now check vpid
     */
  check_vpid:
    if (OMPI_RTE_CMP_VPID & fields) {
        if (OMPI_RTE_CMP_WILD & fields &&
            (pmix_name_wildcard.vpid == name1->vpid ||
             pmix_name_wildcard.vpid == name2->vpid)) {
            return OPAL_EQUAL;
        }
        if (name1->vpid < name2->vpid) {
            return OPAL_VALUE2_GREATER;
        } else if (name1->vpid > name2->vpid) {
            return OPAL_VALUE1_GREATER;
        }
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return OPAL_EQUAL;
}

int ompi_rte_convert_string_to_process_name(opal_process_name_t *name,
                                            const char* name_string)
{
    char *temp, *token;
    opal_jobid_t job;
    opal_vpid_t vpid;
    int return_code=OPAL_SUCCESS;

    /* set default */
    name->jobid = pmix_name_invalid.jobid;
    name->vpid = pmix_name_invalid.vpid;

    /* check for NULL string - error */
    if (NULL == name_string) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    temp = strdup(name_string);  /** copy input string as the strtok process is destructive */
    token = strchr(temp, ORTE_SCHEMA_DELIMITER_CHAR); /** get first field -> jobid */

    /* check for error */
    if (NULL == token) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        free(temp);
        return OPAL_ERR_BAD_PARAM;
    }
    *token = '\0';
    token++;

    /* check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(temp, ORTE_SCHEMA_WILDCARD_STRING)) {
        job = pmix_name_wildcard.jobid;
    } else if (0 == strcmp(temp, ORTE_SCHEMA_INVALID_STRING)) {
        job = pmix_name_invalid.jobid;
    } else {
        job = strtoul(temp, NULL, 10);
    }

    /* check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(token, ORTE_SCHEMA_WILDCARD_STRING)) {
        vpid = pmix_name_wildcard.vpid;
    } else if (0 == strcmp(token, ORTE_SCHEMA_INVALID_STRING)) {
        vpid = pmix_name_invalid.vpid;
    } else {
        vpid = strtoul(token, NULL, 10);
    }

    name->jobid = job;
    name->vpid = vpid;

    free(temp);

    return return_code;
}

int ompi_rte_convert_process_name_to_string(char** name_string,
                                            const opal_process_name_t *name)
{
    char *tmp, *tmp2;

    if (NULL == name) { /* got an error */
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    /* check for wildcard and invalid values - where encountered, insert the
     * corresponding string so we can correctly parse the name string when
     * it is passed back to us later
     */
    if (pmix_name_wildcard.jobid == name->jobid) {
        asprintf(&tmp, "%s", ORTE_SCHEMA_WILDCARD_STRING);
    } else if (pmix_name_invalid.jobid == name->jobid) {
        asprintf(&tmp, "%s", ORTE_SCHEMA_INVALID_STRING);
    } else {
        asprintf(&tmp, "%lu", (unsigned long)name->jobid);
    }

    if (pmix_name_wildcard.vpid == name->vpid) {
        asprintf(&tmp2, "%s%c%s", tmp, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_WILDCARD_STRING);
    } else if (pmix_name_invalid.vpid == name->vpid) {
        asprintf(&tmp2, "%s%c%s", tmp, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_INVALID_STRING);
    } else {
        asprintf(&tmp2, "%s%c%lu", tmp, ORTE_SCHEMA_DELIMITER_CHAR, (unsigned long)name->vpid);
    }

    asprintf(name_string, "%s", tmp2);

    free(tmp);
    free(tmp2);

    return OPAL_SUCCESS;
}

int ompi_rte_init(int *pargc, char ***pargv)
{
    int ret;
    char *error = NULL;
    opal_process_name_t pname;
    opal_proc_t *myname;
    int u32, *u32ptr;
    uint16_t u16, *u16ptr;
    char **peers=NULL, *mycpuset;
    char *envar, *ev1, *ev2;
    opal_value_t *kv;
    char *val;
    size_t i;
    uint64_t unique_key[2];
    char *string_key;

    u32ptr = &u32;
    u16ptr = &u16;
    memset(&pmix_process_info, 0, sizeof(pmix_process_info));

    /* initialize the opal layer */
    if (OPAL_SUCCESS != (ret = opal_init(pargc, pargv))) {
        error = "opal_init";
        goto error;
    }

    /* open and setup pmix */
    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
        OPAL_ERROR_LOG(ret);
        /* we cannot run */
        error = "pmix init";
        goto error;
    }
    if (OPAL_SUCCESS != (ret = opal_pmix_base_select())) {
        /* we cannot run */
        error = "pmix init";
        goto error;
    }
    /* set the event base */
    opal_pmix_base_set_evbase(opal_sync_event_base);

    /* initialize the selected module */
    if (!opal_pmix.initialized() && (OPAL_SUCCESS != (ret = opal_pmix.init(NULL)))) {
        /* we cannot run - this could be due to being direct launched
         * without the required PMI support being built, so print
         * out a help message indicating it */
        opal_show_help("help-ompi-rte-pmix.txt", "no-pmi", true);
        return OPAL_ERR_SILENT;
    }
    /* opal_pmix.init will have filled in proc name fields in
     * OPAL, so transfer them here */
    myname = opal_proc_local_get();
    pmix_proc_my_name = myname->proc_name;
    /* get our hostname */
    pmix_process_info.nodename = opal_get_proc_hostname(myname);

    /* get our local rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_RANK,
                          &pmix_proc_my_name, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting local rank";
        goto error;
    }
    pmix_process_info.my_local_rank = u16;

    /* get our node rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_NODE_RANK,
                          &pmix_proc_my_name, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting node rank";
        goto error;
    }
    pmix_process_info.my_node_rank = u16;

    /* get job size */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_JOB_SIZE,
                          &pmix_name_wildcard, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS != ret) {
        error = "getting job size";
        goto error;
    }
    pmix_process_info.num_procs = u32;

    /* push into the environ for pickup in MPI layer for
     * MPI-3 required info key
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_ess_num_procs")) {
        asprintf(&ev1, OPAL_MCA_PREFIX"orte_ess_num_procs=%d", pmix_process_info.num_procs);
        putenv(ev1);
        added_num_procs = true;
    }
    if (NULL == getenv("OMPI_APP_CTX_NUM_PROCS")) {
        asprintf(&ev2, "OMPI_APP_CTX_NUM_PROCS=%d", pmix_process_info.num_procs);
        putenv(ev2);
        added_app_ctx = true;
    }

    /* get our app number from PMI - ok if not found */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, OPAL_PMIX_APPNUM,
                                   &pmix_proc_my_name, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS == ret) {
        pmix_process_info.app_num = u32;
    } else {
        pmix_process_info.app_num = 0;
    }

    /* get the number of local peers - required for wireup of
     * shared memory BTL */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_SIZE,
                          &pmix_name_wildcard, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS == ret) {
        pmix_process_info.num_local_peers = u32 - 1;  // want number besides ourselves
    } else {
        pmix_process_info.num_local_peers = 0;
    }

    /* setup transport keys in case the MPI layer needs them -
     * we can use the jobfam and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_precondition_transports")) {
        unique_key[0] = (pmix_proc_my_name.jobid & 0xff00) >> 16;
        unique_key[1] = pmix_proc_my_name.jobid & 0x00ff;
        if (NULL == (string_key = pre_condition_transports_print(unique_key))) {
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        opal_output_verbose(2, ompi_rte_base_framework.framework_output,
                            "%s transport key %s",
                            OPAL_NAME_PRINT(pmix_proc_my_name), string_key);
        asprintf(&envar, OPAL_MCA_PREFIX"orte_precondition_transports=%s", string_key);
        putenv(envar);
        added_transport_keys = true;
        /* cannot free the envar as that messes up our environ */
        free(string_key);
    }

    /* retrieve temp directories info */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, OPAL_PMIX_NSDIR, &pmix_name_wildcard, &val, OPAL_STRING);
    if (OPAL_SUCCESS == ret && NULL != val) {
        pmix_process_info.job_session_dir = val;
        val = NULL;
    } else {
        /* we need to create something */
        ret = _setup_job_session_dir(&pmix_process_info.job_session_dir);
        if (OPAL_SUCCESS != ret) {
            error = "job session directory";
            goto error;
        }
    }

    /* get our local peers */
    if (0 < pmix_process_info.num_local_peers) {
        /* if my local rank if too high, then that's an error */
        if (pmix_process_info.num_local_peers < pmix_process_info.my_local_rank) {
            ret = OPAL_ERR_BAD_PARAM;
            error = "num local peers";
            goto error;
        }
        /* retrieve the local peers */
        OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_PEERS,
                              &pmix_name_wildcard, &val, OPAL_STRING);
        if (OPAL_SUCCESS == ret && NULL != val) {
            peers = opal_argv_split(val, ',');
            free(val);
        } else {
            peers = NULL;
        }
    } else {
        peers = NULL;
    }

    /* set the locality */
    if (NULL != peers) {
        /* identify our location */
        val = NULL;
        OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, OPAL_PMIX_LOCALITY_STRING,
                                       &pmix_proc_my_name, &val, OPAL_STRING);
        if (OPAL_SUCCESS == ret && NULL != val) {
            mycpuset = val;
        } else {
            mycpuset = NULL;
        }
        pname.jobid = pmix_proc_my_name.jobid;
        for (i=0; NULL != peers[i]; i++) {
            pname.vpid = strtoul(peers[i], NULL, 10);
            if (pname.vpid == pmix_proc_my_name.vpid) {
                /* we are fully local to ourselves */
                u16 = OPAL_PROC_ALL_LOCAL;
            } else {
                val = NULL;
                OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, OPAL_PMIX_LOCALITY_STRING,
                                               &pname, &val, OPAL_STRING);
                if (OPAL_SUCCESS == ret && NULL != val) {
                    u16 = opal_hwloc_compute_relative_locality(mycpuset, val);
                    free(val);
                } else {
                    /* all we can say is that it shares our node */
                    u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
                }
            }
            kv = OBJ_NEW(opal_value_t);
            kv->key = strdup(OPAL_PMIX_LOCALITY);
            kv->type = OPAL_UINT16;
            OPAL_OUTPUT_VERBOSE((1, ompi_rte_base_framework.framework_output,
                                 "%s locality: proc %s locality %s",
                                 OPAL_NAME_PRINT(pmix_proc_my_name),
                                 OPAL_NAME_PRINT(pname), opal_hwloc_base_print_locality(u16)));
            kv->data.uint16 = u16;
            ret = opal_pmix.store_local(&pname, kv);
            if (OPAL_SUCCESS != ret) {
                error = "local store of locality";
                opal_argv_free(peers);
                if (NULL != mycpuset) {
                    free(mycpuset);
                }
                goto error;
            }
            OBJ_RELEASE(kv);
        }
        opal_argv_free(peers);
        if (NULL != mycpuset) {
            free(mycpuset);
        }
    }

    /* poor attempt to detect we are bound */
    if (NULL != getenv("SLURM_CPU_BIND_TYPE")) {
        pmix_proc_is_bound = true;
    }

    /* push our hostname so others can find us, if they need to - the
     * native PMIx component will ignore this request as the hostname
     * is provided by the system */
    OPAL_MODEX_SEND_VALUE(ret, OPAL_PMIX_GLOBAL, OPAL_PMIX_HOSTNAME, pmix_process_info.nodename, OPAL_STRING);
    if (OPAL_SUCCESS != ret) {
        error = "db store hostname";
        goto error;
    }

    return OPAL_SUCCESS;

  error:
    opal_show_help_finalize();
    if (OPAL_ERR_SILENT != ret ) {
        opal_show_help("help-ompi-rte-pmix.txt",
                       "internal-failure",
                       true, error, opal_strerror(ret), ret);
    }
    return ret;

}

static bool check_file(const char *root, const char *path)
{
    struct stat st;
    char *fullpath;

    /*
     * Keep:
     *  - non-zero files starting with "output-"
     */
    if (0 == strncmp(path, "output-", strlen("output-"))) {
        fullpath = opal_os_path(false, &fullpath, root, path, NULL);
        stat(fullpath, &st);
        free(fullpath);
        if (0 == st.st_size) {
            return true;
        }
        return false;
    }

    return true;
}

int ompi_rte_finalize(void)
{
    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    if (added_transport_keys) {
        unsetenv(OPAL_MCA_PREFIX"orte_precondition_transports");
    }
    if (added_num_procs) {
        unsetenv(OPAL_MCA_PREFIX"orte_ess_num_procs");
    }
    if (added_app_ctx) {
        unsetenv("OMPI_APP_CTX_NUM_PROCS");
    }

    /* shutdown pmix */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
        (void) mca_base_framework_close(&opal_pmix_base_framework);
    }

    /* cleanup the session directory we created */
    if (NULL != pmix_process_info.job_session_dir) {
        opal_os_dirpath_destroy(pmix_process_info.job_session_dir,
                                false, check_file);
        free(pmix_process_info.job_session_dir);
    }
    return OMPI_SUCCESS;
}

void ompi_rte_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    char* buffer = NULL;
    struct timespec tp = {0, 100000};

    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        vasprintf( &buffer, fmt, arglist );
    }
    va_end(arglist);

    /* call abort */
    opal_pmix.abort(error_code, buffer, NULL);
    if (NULL != buffer) {
        free(buffer);
    }

    /* provide a little delay for the PMIx thread to
     * get the info out */
    nanosleep(&tp, NULL);

    /* Now Exit */
    _exit(error_code);
}

void ompi_rte_abort_peers(opal_process_name_t *procs,
                          int32_t num_procs,
                          int error_code)
{
    return;
}

static size_t handler = SIZE_MAX;
static bool debugger_register_active = true;
static bool debugger_event_active = true;

static void _release_fn(int status,
                        const opal_process_name_t *source,
                        opal_list_t *info, opal_list_t *results,
                        opal_pmix_notification_complete_fn_t cbfunc,
                        void *cbdata)
{
    /* must let the notifier know we are done */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, NULL, NULL, NULL, cbdata);
    }
    debugger_event_active = false;
}

static void _register_fn(int status,
                         size_t evhandler_ref,
                         void *cbdata)
{
    opal_list_t *codes = (opal_list_t*)cbdata;

    handler = evhandler_ref;
    OPAL_LIST_RELEASE(codes);
    debugger_register_active = false;
}

/*
 * Wait for a debugger if asked.  We support two ways of waiting for
 * attaching debuggers -- see big comment in
 * pmix/tools/pmixrun/debuggers.c explaining the two scenarios.
 */
void ompi_rte_wait_for_debugger(void)
{
    int debugger;
    opal_list_t *codes, directives;
    opal_value_t *kv;
    char *evar;
    int time;

    /* check PMIx to see if we are under a debugger */
    debugger = pmix_in_parallel_debugger;

    if (1 == MPIR_being_debugged) {
        debugger = 1;
    }

    if (!debugger && NULL == getenv("PMIX_TEST_DEBUGGER_ATTACH")) {
        /* if not, just return */
        return;
    }

    /* if we are being debugged, then we need to find
     * the correct plug-ins
     */
    ompi_debugger_setup_dlls();

    if (NULL != (evar = getenv("PMIX_TEST_DEBUGGER_SLEEP"))) {
        time = strtol(evar, NULL, 10);
        sleep(time);
        return;
    }

    /* register an event handler for the PMIX_ERR_DEBUGGER_RELEASE event */
    codes = OBJ_NEW(opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup("errorcode");
    kv->type = OPAL_INT;
    kv->data.integer = OPAL_ERR_DEBUGGER_RELEASE;
    opal_list_append(codes, &kv->super);

    OBJ_CONSTRUCT(&directives, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_EVENT_HDLR_NAME);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("MPI-DEBUGGER-ATTACH");
    opal_list_append(&directives, &kv->super);

    opal_pmix.register_evhandler(codes, &directives, _release_fn, _register_fn, codes);
    /* let the MPI progress engine run while we wait for registration to complete */
    OMPI_WAIT_FOR_COMPLETION(debugger_register_active);
    OPAL_LIST_DESTRUCT(&directives);

    /* let the MPI progress engine run while we wait for debugger release */
    OMPI_WAIT_FOR_COMPLETION(debugger_event_active);

    /* deregister the event handler */
    opal_pmix.deregister_evhandler(handler, NULL, NULL);
}

bool ompi_rte_connect_accept_support(const char *port)
{
    /* not sure how to support this yet */
    return false;
}

static char* pre_condition_transports_print(uint64_t *unique_key)
{
    unsigned int *int_ptr;
    size_t i, j, string_key_len, written_len;
    char *string_key = NULL, *format = NULL;

    /* string is two 64 bit numbers printed in hex with a dash between
     * and zero padding.
     */
    string_key_len = (sizeof(uint64_t) * 2) * 2 + strlen("-") + 1;
    string_key = (char*) malloc(string_key_len);
    if (NULL == string_key) {
        return NULL;
    }

    string_key[0] = '\0';
    written_len = 0;

    /* get a format string based on the length of an unsigned int.  We
     * want to have zero padding for sizeof(unsigned int) * 2
     * characters -- when printing as a hex number, each byte is
     * represented by 2 hex characters.  Format will contain something
     * that looks like %08lx, where the number 8 might be a different
     * number if the system has a different sized long (8 would be for
     * sizeof(int) == 4)).
     */
    asprintf(&format, "%%0%dx", (int)(sizeof(unsigned int)) * 2);

    /* print the first number */
    int_ptr = (unsigned int*) &unique_key[0];
    for (i = 0 ; i < sizeof(uint64_t) / sizeof(unsigned int) ; ++i) {
        if (0 == int_ptr[i]) {
            /* inject some energy */
            for (j=0; j < sizeof(unsigned int); j++) {
                int_ptr[i] |= j << j;
            }
        }
        snprintf(string_key + written_len,
                 string_key_len - written_len,
                 format, int_ptr[i]);
        written_len = strlen(string_key);
    }

    /* print the middle dash */
    snprintf(string_key + written_len, string_key_len - written_len, "-");
    written_len = strlen(string_key);

    /* print the second number */
    int_ptr = (unsigned int*) &unique_key[1];
    for (i = 0 ; i < sizeof(uint64_t) / sizeof(unsigned int) ; ++i) {
        if (0 == int_ptr[i]) {
            /* inject some energy */
            for (j=0; j < sizeof(unsigned int); j++) {
                int_ptr[i] |= j << j;
            }
        }
        snprintf(string_key + written_len,
                 string_key_len - written_len,
                 format, int_ptr[i]);
        written_len = strlen(string_key);
    }
    free(format);

    return string_key;
}

static int _setup_job_session_dir(char **sdir)
{
    char *tmpdir;
    /* get the effective uid */
    uid_t uid = geteuid();

    if( NULL == (tmpdir = getenv("TMPDIR")) )
        if( NULL == (tmpdir = getenv("TEMP")) )
            if( NULL == (tmpdir = getenv("TMP")) )
                tmpdir = "/tmp";

    if (0 > asprintf(&pmix_process_info.job_session_dir,
                     "%s/ompi.%s.%lu/jf.0/%u", tmpdir,
                     pmix_process_info.nodename,
                     (unsigned long)uid,
                     pmix_proc_my_name.jobid)) {
        pmix_process_info.job_session_dir = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}
