/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/base/ras_private.h"
#include "ras_alps.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include <alps/apInfo.h>

typedef int (*parser_fn_t)(char **val_if_found, FILE *fp,
                           const char *var_name);

typedef struct orte_ras_alps_sysconfig_t {
    /* path of file to parse */
    char *path;
    /* target variable name */
    char *var_name;
    /* parser to use */
    parser_fn_t parse;
} orte_ras_alps_sysconfig_t;

/* /// Local Functions /// */
static int orte_ras_alps_allocate(orte_job_t *jdata, opal_list_t *nodes);

static int orte_ras_alps_finalize(void);

static char *ras_alps_getline(FILE *fp);

static int orte_ras_alps_read_appinfo_file(opal_list_t *nodes,
                                           char *filename,
                                           unsigned int *uMe);

static char *orte_ras_get_appinfo_path(void);

static int parser_ini(char **val_if_found, FILE *fp, const char *var_name);

static int parser_separated_columns(char **val_if_found, FILE *fp,
                                    const char *var_name);

/* /// Local Variables /// */
static const orte_ras_alps_sysconfig_t sysconfigs[] = {
    {"/etc/sysconfig/alps", "ALPS_SHARED_DIR_PATH", parser_ini},
    {"/etc/alps.conf"     , "sharedDir"           , parser_separated_columns},
    {"/etc/opt/cray/alps/alps.conf", "sharedDir"  , parser_separated_columns},
    /* must be last element */
    {NULL                 , NULL                  , NULL}
};

/* /// Global Variables /// */
orte_ras_base_module_t orte_ras_alps_module = {
    NULL,
    orte_ras_alps_allocate,
    NULL,
    orte_ras_alps_finalize
};

/* Parses: VAR_NAME=val text files - Pseudo INI */
static int
parser_ini(char **val_if_found, FILE *fp, const char *var_name)
{
    char *alps_config_str = NULL;

    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                        "ras:alps:allocate: parser_ini");

    /* invalid argument */
    if (NULL == val_if_found) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    *val_if_found = NULL;

    while ((alps_config_str = ras_alps_getline(fp))) {
        char *cpq;
        char *cpr;

        cpq = strchr(alps_config_str, '#'); /* Parse comments, actually ANY # */
        cpr = strchr(alps_config_str, '='); /* Parse for variables            */
        if (!cpr ||                         /* Skip if not definition         */
            (cpq && cpq < cpr)) {           /* Skip if commented              */
            free(alps_config_str);
            continue;
        }
        for (cpr--;                         /* Kill trailing whitespace       */
             (*cpr == ' ' || *cpr == '\t'); cpr--);
        for (cpq = alps_config_str;         /* Kill leading whitespace        */
             (*cpq == ' ' || *cpq == '\t'); cpq++);
        /* Filter to needed variable */
        if (strncmp(cpq, var_name, strlen(var_name))) {
            /* Sorry, not the variable name that we are looking for */
            free(alps_config_str);
            continue;
        }
        if (!(cpq = strchr(cpr, '"'))) {    /* Can't find pathname start      */
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        if (!(cpr = strchr(++cpq, '"'))) {  /* Can't find pathname end        */
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        *cpr = '\0';
        if (strlen(cpq) + 8 > PATH_MAX) {   /* Bad configuration              */
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        /* Success! */
        asprintf(val_if_found, "%s/appinfo", cpq);
        if (NULL == val_if_found) {
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        free(alps_config_str);
        return ORTE_SUCCESS;
    }
    /* We didn't find what we were looking for, but no unrecoverable errors
     * occurred in the process.
     * */
    return ORTE_SUCCESS;
}

/* Parses: VAR_NAME val text files */
static int
parser_separated_columns(char **val_if_found, FILE *fp, const char *var_name)
{
    char *alps_config_str = NULL;
    int var_len = strlen(var_name);
    int i;

    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                        "ras:alps:allocate: parser_separated_columns");

    /* invalid argument */
    if (NULL == val_if_found) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    *val_if_found = NULL;

    while ((alps_config_str = ras_alps_getline(fp))) {
        char *cpq = alps_config_str;
        char *cpr;

        /* Eat whitespace */
        while (' ' == *cpq || '\t' == *cpq) {
            cpq++;
        }
        /* Ignore comments and variable names that aren't what we are looking
         * for */
        if ('#' == *cpq || strncmp(cpq, var_name, var_len)) {
            free(alps_config_str);
            continue;
        }
        /* Move to end of the variable name */
        for (i = 0; i < var_len && '\0' != *cpq; ++i, ++cpq);
        /* Eat whitespace until we hit val */
        while (' ' == *cpq || '\t' == *cpq) {
            cpq++;
        }
        /* Now advance cpr until end of value */
        cpr = cpq;
        while ('\0' != *cpr && (' ' != *cpr || '\t' != *cpr)) {
            cpr++;
        }
        *cpr = '\0';
        /* Bad configuration sanity check */
        if (strlen(cpq) + 8 > PATH_MAX) {
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        /* Success! */
        asprintf(val_if_found, "%s/appinfo", cpq);
        if (NULL == val_if_found) {
            free(alps_config_str);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        free(alps_config_str);
        return ORTE_SUCCESS;
    }
    /* We didn't find what we were looking for, but no unrecoverable errors
     * occurred in the process.
     * */
    return ORTE_SUCCESS;
}

/* Gets ALPS scheduler information file pathname from system configuration.  On
 * our XK6 testbed, ALPS_SHARED_DIR_PATH isn't set in /etc/sysconfig/alps.  The
 * shared directory path is set in /etc/alps.conf and its corresponding variable
 * is named sharedDir.  We have to support both because XE6 systems (and
 * probably others) still rely on ALPS_SHARED_DIR_PATH and /etc/sysconfig/alps.
 */
static char *
orte_ras_get_appinfo_path(void)
{
    int i, rc = ORTE_ERROR;
    FILE *fp = NULL;
    char *appinfo_path = NULL;

    /* iterate over all the available ALPS system configurations name pairs
     * until we either fail or find what we are looking for.
     */
    for (i = 0; NULL != sysconfigs[i].path; ++i) {
        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: Trying ALPS configuration "
                            "file: \"%s\"",
                            sysconfigs[i].path);
        if (NULL == (fp = fopen(sysconfigs[i].path, "r"))) {
            int err = errno;
            opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                "ras:alps:allocate: Skipping ALPS "
                                "configuration file: \"%s\" (%s).",
                                sysconfigs[i].path, strerror(err));
            continue;
        }
        /* Let the search begin */
        rc = sysconfigs[i].parse(&appinfo_path, fp, sysconfigs[i].var_name);
        /* no longer needed */
        fclose(fp);

        if (ORTE_SUCCESS == rc) {
            /* Success! */
            if (NULL != appinfo_path) {
                break;
            }
            /* else we didn't find what we were looking for - just continue */
            else {
                continue;
            }
        }
        /* Failure */
        else {
            opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                 "ras:alps:allocate: failure "
                                 "(get_appinfo_dir_path = %d)", rc);
            return NULL;
        }
    }
    /* Were we successful? */
    if (NULL != sysconfigs[i].path) {
        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: Located ALPS scheduler file: "
                            "\"%s\"", appinfo_path);
        return appinfo_path;
    }
    /* Nope */
    else {
        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: Could not locate ALPS "
                            "scheduler file.");
        return NULL;
    }

    /* Never reached */
    return NULL;
}

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 */
static int
orte_ras_alps_allocate(orte_job_t *jdata, opal_list_t *nodes)
{
    int ret;
    char *appinfo_path = NULL;

    if (0 == orte_ras_alps_res_id) {
        orte_show_help("help-ras-alps.txt", "alps-env-var-not-found", 1);
        return ORTE_ERR_NOT_FOUND;
    }
    if (NULL == (appinfo_path = orte_ras_get_appinfo_path())) {
        return ORTE_ERR_NOT_FOUND;
    }
    /* Parse ALPS scheduler information file (appinfo) for node list. */
    if (ORTE_SUCCESS != (ret = orte_ras_alps_read_appinfo_file(
                                   nodes,
                                   appinfo_path,
                                   (unsigned int *)&orte_ras_alps_res_id))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /* Record the number of allocated nodes */
    orte_num_allocated_nodes = opal_list_get_size(nodes);

cleanup:
    /* All done */
    if (NULL != appinfo_path) {
        free(appinfo_path);
    }
    if (ORTE_SUCCESS == ret) {
        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: success");
    }
    else {
        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: failure "
                            "(base_allocate_nodes = %d)", ret);
    }
    return ret;
}

#define RAS_BASE_FILE_MAX_LINE_LENGTH (PATH_MAX * 2)

static char *
ras_alps_getline(FILE *fp)
{
    char *ret = NULL, *input = NULL;

    input = (char *)calloc(RAS_BASE_FILE_MAX_LINE_LENGTH + 1, sizeof(char));
    /* out of resources */
    if (NULL == input) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    ret = fgets(input, RAS_BASE_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input) - 1] = '\0';  /* remove newline */
        return input;
    }

    return NULL;
}

static int compare_nodes (opal_list_item_t **a, opal_list_item_t **b)
{
    orte_node_t *nodea = (orte_node_t *) *a;
    orte_node_t *nodeb = (orte_node_t *) *b;
    int32_t launcha, launchb, *ldptr;

    ldptr = &launcha;
    if (!orte_get_attribute(&nodea->attributes, ORTE_NODE_LAUNCH_ID, (void**)&ldptr, OPAL_INT32)) {
        return 0;
    }

    ldptr = &launchb;
    if (!orte_get_attribute(&nodeb->attributes, ORTE_NODE_LAUNCH_ID, (void**)&ldptr, OPAL_INT32)) {
        return 0;
    }

    return (launcha > launchb) ? 1 : -1;
}

#if ALPS_APPINFO_VERSION > 0 && ALPS_APPINFO_VERSION < 3
    typedef placeNodeList_t orte_ras_alps_placeNodeList_t;
#else
    typedef placeNodeList_ver3_t orte_ras_alps_placeNodeList_t;
#endif

static int
orte_ras_alps_read_appinfo_file(opal_list_t *nodes, char *filename,
                                unsigned int *uMe)
{
    int             iq;
    int             ix;
    int             iFd;                    /* file descriptor for appinfo    */
    int             iTrips;                 /* counter appinfo read attempts  */
    int             max_appinfo_read_attempts;
    struct stat     ssBuf;                  /* stat buffer                    */
    size_t          szLen;                  /* size of appinfo (file)         */
    off_t           oNow;                   /* current appinfo data offset    */
    off_t           oInfo=sizeof(appInfoHdr_t);
    off_t           oDet=sizeof(appInfo_t);
    off_t           oSlots;
    off_t           oEntry;
    int32_t         sNodes=0;
    char            *cpBuf;
    char            *hostname;
    orte_node_t     *node = NULL;
    appInfoHdr_t    *apHdr;                 /* ALPS header structure          */
    appInfo_t       *apInfo;                /* ALPS table info structure      */
#if ALPS_APPINFO_VERSION==0
    placeList_t     *apSlots;               /* ALPS node specific info        */
#else
    orte_ras_alps_placeNodeList_t *apNodes;
#endif

    orte_ras_alps_get_appinfo_attempts(&max_appinfo_read_attempts);
    oNow=0;
    iTrips=0;
    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                        "ras:alps:allocate: begin processing appinfo file");

    while(!oNow) {                          /* Until appinfo read is complete */
        iTrips++;                           /* Increment trip count           */

        iFd=open( filename, O_RDONLY );
        if( iFd==-1 ) {                     /* If file absent, ALPS is down   */
            opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                "ras:alps:allocate: ALPS information open failure");
            usleep(iTrips*50000);           /* Increasing delays, .05 s/try   */

            /*          Fail only when number of attempts have been exhausted.            */
            if( iTrips <= max_appinfo_read_attempts ) continue;
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        if( fstat( iFd, &ssBuf )==-1 ) {    /* If stat fails, access denied   */

            ORTE_ERROR_LOG(ORTE_ERR_NOT_AVAILABLE);
            return ORTE_ERR_NOT_AVAILABLE;
        }

        szLen=ssBuf.st_size;                /* Get buffer size                */
        cpBuf=malloc(szLen+1);              /* Allocate buffer                */
        if (NULL == cpBuf) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /*      Repeated attempts to read appinfo, with an increasing delay between   *
         *      successive attempts to allow scheduler I/O a chance to complete.      */
        if( (oNow=read( iFd, cpBuf, szLen ))!=(off_t)szLen ) {

            /*          This is where apstat fails; we will record it and try again.      */
            opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                "ras:alps:allocate: ALPS information read failure: %ld bytes", (long int)oNow);

            free(cpBuf);                    /* Free (old) buffer              */
            close(iFd);                     /* Close (old) descriptor         */
            oNow=0;                         /* Reset byte count               */
            usleep(iTrips*50000);           /* Increasing delays, .05 s/try   */

            /*          Fail only when number of attempts have been exhausted.            */
            if( iTrips<=max_appinfo_read_attempts ) continue;
            ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
            return ORTE_ERR_FILE_READ_FAILURE;
        }
    }
    close(iFd);
    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                        "ras:alps:allocate: file %s read", filename);

    /*  Now that we have the scheduler information, we just have to parse it for  *
     *  the data that we seek.                                                    */
    oNow=0;
    apHdr=(appInfoHdr_t *)cpBuf;

    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                        "ras:alps:allocate: %d entries in file", apHdr->apNum);

    /*  Header info (apHdr) tells us how many entries are in the file:            *
     *                                                                            *
     *      apHdr->apNum                                                          */

    for( iq=0; iq<apHdr->apNum; iq++ ) {    /*  Parse all entries in file     */

        /*      Just at this level, a lot of information is available:                *
         *                                                                            *
         *          apInfo->apid         ... ALPS job ID                              *
         *          apInfo->resId        ... ALPS reservation ID                      *
         *          apInfo->numCmds      ... Number of executables                    *
         *          apInfo->numPlaces    ... Number of PEs                            */
        apInfo=(appInfo_t *)(cpBuf+oNow+oInfo);

        /*      Calculate the dependent offsets.                                      */
        oSlots=sizeof(cmdDetail_t)*apInfo->numCmds;

        opal_output_verbose(1, orte_ras_base_framework.framework_output,
                            "ras:alps:allocate: read data for resId %u - myId %u",
                            apInfo->resId, *uMe);


#if ALPS_APPINFO_VERSION==0

        /*      Finally, we get to the actual node-specific information:              *
         *                                                                            *
         *          apSlots[ix].cmdIx    ... index of apDet[].cmd                     *
         *          apSlots[ix].nid      ... NodeID (NID)                             *
         *          apSlots[ix].procMask ... mask for processors... need 16-bit shift */
        apSlots=(placeList_t *)(cpBuf+oNow+oInfo+oDet+oSlots);
        oEntry=sizeof(placeList_t)*apInfo->numPlaces;

        oNow+=(oDet+oSlots+oEntry);         /* Target next slot               */

        if( apInfo->resId != *uMe ) continue; /* Filter to our reservation Id */

        /* in this early version of alps, there is one entry for each PE in the
         * allocation - so cycle across the numPlaces entries, assigning a slot
         * for each time a node is named
         */
        for( ix=0; ix<apInfo->numPlaces; ix++ ) {

            opal_output_verbose(5, orte_ras_base_framework.framework_output,
                                "ras:alps:read_appinfo: got NID %d", apSlots[ix].nid);

            asprintf( &hostname, "%d", apSlots[ix].nid );
            if (NULL == hostname) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /*          If this matches the prior nodename, just add to the slot count.   */
            if( NULL!=node && !strcmp(node->name, hostname) ) {

                free(hostname);             /* free hostname since not needed */
                ++node->slots;
            } else {                        /* must be new, so add to list    */

                opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                    "ras:alps:read_appinfo: added NID %d to list", apSlots[ix].nid);

                node = OBJ_NEW(orte_node_t);
                node->name = hostname;
                orte_set_attribute(&node->attributes, ORTE_NODE_LAUNCH_ID, ORTE_ATTR_LOCAL, &apSlots[ix].nid, OPAL_INT32);
                node->slots_inuse = 0;
                node->slots_max = 0;
                node->slots = 1;
                node->state = ORTE_NODE_STATE_UP;
                /* need to order these node ids so the regex generator
                 * can properly function
                 */
                /* add it to the end */
                opal_list_append(nodes, &node->super);
                sNodes++;                   /* Increment the node count       */
            }
        }
#else
        /* in newer versions of alps, there is one entry for each node in the
         * allocation, and that struct directly carries the number of PEs
         * allocated on that node to this job.
         */
        apNodes=(orte_ras_alps_placeNodeList_t *)(cpBuf+oNow+oInfo+oDet+oSlots);
        oEntry=sizeof(orte_ras_alps_placeNodeList_t)*apInfo->numPlaces;

        oNow+=(oDet+oSlots+oEntry);         /* Target next entry               */

        if( apInfo->resId != *uMe ) continue; /* Filter to our reservation Id */

        for( ix=0; ix<apInfo->numPlaces; ix++ ) {
            opal_output_verbose(5, orte_ras_base_framework.framework_output,
                                "ras:alps:read_appinfo(modern): processing NID %d with %d slots",
                                apNodes[ix].nid, apNodes[ix].numPEs);
            asprintf( &hostname, "%d", apNodes[ix].nid );
            if (NULL == hostname) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            node = OBJ_NEW(orte_node_t);
            node->name = hostname;
            orte_set_attribute(&node->attributes, ORTE_NODE_LAUNCH_ID, ORTE_ATTR_LOCAL, &apNodes[ix].nid, OPAL_INT32);
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = apNodes[ix].numPEs;
            node->state = ORTE_NODE_STATE_UP;
            /* need to order these node ids so the regex generator
             * can properly function
             */
            /* add it to the end */
            opal_list_append(nodes, &node->super);
            sNodes++;                   /* Increment the node count       */
        }
#endif
        break;                              /* Extended details ignored       */
    }

    opal_list_sort (nodes, compare_nodes);

    free(cpBuf);                            /* Free the buffer                */

    return ORTE_SUCCESS;
}

/* There's really nothing to do here */
static int
orte_ras_alps_finalize(void)
{
    opal_output_verbose(1, orte_ras_base_framework.framework_output,
                         "ras:alps:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}

