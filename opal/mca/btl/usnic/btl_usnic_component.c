/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * General notes:
 *
 * - OB1 handles out of order receives
 * - OB1 does NOT handle duplicate receives well (it probably does for
 *   MATCH tags, but for non-MATCH tags, it doesn't have enough info
 *   to know when duplicates are received), so we have to ensure not
 *   to pass duplicates up to the PML.
 */

#include "opal_config.h"

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rdma/fabric.h>

#include "opal_stdint.h"
#include "opal/prefetch.h"
#include "opal/mca/timer/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/net.h"
#include "opal/util/if.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/util/show_help.h"
#include "opal/constants.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/util/proc.h"
#else
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/proc/proc.h"
#endif

#include "btl_usnic.h"
#include "btl_usnic_connectivity.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_stats.h"
#include "btl_usnic_util.h"
#include "btl_usnic_ack.h"
#include "btl_usnic_send.h"
#include "btl_usnic_recv.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_test.h"

#define OPAL_BTL_USNIC_NUM_COMPLETIONS 500

/* RNG buffer definition */
opal_rng_buff_t opal_btl_usnic_rand_buff = {0};

/* simulated clock */
uint64_t opal_btl_usnic_ticks = 0;

static opal_event_t usnic_clock_timer_event;
static bool usnic_clock_timer_event_set = false;
static struct timeval usnic_clock_timeout;

/* set to true in a debugger to enable even more verbose output when calling
 * opal_btl_usnic_component_debug */
static volatile bool dump_bitvectors = false;

static int usnic_component_open(void);
static int usnic_component_close(void);
static mca_btl_base_module_t **
usnic_component_init(int* num_btl_modules, bool want_progress_threads,
                       bool want_mpi_threads);
static int usnic_component_progress(void);

/* Types for filtering interfaces */
typedef struct filter_elt_t {
    bool is_netmask;

    /* valid iff is_netmask==false */
    char *if_name;

    /* valid iff is_netmask==true */
    uint32_t addr_be; /* in network byte order */
    uint32_t netmask_be;
} filter_elt_t;

typedef struct usnic_if_filter_t {
    int n_elt;
    filter_elt_t *elts;
} usnic_if_filter_t;

static bool filter_module(opal_btl_usnic_module_t *module,
                          usnic_if_filter_t *filter,
                          bool filter_incl);
static usnic_if_filter_t *parse_ifex_str(const char *orig_str,
                                         const char *name);
static void free_filter(usnic_if_filter_t *filter);


opal_btl_usnic_component_t mca_btl_usnic_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        .btl_version = {
            USNIC_BTL_DEFAULT_VERSION("usnic"),
            .mca_open_component = usnic_component_open,
            .mca_close_component = usnic_component_close,
            .mca_register_component_params = opal_btl_usnic_component_register,
        },
        .btl_data = {
            /* The component is not checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_NONE
        },

        .btl_init = usnic_component_init,
        .btl_progress = usnic_component_progress,
    }
};


/*
 *  Called by MCA framework to open the component
 */
static int usnic_component_open(void)
{
    /* initialize state */
    mca_btl_usnic_component.num_modules = 0;
    mca_btl_usnic_component.usnic_all_modules = NULL;
    mca_btl_usnic_component.usnic_active_modules = NULL;
    mca_btl_usnic_component.transport_header_len = -1;
    mca_btl_usnic_component.prefix_send_offset = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_usnic_component.usnic_procs, opal_list_t);

    /* Sanity check: if_include and if_exclude need to be mutually
       exclusive */
    if (OPAL_SUCCESS !=
        mca_base_var_check_exclusive("opal",
            mca_btl_usnic_component.super.btl_version.mca_type_name,
            mca_btl_usnic_component.super.btl_version.mca_component_name,
            "if_include",
            mca_btl_usnic_component.super.btl_version.mca_type_name,
            mca_btl_usnic_component.super.btl_version.mca_component_name,
            "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return OPAL_SUCCESS;
}


/*
 * Component cleanup
 */
static int usnic_component_close(void)
{
    /* Note that this list should already be empty, because:
       - module.finalize() is invoked before component.close()
       - module.finalize() RELEASEs each proc that it was using
       - this should drive down the ref count on procs to 0
       - procs remove themselves from the component.usnic_procs list
         in their destructor */
    OBJ_DESTRUCT(&mca_btl_usnic_component.usnic_procs);

    if (usnic_clock_timer_event_set) {
        opal_event_del(&usnic_clock_timer_event);
        usnic_clock_timer_event_set = false;
    }

    /* Finalize the connectivity client and agent */
    if (mca_btl_usnic_component.connectivity_enabled) {
        opal_btl_usnic_connectivity_client_finalize();
        opal_btl_usnic_connectivity_agent_finalize();
    }
    if (mca_btl_usnic_component.opal_evbase) {
        opal_progress_thread_finalize(NULL);
    }

    free(mca_btl_usnic_component.usnic_all_modules);
    free(mca_btl_usnic_component.usnic_active_modules);

#if OPAL_BTL_USNIC_UNIT_TESTS
    /* clean up the unit test infrastructure */
    opal_btl_usnic_cleanup_tests();
#endif

    return OPAL_SUCCESS;
}


/*
 * Register address information.  The modex will make this available
 * to all peers.
 */
static int usnic_modex_send(void)
{
    int rc;
    int i;
    size_t size;
    opal_btl_usnic_modex_t* modexes = NULL;

    if (0 == mca_btl_usnic_component.num_modules) {
        return OPAL_SUCCESS;
    }

    size = mca_btl_usnic_component.num_modules *
        sizeof(opal_btl_usnic_modex_t);
    modexes = (opal_btl_usnic_modex_t*) malloc(size);
    if (NULL == modexes) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
        opal_btl_usnic_module_t* module =
            mca_btl_usnic_component.usnic_active_modules[i];
        modexes[i] = module->local_modex;
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: "
                            "control port:%d, "
                            "modex_send data port:%d, "
                            "%s",
                            modexes[i].ports[USNIC_PRIORITY_CHANNEL],
                            modexes[i].ports[USNIC_DATA_CHANNEL],
                            module->if_ipv4_addr_str);
    }

    usnic_compat_modex_send(&rc, &mca_btl_usnic_component.super.btl_version,
                            modexes, size);
    free(modexes);

    return rc;
}


/*
 * See if our memlock limit is >64K.  64K is the RHEL default memlock
 * limit; this check is a first-line-of-defense hueristic to see if
 * the user has set the memlock limit to *something*.
 *
 * We have other checks elsewhere (e.g., to ensure that QPs are able
 * to be allocated -- which also require registered memory -- and to
 * ensure that receive buffers can be registered, etc.), but this is a
 * good first check to ensure that a default OS case is satisfied.
 */
static int check_reg_mem_basics(void)
{
#if HAVE_DECL_RLIMIT_MEMLOCK
    int ret = OPAL_SUCCESS;
    struct rlimit limit;
    char *str_limit = NULL;

    ret = getrlimit(RLIMIT_MEMLOCK, &limit);
    if (0 == ret) {
        if ((long) limit.rlim_cur > (64 * 1024) ||
            limit.rlim_cur == RLIM_INFINITY) {
            return OPAL_SUCCESS;
        } else {
            asprintf(&str_limit, "%ld", (long)limit.rlim_cur);
        }
    } else {
        asprintf(&str_limit, "Unknown");
    }

    opal_show_help("help-mpi-btl-usnic.txt", "check_reg_mem_basics fail",
                   true,
                   opal_process_info.nodename,
                   str_limit);

    return OPAL_ERR_OUT_OF_RESOURCE;
#else
    /* If we don't have RLIMIT_MEMLOCK, then just bypass this
       safety/hueristic check. */
    return OPAL_SUCCESS;
#endif
}


/*
 * Basic sanity checking for usNIC VFs / resources.
 */
static int check_usnic_config(opal_btl_usnic_module_t *module,
        int num_local_procs)
{
    char str[128];
    unsigned unlp;
    struct fi_usnic_info *uip;
    struct fi_info *info;

    info = module->fabric_info;
    uip = &module->usnic_info;

    /* Note: we add one to num_local_procs to account for *this*
       process */
    unlp = (unsigned) num_local_procs + 1;

    /* usNIC allocates QPs as a combination of PCI virtual functions
       (VFs) and resources inside those VFs.  Ensure that:

       1. num_vfs (i.e., "usNICs") >= num_local_procs (to ensure that
          each MPI process will be able to have its own protection
          domain), and
       2. num_vfs * num_qps_per_vf >= num_local_procs * NUM_CHANNELS
          (to ensure that each MPI process will be able to get the
          number of QPs it needs -- we know that every VF will have
          the same number of QPs), and
       3. num_vfs * num_cqs_per_vf >= num_local_procs * NUM_CHANNELS
          (to ensure that each MPI process will be able to get the
          number of CQs that it needs) */
    if (uip->ui.v1.ui_num_vf < unlp) {
        snprintf(str, sizeof(str), "Not enough usNICs (found %d, need %d)",
                 uip->ui.v1.ui_num_vf, unlp);
        goto error;
    }

    if (uip->ui.v1.ui_num_vf * uip->ui.v1.ui_qp_per_vf <
        unlp * USNIC_NUM_CHANNELS) {
        snprintf(str, sizeof(str), "Not enough WQ/RQ (found %d, need %d)",
                 uip->ui.v1.ui_num_vf * uip->ui.v1.ui_qp_per_vf,
                 unlp * USNIC_NUM_CHANNELS);
        goto error;
    }
    if (uip->ui.v1.ui_num_vf * uip->ui.v1.ui_cq_per_vf <
        unlp * USNIC_NUM_CHANNELS) {
        snprintf(str, sizeof(str),
                 "Not enough CQ per usNIC (found %d, need %d)",
                 uip->ui.v1.ui_num_vf * uip->ui.v1.ui_cq_per_vf,
                 unlp * USNIC_NUM_CHANNELS);
        goto error;
    }

    /* All is good! */
    return OPAL_SUCCESS;

 error:
    /* Sad panda */
    opal_show_help("help-mpi-btl-usnic.txt",
                   "not enough usnic resources",
                   true,
                   opal_process_info.nodename,
                   info->fabric_attr->name,
                   str);
    return OPAL_ERROR;
}


static void usnic_clock_callback(int fd, short flags, void *timeout)
{
    /* 1ms == 1,000,000 ns */
    opal_btl_usnic_ticks += 1000000;

    /* run progress to make sure time change gets noticed */
    usnic_component_progress();

    opal_event_add(&usnic_clock_timer_event, timeout);
}


/* Parse a string which is a comma-separated list containing a mix of
 * interface names and IPv4 CIDR-format netmasks.
 *
 * Gracefully tolerates NULL pointer arguments by returning NULL.
 *
 * Returns a usnic_if_filter_t, which contains n_elt and a
 * corresponding array of found filter elements.  Caller is
 * responsible for freeing the returned usnic_if_filter_t, the array
 * of filter elements, and any strings in it (can do this via
 * free_filter()).
 */
static usnic_if_filter_t *parse_ifex_str(const char *orig_str,
                                         const char *name)
{
    int i, ret;
    char **argv, *str, *tmp;
    struct sockaddr_storage argv_inaddr;
    uint32_t argv_prefix, addr;
    usnic_if_filter_t *filter;
    int n_argv;

    if (NULL == orig_str) {
        return NULL;
    }

    /* Get a wrapper for the filter */
    filter = calloc(sizeof(*filter), 1);
    if (NULL == filter) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    argv = opal_argv_split(orig_str, ',');
    if (NULL == argv || 0 == (n_argv = opal_argv_count(argv))) {
        free(filter);
        opal_argv_free(argv);
        return NULL;
    }

    /* upper bound: each entry could be a mask */
    filter->elts = malloc(sizeof(*filter->elts) * n_argv);
    if (NULL == filter->elts) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        free(filter);
        opal_argv_free(argv);
        return NULL;
    }

    /* Shuffle iface names to the beginning of the argv array.  Process each
     * netmask as we encounter it and append the resulting value to netmask_t
     * array which we will return. */
    filter->n_elt = 0;
    for (i = 0; NULL != argv[i]; ++i) {
        /* assume that all interface names begin with an alphanumeric
         * character, not a number */
        if (isalpha(argv[i][0])) {
            filter->elts[filter->n_elt].is_netmask = false;
            filter->elts[filter->n_elt].if_name = strdup(argv[i]);
            opal_output_verbose(20, USNIC_OUT,
                                "btl:usnic:parse_ifex_str: parsed %s device name: %s",
                                name, filter->elts[filter->n_elt].if_name);

            ++filter->n_elt;
            continue;
        }

        /* Found a subnet notation.  Convert it to an IP
           address/netmask.  Get the prefix first. */
        argv_prefix = 0;
        tmp = strdup(argv[i]);
        str = strchr(argv[i], '/');
        if (NULL == str) {
            opal_show_help("help-mpi-btl-usnic.txt", "invalid if_inexclude",
                           true, name, opal_process_info.nodename,
                           tmp, "Invalid specification (missing \"/\")");
            free(tmp);
            continue;
        }
        *str = '\0';
        argv_prefix = atoi(str + 1);
        if (argv_prefix < 1 || argv_prefix > 32) {
            opal_show_help("help-mpi-btl-usnic.txt", "invalid if_inexclude",
                           true, name, opal_process_info.nodename,
                           tmp, "Invalid specification (prefix < 1 or prefix >32)");
            free(tmp);
            continue;
        }

        /* Now convert the IPv4 address */
        ((struct sockaddr*) &argv_inaddr)->sa_family = AF_INET;
        ret = inet_pton(AF_INET, argv[i],
                        &((struct sockaddr_in*) &argv_inaddr)->sin_addr);
        if (1 != ret) {
            opal_show_help("help-mpi-btl-usnic.txt", "invalid if_inexclude",
                           true, name, opal_process_info.nodename, tmp,
                           "Invalid specification (inet_pton() failed)");
            free(tmp);
            continue;
        }
        opal_output_verbose(20, USNIC_OUT,
                            "btl:usnic:parse_ifex_str: parsed %s address+prefix: %s / %u",
                            name,
                            opal_net_get_hostname((struct sockaddr*) &argv_inaddr),
                            argv_prefix);

        memcpy(&addr,
               &((struct sockaddr_in*) &argv_inaddr)->sin_addr,
               sizeof(addr));

        /* be helpful: if the user passed A.B.C.D/24 instead of A.B.C.0/24,
         * also normalize the netmask */
        filter->elts[filter->n_elt].is_netmask = true;
        filter->elts[filter->n_elt].if_name = NULL;
        filter->elts[filter->n_elt].netmask_be =
            usnic_cidrlen_to_netmask(argv_prefix);
        filter->elts[filter->n_elt].addr_be = addr &
            filter->elts[filter->n_elt].netmask_be;
        ++filter->n_elt;

        free(tmp);
    }
    assert(i == n_argv); /* sanity */

    opal_argv_free(argv);

    /* don't return an empty filter */
    if (filter->n_elt == 0) {
        free_filter(filter);
        return NULL;
    }

    return filter;
}

/*
 * Check this module to see if should be kept or not.
 */
static bool filter_module(opal_btl_usnic_module_t *module,
                          usnic_if_filter_t *filter,
                          bool filter_incl)
{
    int i;
    uint32_t module_mask;
    struct sockaddr_in *src;
    struct fi_usnic_info *uip;
    struct fi_info *info;
    bool match;

    info = module->fabric_info;
    uip = &module->usnic_info;
    src = info->src_addr;
    module_mask = src->sin_addr.s_addr & uip->ui.v1.ui_netmask_be;
    match = false;
    for (i = 0; i < filter->n_elt; ++i) {
        if (filter->elts[i].is_netmask) {
            /* conservative: we also require the netmask to match */
            if (filter->elts[i].netmask_be == uip->ui.v1.ui_netmask_be &&
                filter->elts[i].addr_be == module_mask) {
                match = true;
                break;
            }
        }
        else {
            if (strcmp(filter->elts[i].if_name, info->fabric_attr->name) == 0) {
                match = true;
                break;
            }
        }
    }

    /* Turn the match result into whether we should keep it or not */
    return match ^ !filter_incl;
}

/* utility routine to safely free a filter element array */
static void free_filter(usnic_if_filter_t *filter)
{
    int i;

    if (filter == NULL) {
        return;
    }

    if (NULL != filter->elts) {
        for (i = 0; i < filter->n_elt; ++i) {
            if (!filter->elts[i].is_netmask) {
                free(filter->elts[i].if_name);
            }
        }
        free(filter->elts);
    }
    free(filter);
}

/*
 *  UD component initialization:
 *  (1) read interface list from kernel and compare against component
 *      parameters then create a BTL instance for selected interfaces
 *  (2) post OOB receive for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */
static mca_btl_base_module_t** usnic_component_init(int* num_btl_modules,
                                                    bool want_progress_threads,
                                                    bool want_mpi_threads)
{
    mca_btl_base_module_t **btls = NULL;
    int i, j, num_final_modules;
    int num_devs;
    opal_btl_usnic_module_t *module;
    usnic_if_filter_t *filter = NULL;
    bool keep_module;
    bool filter_incl = false;
    int min_distance, num_local_procs;
    struct fi_info *info_list;
    struct fi_info *info;
    struct fi_info hints = {0};
    struct fi_ep_attr ep_attr = {0};
    struct fi_fabric_attr fabric_attr = {0};
    struct fid_fabric *fabric;
    struct fid_domain *domain;
    int ret;

    *num_btl_modules = 0;

    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (want_mpi_threads && !mca_btl_base_thread_multiple_override) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: MPI_THREAD_MULTIPLE not supported; skipping this component");
        return NULL;
    }

    /* We only want providers named "usnic that are of type EP_DGRAM */
    fabric_attr.prov_name = "usnic";
    ep_attr.type = FI_EP_DGRAM;

    hints.caps = FI_MSG;
    hints.mode = FI_LOCAL_MR | FI_MSG_PREFIX;
    hints.addr_format = FI_SOCKADDR;
    hints.ep_attr = &ep_attr;
    hints.fabric_attr = &fabric_attr;

    /* This code understands libfabric API v1.0 and v1.1.  Even if we
       were compiled with libfabric API v1.0, we still want to request
       v1.1 -- here's why:

       - In libfabric v1.0.0 (i.e., API v1.0), the usnic provider did
         not check the value of the "version" parameter passed into
         fi_getinfo()

       - If you pass FI_VERSION(1,0) to libfabric v1.1.0 (i.e., API
         v1.1), the usnic provider will disable FI_MSG_PREFIX support
         (on the assumption that the application will not handle
         FI_MSG_PREFIX properly).  This can happen if you compile OMPI
         against libfabric v1.0.0 (i.e., API v1.0) and run OMPI
         against libfabric v1.1.0 (i.e., API v1.1).

       So never request API v1.0 -- always request a minimum of
       v1.1.

       NOTE: The configure.m4 in this component will require libfabric
       >= v1.1.0 (i.e., it won't accept v1.0.0) because of a critical
       bug in the usnic provider in libfabric v1.0.0.  However, the
       compatibility code with libfabric v1.0.0 in the usNIC BTL has
       been retained, for two reasons:

       1. It's not harmful, nor overly complicated.  So the
          compatibility code was not ripped out.
       2. At least some versions of Cisco Open MPI are shipping with
          an embedded (libfabric v1.0.0+critical bug fix).

       Someday, #2 may no longer be true, and we may therefore rip out
       the libfabric v1.0.0 compatibility code. */
    uint32_t libfabric_api;
    libfabric_api = FI_VERSION(1, 1);
    ret = fi_getinfo(libfabric_api, NULL, 0, 0, &hints, &info_list);
    if (0 != ret) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: disqualifiying myself due to fi_getinfo failure: %s (%d)", strerror(-ret), ret);
        return NULL;
    }

    num_devs = 0;
    for (info = info_list; NULL != info; info = info->next) {
        ++num_devs;
    }
    if (0 == num_devs) {
        opal_output_verbose(5, USNIC_OUT,
            "btl:usnic: disqualifiying myself due to lack of libfabric providers");
        return NULL;
    }

    /* Do quick sanity check to ensure that we can lock memory (which
       is required for registered memory). */
    if (OPAL_SUCCESS != check_reg_mem_basics()) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: disqualifiying myself due to lack of lockable memory");
        return NULL;
    }

    /************************************************************************
     * Below this line, we assume that usnic is loaded on all procs,
     * and therefore we will guarantee to the the modex send, even if
     * we fail.
     ************************************************************************/

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: usNIC fabrics found");

    /* Due to ambiguities in documentation, in libfabric v1.0.0 (i.e.,
       API v1.0) the usnic provider returned sizeof(struct
       fi_cq_err_entry) from fi_cq_readerr() upon success.

       The ambiguities were clarified in libfabric v1.1.0 (i.e., API
       v1.1); the usnic provider returned 1 from fi_cq_readerr() upon
       success.

       So query to see what version of the libfabric API we are
       running with, and adapt accordingly. */
    libfabric_api = fi_version();
    if (1 == FI_MAJOR(libfabric_api) &&
        0 == FI_MINOR(libfabric_api)) {
        // Old fi_cq_readerr() behavior: success=sizeof(...), try again=0
        mca_btl_usnic_component.cq_readerr_success_value =
            sizeof(struct fi_cq_err_entry);
        mca_btl_usnic_component.cq_readerr_try_again_value = 0;
    } else {
        // New fi_cq_readerr() behavior: success=1, try again=-FI_EAGAIN
        mca_btl_usnic_component.cq_readerr_success_value = 1;
        mca_btl_usnic_component.cq_readerr_try_again_value = -FI_EAGAIN;
    }

    /* libnl initialization */
    opal_proc_t *me = opal_proc_local_get();
    opal_process_name_t *name = &(me->proc_name);
    mca_btl_usnic_component.my_hashed_rte_name =
        usnic_compat_rte_hash_name(name);
    MSGDEBUG1_OUT("%s: my_hashed_rte_name=0x%" PRIx64,
                   __func__, mca_btl_usnic_component.my_hashed_rte_name);

    opal_srand(&opal_btl_usnic_rand_buff, ((uint32_t) getpid()));

    /* Setup an array of pointers to point to each module (which we'll
       return upstream) */
    mca_btl_usnic_component.num_modules = num_devs;
    btls = (struct mca_btl_base_module_t**)
        malloc(mca_btl_usnic_component.num_modules *
               sizeof(opal_btl_usnic_module_t*));
    if (NULL == btls) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        goto send_modex;
    }

    /* Allocate space for btl module instances */
    mca_btl_usnic_component.usnic_all_modules =
        calloc(mca_btl_usnic_component.num_modules,
               sizeof(*mca_btl_usnic_component.usnic_all_modules));
    mca_btl_usnic_component.usnic_active_modules =
        calloc(mca_btl_usnic_component.num_modules,
               sizeof(*mca_btl_usnic_component.usnic_active_modules));
    if (NULL == mca_btl_usnic_component.usnic_all_modules ||
        NULL == mca_btl_usnic_component.usnic_active_modules) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        goto error;
    }

    /* If we have include or exclude list, parse and set up now
     * (higher level guarantees there will not be both include and exclude,
     * so don't bother checking that here)
     */
    if (NULL != mca_btl_usnic_component.if_include) {
        opal_output_verbose(20, USNIC_OUT,
                            "btl:usnic:filter_module: if_include=%s",
                            mca_btl_usnic_component.if_include);

        filter_incl = true;
        filter = parse_ifex_str(mca_btl_usnic_component.if_include, "include");
    } else if (NULL != mca_btl_usnic_component.if_exclude) {
        opal_output_verbose(20, USNIC_OUT,
                            "btl:usnic:filter_module: if_exclude=%s",
                            mca_btl_usnic_component.if_exclude);

        filter_incl = false;
        filter = parse_ifex_str(mca_btl_usnic_component.if_exclude, "exclude");
    }

    num_local_procs = opal_process_info.num_local_peers;

    /* Go through the list of devices and determine if we want it or
       not.  Create a module for each one that we want. */
    info = info_list;
    for (j = i = 0; i < num_devs &&
             (0 == mca_btl_usnic_component.max_modules ||
              i < mca_btl_usnic_component.max_modules);
             ++i, info = info->next) {

        ret = fi_fabric(info->fabric_attr, &fabric, NULL);
        if (0 != ret) {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "libfabric API failed",
                           true,
                           opal_process_info.nodename,
                           info->fabric_attr->name,
                           "fi_fabric()", __FILE__, __LINE__,
                           ret,
                           strerror(-ret));
            continue;
        }
        opal_memchecker_base_mem_defined(&fabric, sizeof(fabric));

        ret = fi_domain(fabric, info, &domain, NULL);
        if (0 != ret) {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "libfabric API failed",
                           true,
                           opal_process_info.nodename,
                           info->fabric_attr->name,
                           "fi_domain()", __FILE__, __LINE__,
                           ret,
                           strerror(-ret));
            continue;
        }
        opal_memchecker_base_mem_defined(&domain, sizeof(domain));

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: found: usNIC direct device %s",
                            info->fabric_attr->name);

        /* Save a little info on the module that we have already
           gathered.  The rest of the module will be filled in
           later. */
        module = &(mca_btl_usnic_component.usnic_all_modules[j]);
        memcpy(module, &opal_btl_usnic_module_template,
               sizeof(opal_btl_usnic_module_t));
        module->fabric = fabric;
        module->domain = domain;
        module->fabric_info = info;

        /* Obtain usnic-specific device info (e.g., netmask) that
           doesn't come in the normal fi_getinfo(). This allows us to
           do filtering, later. */
        ret = fi_open_ops(&fabric->fid, FI_USNIC_FABRIC_OPS_1, 0,
                (void **)&module->usnic_fabric_ops, NULL);
        if (ret != 0) {
            opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: device %s fabric_open_ops failed %d (%s)",
                        info->fabric_attr->name, ret, fi_strerror(-ret));
            fi_close(&domain->fid);
            fi_close(&fabric->fid);
            continue;
        }

        ret =
            module->usnic_fabric_ops->getinfo(FI_EXT_USNIC_INFO_VERSION,
                                            fabric,
                                            &module->usnic_info);
        if (ret != 0) {
            opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: device %s usnic_getinfo failed %d (%s)",
                        info->fabric_attr->name, ret, fi_strerror(-ret));
            fi_close(&domain->fid);
            fi_close(&fabric->fid);
            continue;
        }
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: device %s usnic_info: link speed=%d, netmask=0x%x, ifname=%s, num_vf=%d, qp/vf=%d, cq/vf=%d",
                            info->fabric_attr->name,
                            (unsigned int) module->usnic_info.ui.v1.ui_link_speed,
                            (unsigned int) module->usnic_info.ui.v1.ui_netmask_be,
                            module->usnic_info.ui.v1.ui_ifname,
                            module->usnic_info.ui.v1.ui_num_vf,
                            module->usnic_info.ui.v1.ui_qp_per_vf,
                            module->usnic_info.ui.v1.ui_cq_per_vf);

        /* respect if_include/if_exclude subnets/ifaces from the user */
        if (filter != NULL) {
            keep_module = filter_module(module, filter, filter_incl);
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: %s %s due to %s",
                                (keep_module ? "keeping" : "skipping"),
                                info->fabric_attr->name,
                                (filter_incl ? "if_include" : "if_exclude"));
            if (!keep_module) {
                fi_close(&domain->fid);
                fi_close(&fabric->fid);
                continue;
            }
        }

        /* The first time through, check some usNIC configuration
           minimum settings with information we got back from the fi_*
           probes (these are VIC-wide settings -- they don't change
           for each module we create, so we only need to check
           once). */
        if (0 == j &&
            check_usnic_config(module, num_local_procs) != OPAL_SUCCESS) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: device %s is not provisioned with enough resources -- skipping",
                                info->fabric_attr->name);
            fi_close(&domain->fid);
            fi_close(&fabric->fid);

            mca_btl_usnic_component.num_modules = 0;
            goto error;
        }

        /*************************************************/
        /* Below this point, we know we want this device */
        /*************************************************/

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: device %s looks good!",
                            info->fabric_attr->name);

        /* Let this module advance to the next round! */
        btls[j++] = &(module->super);
    }
    mca_btl_usnic_component.num_modules = j;

    /* free filter if created */
    if (filter != NULL) {
        free_filter(filter);
        filter = NULL;
    }

    /* If we actually have some modules, setup the connectivity
       checking agent and client. */
    if (mca_btl_usnic_component.num_modules > 0 &&
        mca_btl_usnic_component.connectivity_enabled) {
        mca_btl_usnic_component.opal_evbase = opal_progress_thread_init(NULL);
        if (OPAL_SUCCESS != opal_btl_usnic_connectivity_agent_init() ||
            OPAL_SUCCESS != opal_btl_usnic_connectivity_client_init()) {
            opal_progress_thread_finalize(NULL);
            return NULL;
        }
    }

    /* Now that we know how many modules there are, let the modules
       initialize themselves (it's useful to know how many modules
       there are before doing this). */
    for (num_final_modules = i = 0;
         i < mca_btl_usnic_component.num_modules; ++i) {
        module = (opal_btl_usnic_module_t*) btls[i];

        /* Let the module initialize itself */
        if (OPAL_SUCCESS != opal_btl_usnic_module_init(module)) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: failed to init module for %s",
                                module->if_ipv4_addr_str);
            continue;
        }

        /*************************************************/
        /* Below this point, we know we want this module */
        /*************************************************/

        /* If module_init() failed for any prior module, this will be
           a down shift in the btls[] array.  Otherwise, it's an
           overwrite of the same value. */
        btls[num_final_modules++] = &(module->super);

        /* Output all of this module's values. */
        const char *devname = module->fabric_info->fabric_attr->name;
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s num sqe=%d, num rqe=%d, num cqe=%d, num aveqe=%d",
                            devname,
                            module->sd_num,
                            module->rd_num,
                            module->cq_num,
                            module->av_eq_num);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s priority MTU = %" PRIsize_t,
                            devname,
                            module->max_tiny_msg_size);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s priority limit = %" PRIsize_t,
                            devname,
                            module->max_tiny_payload);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s eager limit = %" PRIsize_t,
                            devname,
                            module->super.btl_eager_limit);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s eager rndv limit = %" PRIsize_t,
                            devname,
                            module->super.btl_rndv_eager_limit);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s max send size= %" PRIsize_t
                            " (not overrideable)",
                            devname,
                            module->super.btl_max_send_size);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: %s exclusivity = %d",
                            devname,
                            module->super.btl_exclusivity);
    }

    /* We may have skipped some modules, so reset
       component.num_modules */
    mca_btl_usnic_component.num_modules = num_final_modules;

    /* We've packed all the modules and pointers to those modules in
       the lower ends of their respective arrays.  If not all the
       modules initialized successfully, we're wasting a little space.
       We could realloc and re-form the btls[] array, but it doesn't
       seem worth it.  Just waste a little space.

       That being said, if we ended up with zero acceptable devices,
       then free everything. */
    if (0 == num_final_modules) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: returning 0 modules");
        goto error;
    }

    /* we have a nonzero number of modules, so save a copy of the btls array
     * for later use */
    memcpy(mca_btl_usnic_component.usnic_active_modules, btls,
           num_final_modules * sizeof(*btls));

    /* Loop over the modules and find the minimum value for
       module->numa_distance.  For every module that has a
       numa_distance higher than the minimum value, increase its btl
       latency rating so that the PML will prefer to send short
       messages over "near" modules. */
    min_distance = 9999999;
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        module = (opal_btl_usnic_module_t*) btls[i];
        if (module->numa_distance < min_distance) {
            min_distance = module->numa_distance;
        }
    }
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        module = (opal_btl_usnic_module_t*) btls[i];
        if (module->numa_distance > min_distance) {
            ++module->super.btl_latency;
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: %s is far from me; increasing latency rating",
                                module->if_ipv4_addr_str);
        }
    }

    /* start timer to guarantee synthetic clock advances */
    opal_event_set(opal_sync_event_base, &usnic_clock_timer_event,
                   -1, 0, usnic_clock_callback,
                   &usnic_clock_timeout);
    usnic_clock_timer_event_set = true;

    /* 1ms timer */
    usnic_clock_timeout.tv_sec = 0;
    usnic_clock_timeout.tv_usec = 1000;
    opal_event_add(&usnic_clock_timer_event, &usnic_clock_timeout);

    /* Setup MPI_T performance variables */
    opal_btl_usnic_setup_mpit_pvars();

    /* All done */
    *num_btl_modules = mca_btl_usnic_component.num_modules;
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: returning %d modules", *num_btl_modules);

 send_modex:
    usnic_modex_send();
    return btls;

 error:
    /* clean up as much allocated memory as possible */
    free(btls);
    btls = NULL;
    free(mca_btl_usnic_component.usnic_all_modules);
    mca_btl_usnic_component.usnic_all_modules = NULL;
    free(mca_btl_usnic_component.usnic_active_modules);
    mca_btl_usnic_component.usnic_active_modules = NULL;

    /* free filter if created */
    if (filter != NULL) {
        free_filter(filter);
        filter = NULL;
    }

    goto send_modex;
}

/*
 * Component progress
 * The fast-path of an incoming packet available on the priority
 * receive queue is handled directly in this routine, everything else
 * is deferred to an external call, usnic_component_progress_2()
 * This helps keep usnic_component_progress() very small and very responsive
 * to a single incoming packet.  We make sure not to always return
 * immediately after one packet to avoid starvation, "fastpath_ok" is
 * used for this.
 */
static int usnic_handle_completion(opal_btl_usnic_module_t* module,
    opal_btl_usnic_channel_t *channel, struct fi_cq_entry *completion);
static int usnic_component_progress_2(void);
static void usnic_handle_cq_error(opal_btl_usnic_module_t* module,
    opal_btl_usnic_channel_t *channel, int cq_ret);

static int usnic_component_progress(void)
{
    int i;
    int count;
    opal_btl_usnic_recv_segment_t* rseg;
    opal_btl_usnic_module_t* module;
    struct fi_cq_entry completion;
    opal_btl_usnic_channel_t *channel;
    static bool fastpath_ok = true;

    /* update our simulated clock */
    opal_btl_usnic_ticks += 5000;

    count = 0;
    if (fastpath_ok) {
        for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
            module = mca_btl_usnic_component.usnic_active_modules[i];
            channel = &module->mod_channels[USNIC_PRIORITY_CHANNEL];

            assert(channel->chan_deferred_recv == NULL);

            int ret = fi_cq_read(channel->cq, &completion, 1);
            assert(0 != ret);
            if (OPAL_LIKELY(1 == ret)) {
                opal_memchecker_base_mem_defined(&completion,
                                                 sizeof(completion));
                rseg = (opal_btl_usnic_recv_segment_t*) completion.op_context;
                if (OPAL_LIKELY(OPAL_BTL_USNIC_SEG_RECV ==
                            rseg->rs_base.us_type)) {
                    opal_btl_usnic_recv_fast(module, rseg, channel);
                    fastpath_ok = false;    /* prevent starvation */
                    return 1;
                } else {
                    count += usnic_handle_completion(module, channel,
                                                     &completion);
                }
            } else if (OPAL_LIKELY(-FI_EAGAIN == ret)) {
                continue;
            } else {
                usnic_handle_cq_error(module, channel, ret);
            }
        }
    }

    fastpath_ok = true;
    return count + usnic_component_progress_2();
}

static int usnic_handle_completion(
    opal_btl_usnic_module_t* module,
    opal_btl_usnic_channel_t *channel,
    struct fi_cq_entry *completion)
{
    opal_btl_usnic_segment_t* seg;
    opal_btl_usnic_recv_segment_t* rseg;

    seg = (opal_btl_usnic_segment_t*)completion->op_context;
    rseg = (opal_btl_usnic_recv_segment_t*)seg;

    /* Make the completion be Valgrind-defined */
    opal_memchecker_base_mem_defined(seg, sizeof(*seg));

    /* Handle work completions */
    switch(seg->us_type) {

    /**** Send ACK completions ****/
    case OPAL_BTL_USNIC_SEG_ACK:
        opal_btl_usnic_ack_complete(module,
                (opal_btl_usnic_ack_segment_t *)seg);
        break;

    /**** Send of frag segment completion ****/
    case OPAL_BTL_USNIC_SEG_FRAG:
        opal_btl_usnic_frag_send_complete(module,
                (opal_btl_usnic_frag_segment_t*)seg);
        break;

    /**** Send of chunk segment completion ****/
    case OPAL_BTL_USNIC_SEG_CHUNK:
        opal_btl_usnic_chunk_send_complete(module,
                (opal_btl_usnic_chunk_segment_t*)seg);
        break;

    /**** Receive completions ****/
    case OPAL_BTL_USNIC_SEG_RECV:
        opal_btl_usnic_recv(module, rseg, channel);
        break;

    default:
        BTL_ERROR(("Unhandled completion segment type %d", seg->us_type));
        break;
    }
    return 1;
}

static void
usnic_handle_cq_error(opal_btl_usnic_module_t* module,
    opal_btl_usnic_channel_t *channel, int cq_ret)
{
    int rc;
    struct fi_cq_err_entry err_entry;
    opal_btl_usnic_recv_segment_t* rseg;

    if (cq_ret != -FI_EAVAIL) {
        BTL_ERROR(("%s: cq_read ret = %d (%s)",
               module->fabric_info->fabric_attr->name, cq_ret,
               fi_strerror(-cq_ret)));
        channel->chan_error = true;
    }

    rc = fi_cq_readerr(channel->cq, &err_entry, 0);
    if (rc == mca_btl_usnic_component.cq_readerr_try_again_value) {
        return;
    } else if (rc != mca_btl_usnic_component.cq_readerr_success_value) {
        BTL_ERROR(("%s: cq_readerr ret = %d (expected %d)",
                   module->fabric_info->fabric_attr->name, rc,
                   (int) mca_btl_usnic_component.cq_readerr_success_value));
        channel->chan_error = true;
    }

    /* Silently count CRC errors.  Truncation errors are usually a
       different symptom of a CRC error. */
    else if (FI_ECRC == err_entry.prov_errno ||
             FI_ETRUNC == err_entry.prov_errno) {
#if MSGDEBUG1
        static int once = 0;
        if (once++ == 0) {
            BTL_ERROR(("%s: Channel %d, %s",
                       module->fabric_info->fabric_attr->name,
                       channel->chan_index,
                       FI_ECRC == err_entry.prov_errno ?
                       "CRC error" : "message truncation"));
        }
#endif

        /* silently count CRC errors */
        ++module->stats.num_crc_errors;

        /* repost segment */
        ++module->stats.num_recv_reposts;

        /* Add recv to linked list for reposting */
        rseg = err_entry.op_context;
        if (OPAL_BTL_USNIC_SEG_RECV == rseg->rs_base.us_type) {
            rseg->rs_next = channel->repost_recv_head;
            channel->repost_recv_head = rseg;
        }
    } else {
        BTL_ERROR(("%s: CQ[%d] prov_err = %d",
               module->fabric_info->fabric_attr->name, channel->chan_index,
                   err_entry.prov_errno));
        channel->chan_error = true;
    }
}

static int usnic_component_progress_2(void)
{
    int i, j, count = 0, num_events, ret;
    opal_btl_usnic_module_t* module;
    static struct fi_cq_entry completions[OPAL_BTL_USNIC_NUM_COMPLETIONS];
    opal_btl_usnic_channel_t *channel;
    int rc;
    int c;

    /* update our simulated clock */
    opal_btl_usnic_ticks += 5000;

    /* Poll for completions */
    for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
        module = mca_btl_usnic_component.usnic_active_modules[i];

        /* poll each channel */
        for (c=0; c<USNIC_NUM_CHANNELS; ++c) {
            channel = &module->mod_channels[c];

            if (channel->chan_deferred_recv != NULL) {
                (void) opal_btl_usnic_recv_frag_bookkeeping(module,
                        channel->chan_deferred_recv, channel);
                channel->chan_deferred_recv = NULL;
            }

            num_events = ret =
                fi_cq_read(channel->cq, completions,
                           OPAL_BTL_USNIC_NUM_COMPLETIONS);
            assert(0 != ret);
            opal_memchecker_base_mem_defined(&ret, sizeof(ret));
            if (OPAL_UNLIKELY(ret < 0 && -FI_EAGAIN != ret)) {
                usnic_handle_cq_error(module, channel, num_events);
                num_events = 0;
            } else if (-FI_EAGAIN == ret) {
                num_events = 0;
            }

            opal_memchecker_base_mem_defined(completions,
                                             sizeof(completions[0]) *
                                             num_events);
            /* Handle each event */
            for (j = 0; j < num_events; j++) {
                count += usnic_handle_completion(module, channel,
                                                 &completions[j]);
            }

            /* return error if detected - this may be slightly deferred
             * since fastpath avoids the "if" of checking this.
             */
            if (channel->chan_error) {
                channel->chan_error = false;
                return OPAL_ERROR;
            }

            /* progress sends */
            opal_btl_usnic_module_progress_sends(module);

            /* Re-post all the remaining receive buffers */
            if (OPAL_LIKELY(NULL != channel->repost_recv_head)) {
                rc = opal_btl_usnic_post_recv_list(channel);
                if (OPAL_UNLIKELY(rc != 0)) {
                    BTL_ERROR(("error posting recv: %s\n", strerror(errno)));
                    return OPAL_ERROR;
                }
            }
        }
    }

    return count;
}

/* could take indent as a parameter instead of hard-coding it */
static void dump_endpoint(opal_btl_usnic_endpoint_t *endpoint)
{
    int i;
    opal_btl_usnic_frag_t *frag;
    opal_btl_usnic_send_segment_t *sseg;
    struct in_addr ia;
    char ep_addr_str[INET_ADDRSTRLEN];
    char tmp[128], str[2048];

    memset(ep_addr_str, 0x00, sizeof(ep_addr_str));
    ia.s_addr = endpoint->endpoint_remote_modex.ipv4_addr;
    inet_ntop(AF_INET, &ia, ep_addr_str, sizeof(ep_addr_str));

    opal_output(0, "    endpoint %p, %s job=%u, rank=%u rts=%s s_credits=%"PRIi32"\n",
                (void *)endpoint, ep_addr_str,
                endpoint->endpoint_proc->proc_opal->proc_name.jobid,
                endpoint->endpoint_proc->proc_opal->proc_name.vpid,
                (endpoint->endpoint_ready_to_send ? "true" : "false"),
                endpoint->endpoint_send_credits);
    opal_output(0, "      endpoint->frag_send_queue:\n");

    OPAL_LIST_FOREACH(frag, &endpoint->endpoint_frag_send_queue,
                      opal_btl_usnic_frag_t) {
        opal_btl_usnic_small_send_frag_t *ssfrag;
        opal_btl_usnic_large_send_frag_t *lsfrag;

        snprintf(str, sizeof(str), "      --> frag %p, %s", (void *)frag,
                 usnic_frag_type(frag->uf_type));
        switch (frag->uf_type) {
            case OPAL_BTL_USNIC_FRAG_LARGE_SEND:
                lsfrag = (opal_btl_usnic_large_send_frag_t *)frag;
                snprintf(tmp, sizeof(tmp), " tag=%"PRIu8" id=%"PRIu32" offset=%llu/%llu post_cnt=%"PRIu32" ack_bytes_left=%llu\n",
                        lsfrag->lsf_tag,
                        lsfrag->lsf_frag_id,
                        (unsigned long long)lsfrag->lsf_cur_offset,
                        (unsigned long long)lsfrag->lsf_base.sf_size,
                        lsfrag->lsf_base.sf_seg_post_cnt,
                        (unsigned long long)lsfrag->lsf_base.sf_ack_bytes_left);
                strncat(str, tmp, sizeof(str) - strlen(str) - 1);
                opal_output(0, "%s", str);

                OPAL_LIST_FOREACH(sseg, &lsfrag->lsf_seg_chain,
                                  opal_btl_usnic_send_segment_t) {
                    /* chunk segs are just typedefs to send segs */
                    opal_output(0, "        chunk seg %p, chan=%s hotel=%d times_posted=%"PRIu32" pending=%s\n",
                                (void *)sseg,
                                (USNIC_PRIORITY_CHANNEL == sseg->ss_channel ?
                                "prio" : "data"),
                                sseg->ss_hotel_room,
                                sseg->ss_send_posted,
                                (sseg->ss_ack_pending ? "true" : "false"));
                }
            break;

            case OPAL_BTL_USNIC_FRAG_SMALL_SEND:
                ssfrag = (opal_btl_usnic_small_send_frag_t *)frag;
                snprintf(tmp, sizeof(tmp), " sf_size=%llu post_cnt=%"PRIu32" ack_bytes_left=%llu\n",
                        (unsigned long long)ssfrag->ssf_base.sf_size,
                        ssfrag->ssf_base.sf_seg_post_cnt,
                        (unsigned long long)ssfrag->ssf_base.sf_ack_bytes_left);
                strncat(str, tmp, sizeof(str) - strlen(str) - 1);
                opal_output(0, "%s", str);

                sseg = &ssfrag->ssf_segment;
                opal_output(0, "        small seg %p, chan=%s hotel=%d times_posted=%"PRIu32" pending=%s\n",
                    (void *)sseg,
                    (USNIC_PRIORITY_CHANNEL == sseg->ss_channel ?
                        "prio" : "data"),
                    sseg->ss_hotel_room,
                    sseg->ss_send_posted,
                    (sseg->ss_ack_pending ? "true" : "false"));
            break;

            case OPAL_BTL_USNIC_FRAG_PUT_DEST:
                /* put_dest frags are just a typedef to generic frags */
                snprintf(tmp, sizeof(tmp), " put_addr=%p\n", frag->uf_remote_seg[0].seg_addr.pval);
                strncat(str, tmp, sizeof(str) - strlen(str) - 1);
                opal_output(0, "%s", str);
            break;
        }
    }

    /* Now examine the hotel for this endpoint and dump any segments we find
     * there.  Yes, this peeks at members that are technically "private", so
     * eventually this should be done through some sort of debug or iteration
     * interface in the hotel code. */
    opal_output(0, "      endpoint->endpoint_sent_segs (%p):\n",
           (void *)endpoint->endpoint_sent_segs);
    for (i = 0; i < WINDOW_SIZE; ++i) {
        sseg = endpoint->endpoint_sent_segs[i];
        if (NULL != sseg) {
            opal_output(0, "        [%d] sseg=%p %s chan=%s hotel=%d times_posted=%"PRIu32" pending=%s\n",
                   i,
                   (void *)sseg,
                   usnic_seg_type_str(sseg->ss_base.us_type),
                   (USNIC_PRIORITY_CHANNEL == sseg->ss_channel ?
                    "prio" : "data"),
                   sseg->ss_hotel_room,
                   sseg->ss_send_posted,
                   (sseg->ss_ack_pending ? "true" : "false"));
        }
    }

    opal_output(0, "      ack_needed=%s n_t=%"UDSEQ" n_a=%"UDSEQ" n_r=%"UDSEQ" n_s=%"UDSEQ" rfstart=%"PRIu32"\n",
                (endpoint->endpoint_ack_needed?"true":"false"),
                endpoint->endpoint_next_seq_to_send,
                endpoint->endpoint_ack_seq_rcvd,
                endpoint->endpoint_next_contig_seq_to_recv,
                endpoint->endpoint_highest_seq_rcvd,
                endpoint->endpoint_rfstart);

    if (dump_bitvectors) {
        opal_btl_usnic_snprintf_bool_array(str, sizeof(str),
                                           endpoint->endpoint_rcvd_segs,
                                           WINDOW_SIZE);
        opal_output(0, "      rcvd_segs 0x%s", str);
    }
}

void opal_btl_usnic_component_debug(void)
{
    int i;
    opal_btl_usnic_module_t *module;
    opal_btl_usnic_endpoint_t *endpoint;
    opal_btl_usnic_send_segment_t *sseg;
    opal_list_item_t *item;
    const opal_proc_t *proc = opal_proc_local_get();

    opal_output(0, "*** dumping usnic state for MPI_COMM_WORLD rank %u ***\n",
                proc->proc_name.vpid);
    for (i = 0; i < (int)mca_btl_usnic_component.num_modules; ++i) {
        module = mca_btl_usnic_component.usnic_active_modules[i];

        opal_output(0, "active_modules[%d]=%p %s max{frag,chunk,tiny}=%llu,%llu,%llu\n",
               i, (void *)module, module->fabric_info->fabric_attr->name,
               (unsigned long long)module->max_frag_payload,
               (unsigned long long)module->max_chunk_payload,
               (unsigned long long)module->max_tiny_payload);

        opal_output(0, "  endpoints_with_sends:\n");
        OPAL_LIST_FOREACH(endpoint, &module->endpoints_with_sends,
                          opal_btl_usnic_endpoint_t) {
            dump_endpoint(endpoint);
        }

        opal_output(0, "  endpoints_that_need_acks:\n");
        OPAL_LIST_FOREACH(endpoint, &module->endpoints_that_need_acks,
                          opal_btl_usnic_endpoint_t) {
            dump_endpoint(endpoint);
        }

        /* the all_endpoints list uses a different list item member */
        opal_output(0, "  all_endpoints:\n");
        opal_mutex_lock(&module->all_endpoints_lock);
        item = opal_list_get_first(&module->all_endpoints);
        while (item != opal_list_get_end(&module->all_endpoints)) {
            endpoint = container_of(item, mca_btl_base_endpoint_t,
                                    endpoint_endpoint_li);
            item = opal_list_get_next(item);
            dump_endpoint(endpoint);
        }
        opal_mutex_unlock(&module->all_endpoints_lock);

        opal_output(0, "  pending_resend_segs:\n");
        OPAL_LIST_FOREACH(sseg, &module->pending_resend_segs,
                          opal_btl_usnic_send_segment_t) {
            opal_output(0, "    sseg %p\n", (void *)sseg);
        }

        opal_btl_usnic_print_stats(module, "  manual", /*reset=*/false);
    }
}

#include "test/btl_usnic_component_test.h"
