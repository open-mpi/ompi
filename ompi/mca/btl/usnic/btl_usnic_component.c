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
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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

#include "ompi_config.h"

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <infiniband/verbs.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "opal_stdint.h"
#include "opal/prefetch.h"
#include "opal/mca/timer/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/net.h"
#include "opal/util/if.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/util/show_help.h"

#include "ompi/constants.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/common/verbs/common_verbs.h"

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
#include "btl_usnic_ext.h"
#include "btl_usnic_test.h"

#define OMPI_BTL_USNIC_NUM_WC       500
#define max(a,b) ((a) > (b) ? (a) : (b))

/* RNG buffer definition */
opal_rng_buff_t ompi_btl_usnic_rand_buff;

/* simulated clock */
uint64_t ompi_btl_usnic_ticks = 0;

static opal_event_t usnic_clock_timer_event;
static bool usnic_clock_timer_event_set = false;
static struct timeval usnic_clock_timeout;

/* set to true in a debugger to enable even more verbose output when calling
 * ompi_btl_usnic_component_debug */
static volatile bool dump_bitvectors = false;

static int usnic_component_open(void);
static int usnic_component_close(void);
static mca_btl_base_module_t **
usnic_component_init(int* num_btl_modules, bool want_progress_threads,
                       bool want_mpi_threads);
static int usnic_component_progress(void);
static int init_module_from_port(ompi_btl_usnic_module_t *module,
                                 ompi_common_verbs_port_item_t *port);

/* Types for filtering interfaces */
typedef struct filter_elt_t {
    bool is_netmask;

    /* valid iff is_netmask==false */
    char *if_name;

    /* valid iff is_netmask==true */
    uint32_t addr; /* in network byte order */
    uint32_t prefixlen;
} filter_elt_t;

typedef struct usnic_if_filter_t {
    int n_elt;
    filter_elt_t *elts;
} usnic_if_filter_t;

static bool filter_module(ompi_btl_usnic_module_t *module,
                          usnic_if_filter_t *filter,
                          bool filter_incl);
static usnic_if_filter_t *parse_ifex_str(const char *orig_str,
                                         const char *name);
static void free_filter(usnic_if_filter_t *filter);


ompi_btl_usnic_component_t mca_btl_usnic_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "usnic", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            usnic_component_open,  /* component open */
            usnic_component_close,  /* component close */
            NULL, /* component query */
            ompi_btl_usnic_component_register, /* component register */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        usnic_component_init,
        usnic_component_progress,
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

    /* In this version, the USNIC stack does not support having more
     * than one GID.  So just hard-wire this value to 0. */
    mca_btl_usnic_component.gid_index = 0;
    
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_usnic_component.usnic_procs, opal_list_t);
    
    /* Sanity check: if_include and if_exclude need to be mutually
       exclusive */
    if (OPAL_SUCCESS != 
        mca_base_var_check_exclusive("ompi",
            mca_btl_usnic_component.super.btl_version.mca_type_name,
            mca_btl_usnic_component.super.btl_version.mca_component_name,
            "if_include",
            mca_btl_usnic_component.super.btl_version.mca_type_name,
            mca_btl_usnic_component.super.btl_version.mca_component_name,
            "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OMPI_ERR_NOT_AVAILABLE;
    }
    
    return OMPI_SUCCESS;
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
        ompi_btl_usnic_connectivity_client_finalize();
        ompi_btl_usnic_connectivity_agent_finalize();
    }

    free(mca_btl_usnic_component.usnic_all_modules);
    free(mca_btl_usnic_component.usnic_active_modules);

    ompi_btl_usnic_rtnl_sk_free(mca_btl_usnic_component.unlsk);

#if OMPI_BTL_USNIC_UNIT_TESTS
    /* clean up the unit test infrastructure */
    ompi_btl_usnic_cleanup_tests();
#endif

    return OMPI_SUCCESS;
}


/*
 * Register UD address information.  The MCA framework will make this
 * available to all peers.
 */
static int usnic_modex_send(void)
{
    int rc;
    size_t i;
    size_t size;
    ompi_btl_usnic_addr_t* addrs = NULL;

    if (0 == mca_btl_usnic_component.num_modules) {
        return OMPI_SUCCESS;
    }

    size = mca_btl_usnic_component.num_modules * 
        sizeof(ompi_btl_usnic_addr_t);
    addrs = (ompi_btl_usnic_addr_t*) malloc(size);
    if (NULL == addrs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
        ompi_btl_usnic_module_t* module =
            mca_btl_usnic_component.usnic_active_modules[i];
        addrs[i] = module->local_addr;
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: modex_send DQP:%d, CQP:%d, subnet = 0x%016" PRIx64 " interface =0x%016" PRIx64,
                            addrs[i].qp_num[USNIC_DATA_CHANNEL],
                            addrs[i].qp_num[USNIC_PRIORITY_CHANNEL],
                            ntoh64(addrs[i].gid.global.subnet_prefix),
                            ntoh64(addrs[i].gid.global.interface_id));
    }

    rc = ompi_modex_send(&mca_btl_usnic_component.super.btl_version,
                         addrs, size);
    free(addrs);

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
    int ret = OMPI_SUCCESS;
    struct rlimit limit;
    char *str_limit = NULL;

    ret = getrlimit(RLIMIT_MEMLOCK, &limit);
    if (0 == ret) {
        if ((long) limit.rlim_cur > (64 * 1024) ||
            limit.rlim_cur == RLIM_INFINITY) {
            return OMPI_SUCCESS;
	} else {
            asprintf(&str_limit, "%ld", (long)limit.rlim_cur);
	}
    } else {
        asprintf(&str_limit, "Unknown");
    }

    opal_show_help("help-mpi-btl-usnic.txt", "check_reg_mem_basics fail",
                   true,
                   ompi_process_info.nodename,
                   str_limit);

    return OMPI_ERR_OUT_OF_RESOURCE;
#else
    /* If we don't have RLIMIT_MEMLOCK, then just bypass this
       safety/hueristic check. */
    return OMPI_SUCCESS;
#endif
}


static int read_device_sysfs(ompi_btl_usnic_module_t *module, const char *name)
{
    int ret, fd;
    char filename[OPAL_PATH_MAX], line[256];

    snprintf(filename, sizeof(filename), "/sys/class/infiniband/%s/%s",
             ibv_get_device_name(module->device), name);
    fd = open(filename, O_RDONLY);
    if (fd < 0) {
        return -1;
    }

    ret = read(fd, line, sizeof(line));
    close(fd);
    if (ret < 0) {
        return -1;
    }

    return atoi(line);
}

static int check_usnic_config(struct ibv_device_attr *device_attr,
                              ompi_btl_usnic_module_t *module,
                              int num_local_procs)
{
    char str[128];
    int num_vfs, qp_per_vf, cq_per_vf;

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
    num_vfs = read_device_sysfs(module, "max_vf");
    qp_per_vf = read_device_sysfs(module, "qp_per_vf");
    cq_per_vf = read_device_sysfs(module, "cq_per_vf");
    if (num_vfs < 0 || qp_per_vf < 0 || cq_per_vf < 0) {
        snprintf(str, sizeof(str), "Cannot read usNIC Linux verbs resources");
        goto error;
    }

    if (num_vfs < num_local_procs) {
        snprintf(str, sizeof(str), "Not enough usNICs (found %d, need %d)",
                 num_vfs, num_local_procs);
        goto error;
    }

    if (num_vfs * qp_per_vf < num_local_procs * USNIC_NUM_CHANNELS) {
        snprintf(str, sizeof(str), "Not enough WQ/RQ (found %d, need %d)",
                 num_vfs * qp_per_vf,
                 num_local_procs * USNIC_NUM_CHANNELS);
        goto error;
    }
    if (num_vfs * cq_per_vf < num_local_procs * USNIC_NUM_CHANNELS) {
        snprintf(str, sizeof(str), "Not enough CQ per usNIC (found %d, need %d)",
                 num_vfs * cq_per_vf,
                 num_local_procs * USNIC_NUM_CHANNELS);
        goto error;
    }

    /* All is good! */
    return OMPI_SUCCESS;

 error:
    /* Sad panda */
    opal_show_help("help-mpi-btl-usnic.txt",
                   "not enough usnic resources",
                   true,
                   ompi_process_info.nodename,
                   ibv_get_device_name(module->device),
                   str);
    return OMPI_ERROR;
}


static void
usnic_clock_callback(int fd, short flags, void *timeout)
{
    /* 1ms == 1,000,000 ns */
    ompi_btl_usnic_ticks += 1000000;

    /* run progress to make sure time change gets noticed */
    usnic_component_progress();

    opal_event_add(&usnic_clock_timer_event, timeout);
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
    uint32_t i, num_final_modules;
    ompi_btl_usnic_module_t *module;
    opal_list_item_t *item;
    opal_list_t *port_list;
    ompi_common_verbs_port_item_t *port;
    struct ibv_device_attr device_attr;
    usnic_if_filter_t *filter;
    bool keep_module;
    bool filter_incl = false;
    int min_distance, num_local_procs, err;

    *num_btl_modules = 0;

    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: MPI_THREAD_MULTIPLE not supported; skipping this component");
        return NULL;
    }

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/1305, check to
       see if $sysfsdir/class/infiniband exists.  If it does not,
       assume that the RDMA hardware drivers are not loaded, and
       therefore we don't want OpenFabrics verbs support in this OMPI
       job.  No need to print a warning. */
    if (!ompi_common_verbs_check_basics()) {
        return NULL;
    }

    /* Do quick sanity check to ensure that we can lock memory (which
       is required for verbs registered memory). */
    if (OMPI_SUCCESS != check_reg_mem_basics()) {
        return NULL;
    }

    /************************************************************************
     * Below this line, we assume that usnic is loaded on all procs,
     * and therefore we will guarantee to the the modex send, even if
     * we fail.
     ************************************************************************/

    /* initialization */
    mca_btl_usnic_component.my_hashed_rte_name = 
        ompi_rte_hash_name(&(ompi_proc_local()->proc_name));
    MSGDEBUG1_OUT("%s: my_hashed_rte_name=0x%" PRIx64,
                   __func__, mca_btl_usnic_component.my_hashed_rte_name);

    opal_srand(&ompi_btl_usnic_rand_buff, ((uint32_t) getpid()));

    err = ompi_btl_usnic_rtnl_sk_alloc(&mca_btl_usnic_component.unlsk);
    if (0 != err) {
        /* API returns negative errno values */
        opal_show_help("help-mpi-btl-usnic.txt", "rtnetlink init fail",
                       true, ompi_process_info.nodename, strerror(-err));
        return NULL;
    }

    /* Find the ports that we want to use.  We do our own interface name
     * filtering below, so don't let the verbs code see our
     * if_include/if_exclude strings */
    port_list = ompi_common_verbs_find_ports(NULL, NULL,
                                             OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC_UDP,
                                             USNIC_OUT);
    if (NULL == port_list) {
        OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto free_include_list;
    } else if (opal_list_get_size(port_list) > 0) {
        mca_btl_usnic_component.use_udp = true;
        opal_output_verbose(5, USNIC_OUT, "btl:usnic: using UDP transport");
    } else {
        OBJ_RELEASE(port_list);

        /* If we got no USNIC_UDP transport devices, try again with
           USNIC */
        port_list = ompi_common_verbs_find_ports(NULL, NULL,
                                                 OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC,
                                                 USNIC_OUT);

        if (NULL == port_list) {
            OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto free_include_list;
        } else if (opal_list_get_size(port_list) > 0) {
            mca_btl_usnic_component.use_udp = false;
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: using L2-only transport");
        } else {
            /* There's no usNICs, so bail... */
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: no usNICs found");
            goto free_include_list;
        }
    }

    /* Setup the connectivity checking agent and client. */
    if (mca_btl_usnic_component.connectivity_enabled) {
        if (OMPI_SUCCESS != ompi_btl_usnic_connectivity_agent_init() ||
            OMPI_SUCCESS != ompi_btl_usnic_connectivity_client_init()) {
            return NULL;
        }
    }

    /* Initialize the table of usnic extension function pointers */
    item = opal_list_get_first(port_list);
    port = (ompi_common_verbs_port_item_t*) item;
    ompi_btl_usnic_ext_init(port->device->context);

    /* Setup an array of pointers to point to each module (which we'll
       return upstream) */
    mca_btl_usnic_component.num_modules = opal_list_get_size(port_list);
    btls = (struct mca_btl_base_module_t**)
        malloc(mca_btl_usnic_component.num_modules * 
               sizeof(ompi_btl_usnic_module_t*));
    if (NULL == btls) {
        OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto free_include_list;
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
        OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
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
    } else {
        filter = NULL;
    }

    num_local_procs = ompi_process_info.num_local_peers;

    /* Go through the list of ports and determine if we want it or
       not.  Create and (mostly) fill a module struct for each port
       that we want. */
    for (i = 0, item = opal_list_get_first(port_list);
         item != opal_list_get_end(port_list) &&
             (0 == mca_btl_usnic_component.max_modules ||
              i < mca_btl_usnic_component.max_modules);
         item = opal_list_get_next(item)) {
        port = (ompi_common_verbs_port_item_t*) item;

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: found: device %s, port %d",
                            port->device->device_name, port->port_num);

        /* Fill in a bunch of the module struct */
        module = &(mca_btl_usnic_component.usnic_all_modules[i]);
        if (OMPI_SUCCESS != init_module_from_port(module, port)) {
            --mca_btl_usnic_component.num_modules;
            continue; /* next port */
        }

        /* respect if_include/if_exclude subnets/ifaces from the user */
        if (filter != NULL) {
            keep_module = filter_module(module, filter, filter_incl);
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: %s module %s due to %s",
                                (keep_module ? "keeping" : "skipping"),
                                ibv_get_device_name(module->device),
                                (filter_incl ? "if_include" : "if_exclude"));
            if (!keep_module) {
                --mca_btl_usnic_component.num_modules;
                continue; /* next port */
            }
        }

        /* Query this device */
        if (0 != ibv_query_device(module->device_context, &device_attr)) {
            opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                           true, 
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name,
                           "ibv_query_device", __FILE__, __LINE__,
                           "Failed to query usNIC device; is the usnic_verbs Linux kernel module loaded?");
            --mca_btl_usnic_component.num_modules;
            continue;
        }
        opal_memchecker_base_mem_defined(&device_attr, sizeof(device_attr));

        /* Check some usNIC configuration minimum settings */
        if (check_usnic_config(&device_attr, module,
                               num_local_procs) != OMPI_SUCCESS) {
            --mca_btl_usnic_component.num_modules;
            continue;
        }

        /* Tell this device's context that we are aware that we need
           to request the UD header length.  If it fails, just skip
           this device. */
        if (NULL != ompi_btl_usnic_ext.enable_udp) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: enabling UDP support for %s",
                                ibv_get_device_name(module->device));
            if (0 !=
                ompi_btl_usnic_ext.enable_udp(port->device->context)) {
                --mca_btl_usnic_component.num_modules;
                opal_output_verbose(5, USNIC_OUT,
                                    "btl:usnic: UDP support unexpectedly failed for %s; ignoring this device", 
                                    ibv_get_device_name(module->device));
                continue;
            }

            int len =
                ompi_btl_usnic_ext.get_ud_header_len(port->device->context,
                                                     port->port_num);
            /* Sanity check: the len we get back should be 42.  If
               it's not, skip this device. */
            if (OMPI_BTL_USNIC_UDP_HDR_SZ != len) {
                opal_output_verbose(5, USNIC_OUT,
                                    "btl:usnic: unexpected UD header length for %s reported by extension (%d); ignoring this device", 
                                    ibv_get_device_name(module->device),
                                    len);
                --mca_btl_usnic_component.num_modules;
                continue;
            }
        }

        /* How many xQ entries do we want? */
        if (-1 == mca_btl_usnic_component.sd_num) {
            module->sd_num = device_attr.max_qp_wr;
        } else {
            module->sd_num = mca_btl_usnic_component.sd_num;
        }
        if (-1 == mca_btl_usnic_component.rd_num) {
            module->rd_num = device_attr.max_qp_wr;
        } else {
            module->rd_num = mca_btl_usnic_component.rd_num;
        }
        if (-1 == mca_btl_usnic_component.cq_num) {
            module->cq_num = device_attr.max_cqe;
        } else {
            module->cq_num = mca_btl_usnic_component.cq_num;
        }

        /*
         * Queue sizes for priority channel scale with # of endpoint. A
         * little bit of chicken and egg here, we really want
         * procs*ports, but we can't know # of ports until we try to
         * initialize, so 32*num_procs is best guess.  User can always
         * override.
         */
        if (-1 == mca_btl_usnic_component.prio_sd_num) {
            module->prio_sd_num =
                max(128, 32 * ompi_process_info.num_procs) - 1;
        } else {
            module->prio_sd_num = mca_btl_usnic_component.prio_sd_num;
        }
        if (module->prio_sd_num > device_attr.max_qp_wr) {
            module->prio_sd_num = device_attr.max_qp_wr;
        }
        if (-1 == mca_btl_usnic_component.prio_rd_num) {
            module->prio_rd_num =
                max(128, 32 * ompi_process_info.num_procs) - 1;
        } else {
            module->prio_rd_num = mca_btl_usnic_component.prio_rd_num;
        }
        if (module->prio_rd_num > device_attr.max_qp_wr) {
            module->prio_rd_num = device_attr.max_qp_wr;
        }

        /* Find the max payload this port can handle */
        module->max_frag_payload =
            module->if_mtu - /* start with the MTU */
            OMPI_BTL_USNIC_PROTO_HDR_SZ -
            sizeof(ompi_btl_usnic_btl_header_t); /* subtract size of
                                                    the BTL header */
        /* same, but use chunk header */
        module->max_chunk_payload =
            module->if_mtu -
            OMPI_BTL_USNIC_PROTO_HDR_SZ -
            sizeof(ompi_btl_usnic_btl_chunk_header_t);

        /* Priorirty queue MTU and max size */
        if (0 == module->tiny_mtu) {
            module->tiny_mtu = 768;
            module->max_tiny_payload = module->tiny_mtu -
                OMPI_BTL_USNIC_PROTO_HDR_SZ -
                sizeof(ompi_btl_usnic_btl_header_t);
        } else {
            module->tiny_mtu = module->max_tiny_payload +
                OMPI_BTL_USNIC_PROTO_HDR_SZ +
                sizeof(ompi_btl_usnic_btl_header_t);
        }

        /* If the eager rndv limit is 0, initialize it to default */
        if (0 == module->super.btl_rndv_eager_limit) {
            module->super.btl_rndv_eager_limit = USNIC_DFLT_RNDV_EAGER_LIMIT;
        }

        /* Make a hash table of senders */
        OBJ_CONSTRUCT(&module->senders, opal_hash_table_t);
        /* JMS This is a fixed size -- BAD!  But since hash table
           doesn't grow dynamically, I don't know what size to put
           here.  I think the long-term solution is to write a better
           hash table... :-( */
        opal_hash_table_init(&module->senders, 4096);

        /* Let this module advance to the next round! */
        btls[i++] = &(module->super);
    }

    /* free filter if created */
    if (filter != NULL) {
        free_filter(filter);
        filter = NULL;
    }

    /* Do final module initialization with anything that required
       knowing how many modules there would be. */
    for (num_final_modules = i = 0;
         i < mca_btl_usnic_component.num_modules; ++i) {
        module = (ompi_btl_usnic_module_t*) btls[i];

        /* If the eager send limit is 0, initialize it to default */
        if (0 == module->super.btl_eager_limit) {
            /* 150k for 1 module, 25k for >1 module */
            if (1 == mca_btl_usnic_component.num_modules) {
                module->super.btl_eager_limit =
                    USNIC_DFLT_EAGER_LIMIT_1DEVICE;
            } else {
                module->super.btl_eager_limit =
                    USNIC_DFLT_EAGER_LIMIT_NDEVICES;
            }
        }

        /* Since we emulate PUT, max_send_size can be same as
           eager_limit */
        module->super.btl_max_send_size = module->super.btl_eager_limit;

        /* Initialize this module's state */
        if (ompi_btl_usnic_module_init(module) != OMPI_SUCCESS) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: failed to init module for %s:%d",
                                ibv_get_device_name(module->device),
                                module->port_num);
            continue;
        }

        /**** If we get here, this is a good module/port -- we want
              it ****/

        /* Tell the common_verbs_device to not free the device context
           when the list is freed.  Then free the port pointer cached
           on this module; it was only used to carry this
           module<-->port association down to this second loop.  The
           port item will be freed later, and is of no more use to the
           module. */
        module->port->device->destructor_free_context = false;
        module->port = NULL;

        /* If module_init() failed for any prior module, this will be
           a down shift in the btls[] array.  Otherwise, it's an
           overwrite of the same value. */
        btls[num_final_modules++] = &(module->super);

        /* Output all of this module's values. */
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: num sqe=%d, num rqe=%d, num cqe=%d",
                            module->sd_num,
                            module->rd_num,
                            module->cq_num);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: priority MTU %s:%d = %" PRIsize_t,
                            ibv_get_device_name(module->device),
                            module->port_num,
                            module->tiny_mtu);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: priority limit %s:%d = %" PRIsize_t,
                            ibv_get_device_name(module->device),
                            module->port_num,
                            module->max_tiny_payload);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: eager limit %s:%d = %" PRIsize_t,
                            ibv_get_device_name(module->device),
                            module->port_num,
                            module->super.btl_eager_limit);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: eager rndv limit %s:%d = %" PRIsize_t,
                            ibv_get_device_name(module->device),
                            module->port_num,
                            module->super.btl_rndv_eager_limit);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: max send size %s:%d = %" PRIsize_t 
                            " (not overrideable)",
                            ibv_get_device_name(module->device),
                            module->port_num,
                            module->super.btl_max_send_size);
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: exclusivity %s:%d = %d",
                            ibv_get_device_name(module->device),
                            module->port_num,
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

       That being said, if we ended up with zero acceptable ports,
       then free everything. */
    if (0 == num_final_modules) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: returning 0 modules");
        goto error;
    }

    /* we have a nonzero number of modules, so save a copy of the btls array
     * for later use */
    memcpy(mca_btl_usnic_component.usnic_active_modules, btls,
           num_final_modules*sizeof(*btls));

    /* Loop over the modules and find the minimum value for
       module->numa_distance.  For every module that has a
       numa_distance higher than the minimum value, increase its btl
       latency rating so that the PML will prefer to send short
       messages over "near" modules. */
    min_distance = 9999999;
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        module = (ompi_btl_usnic_module_t*) btls[i];
        if (module->numa_distance < min_distance) {
            min_distance = module->numa_distance;
        }
    }
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        module = (ompi_btl_usnic_module_t*) btls[i];
        if (module->numa_distance > min_distance) {
            ++module->super.btl_latency;
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: %s is far from me; increasing latency rating",
                                ibv_get_device_name(module->device));
        }
    }

    /* start timer to guarantee synthetic clock advances */
    opal_event_set(opal_event_base, &usnic_clock_timer_event,
                   -1, 0, usnic_clock_callback,
                   &usnic_clock_timeout);
    usnic_clock_timer_event_set = true;

    /* 1ms timer */
    usnic_clock_timeout.tv_sec = 0;
    usnic_clock_timeout.tv_usec = 1000;
    opal_event_add(&usnic_clock_timer_event, &usnic_clock_timeout);

    /* Setup MPI_T performance variables */
    ompi_btl_usnic_setup_mpit_pvars();

    /* All done */
    *num_btl_modules = mca_btl_usnic_component.num_modules;
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: returning %d modules", *num_btl_modules);

 free_include_list:
    if (NULL != port_list) {
        while (NULL != (item = opal_list_remove_first(port_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(port_list);
    }

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
    goto free_include_list;
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
static int usnic_handle_completion(ompi_btl_usnic_module_t* module,
    ompi_btl_usnic_channel_t *channel, struct ibv_wc *cwc);
static int usnic_component_progress_2(void);

static int usnic_component_progress(void)
{
    uint32_t i;
    int count;
    ompi_btl_usnic_recv_segment_t* rseg;
    ompi_btl_usnic_module_t* module;
    struct ibv_wc wc;
    ompi_btl_usnic_channel_t *channel;
    static bool fastpath_ok=true;

    /* update our simulated clock */
    ompi_btl_usnic_ticks += 5000;

    count = 0;
    if (fastpath_ok) {
        for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
            module = mca_btl_usnic_component.usnic_active_modules[i];
            channel = &module->mod_channels[USNIC_PRIORITY_CHANNEL];

            assert(channel->chan_deferred_recv == NULL);

            if (ibv_poll_cq(channel->cq, 1, &wc) == 1) {
                if (OPAL_LIKELY(wc.opcode == IBV_WC_RECV &&
                                wc.status == IBV_WC_SUCCESS)) {
                    rseg = (ompi_btl_usnic_recv_segment_t*)(intptr_t)wc.wr_id;
                    ompi_btl_usnic_recv_fast(module, rseg, channel,
                                             wc.byte_len);
                    fastpath_ok = false;    /* prevent starvation */
                    return 1;
                } else {
                    count += usnic_handle_completion(module, channel, &wc);
                }
            }
        }
    }

    fastpath_ok = true;
    return count + usnic_component_progress_2();
}

static int usnic_handle_completion(
    ompi_btl_usnic_module_t* module,
    ompi_btl_usnic_channel_t *channel,
    struct ibv_wc *cwc)
{
    ompi_btl_usnic_segment_t* seg;
    ompi_btl_usnic_recv_segment_t* rseg;

    seg = (ompi_btl_usnic_segment_t*)(unsigned long)cwc->wr_id;
    rseg = (ompi_btl_usnic_recv_segment_t*)seg;

    if (OPAL_UNLIKELY(cwc->status != IBV_WC_SUCCESS)) {

        /* If it was a receive error, just drop it and keep
           going.  The sender will eventually re-send it. */
        if (IBV_WC_RECV == cwc->opcode) {
            if (cwc->byte_len <
                 (OMPI_BTL_USNIC_PROTO_HDR_SZ +
                  sizeof(ompi_btl_usnic_btl_header_t))) {
                uint32_t m = mca_btl_usnic_component.max_short_packets;
                ++module->num_short_packets;
                if (OPAL_UNLIKELY(0 != m &&
                                  module->num_short_packets >= m)) {
                    opal_show_help("help-mpi-btl-usnic.txt",
                                   "received too many short packets",
                                   true,
                                   ompi_process_info.nodename,
                                   ibv_get_device_name(module->device),
                                   module->if_name,
                                   module->num_short_packets);

                    /* Reset so that we only show this warning once
                       per MPI process */
                    mca_btl_usnic_component.max_short_packets = 0;
                }
            } else {
                /* silently count CRC errors */
                ++module->stats.num_crc_errors;
            }
            rseg->rs_recv_desc.next = channel->repost_recv_head;
            channel->repost_recv_head = &rseg->rs_recv_desc;
            return 0;
        } else {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "non-receive completion error",
                           true,
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name,
                           channel->chan_index,
                           cwc->status,
                           (void*) cwc->wr_id,
                           cwc->opcode,
                           cwc->vendor_err);

            /* mark error on this channel */
            channel->chan_error = true;
            return 0;
        }
    }

    /* Handle work completions */
    switch(seg->us_type) {

    /**** Send ACK completions ****/
    case OMPI_BTL_USNIC_SEG_ACK:
        assert(IBV_WC_SEND == cwc->opcode);
        ompi_btl_usnic_ack_complete(module,
                (ompi_btl_usnic_ack_segment_t *)seg);
        break;

    /**** Send of frag segment completion ****/
    case OMPI_BTL_USNIC_SEG_FRAG:
        assert(IBV_WC_SEND == cwc->opcode);
        ompi_btl_usnic_frag_send_complete(module,
                (ompi_btl_usnic_frag_segment_t*)seg);
        break;

    /**** Send of chunk segment completion ****/
    case OMPI_BTL_USNIC_SEG_CHUNK:
        assert(IBV_WC_SEND == cwc->opcode);
        ompi_btl_usnic_chunk_send_complete(module,
                (ompi_btl_usnic_chunk_segment_t*)seg);
        break;

    /**** Receive completions ****/
    case OMPI_BTL_USNIC_SEG_RECV:
        assert(IBV_WC_RECV == cwc->opcode);
        ompi_btl_usnic_recv(module, rseg, channel, cwc->byte_len);
        break;

    default:
        BTL_ERROR(("Unhandled completion opcode %d segment type %d",
                    cwc->opcode, seg->us_type));
        break;
    }
    return 1;
}

static int usnic_component_progress_2(void)
{
    uint32_t i;
    int j, count = 0, num_events;
    struct ibv_recv_wr *bad_wr;
    ompi_btl_usnic_module_t* module;
    static struct ibv_wc wc[OMPI_BTL_USNIC_NUM_WC];
    ompi_btl_usnic_channel_t *channel;
    int rc;
    int c;

    /* update our simulated clock */
    ompi_btl_usnic_ticks += 5000;

    /* Poll for completions */
    for (i = 0; i < mca_btl_usnic_component.num_modules; i++) {
        module = mca_btl_usnic_component.usnic_active_modules[i];

        /* poll each channel */
        for (c=0; c<USNIC_NUM_CHANNELS; ++c) {
            channel = &module->mod_channels[c];

            if (channel->chan_deferred_recv != NULL) {
                (void) ompi_btl_usnic_recv_frag_bookkeeping(module,
                        channel->chan_deferred_recv, channel);
                channel->chan_deferred_recv = NULL;
            }

            num_events = ibv_poll_cq(channel->cq, OMPI_BTL_USNIC_NUM_WC, wc);
            opal_memchecker_base_mem_defined(&num_events, sizeof(num_events));
            opal_memchecker_base_mem_defined(wc, sizeof(wc[0]) * num_events);
            if (OPAL_UNLIKELY(num_events < 0)) {
                BTL_ERROR(("%s: error polling CQ[%d] with %d: %s",
                        ibv_get_device_name(module->device), c,
                        num_events, strerror(errno)));
                return OMPI_ERROR;
            }

            /* Handle each event */
            for (j = 0; j < num_events; j++) {
                count += usnic_handle_completion(module, channel, &wc[j]);
            }

            /* return error if detected - this may be slightly deferred
             * since fastpath avoids the "if" of checking this.
             */
            if (channel->chan_error) {
                channel->chan_error = false;
                return OMPI_ERROR;
            }

            /* progress sends */
            ompi_btl_usnic_module_progress_sends(module);

            /* Re-post all the remaining receive buffers */
            if (OPAL_LIKELY(channel->repost_recv_head)) {
                rc = ibv_post_recv(channel->qp, 
                                channel->repost_recv_head, &bad_wr);
                channel->repost_recv_head = NULL;
                if (OPAL_UNLIKELY(rc != 0)) {
                    BTL_ERROR(("error posting recv: %s\n", strerror(errno)));
                    return OMPI_ERROR;
                }
            }
        }
    }

    return count;
}

/* returns OMPI_SUCCESS if module initialization was successful, OMPI_ERROR
 * otherwise */
static int init_module_from_port(ompi_btl_usnic_module_t *module,
                                 ompi_common_verbs_port_item_t *port)
{
    union ibv_gid gid;
    char my_ip_string[32];

    memcpy(module, &ompi_btl_usnic_module_template,
           sizeof(ompi_btl_usnic_module_t));
    module->port = port;
    module->device = port->device->device;
    module->device_context = port->device->context;
    module->port_num = port->port_num;
    module->numa_distance = 0;
    module->local_addr.use_udp = mca_btl_usnic_component.use_udp;

    /* If we fail to query the GID, just warn and skip this port */
    if (0 != ibv_query_gid(module->device_context,
                           module->port_num,
                           mca_btl_usnic_component.gid_index, &gid)) {
        opal_memchecker_base_mem_defined(&gid, sizeof(gid));
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true,
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_query_gid", __FILE__, __LINE__,
                       "Failed to query usNIC GID");
        return OMPI_ERROR;
    }

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: GID for %s:%d: subnet 0x%016" PRIx64 ", interface 0x%016" PRIx64,
                        ibv_get_device_name(module->device),
                        module->port_num,
                        ntoh64(gid.global.subnet_prefix),
                        ntoh64(gid.global.interface_id));
    module->local_addr.gid = gid;

    /* Extract the MAC address from the interface_id */
    ompi_btl_usnic_gid_to_mac(&gid, module->local_addr.mac);

    /* Use that MAC address to find the device/port's
       corresponding IP address */
    if (OPAL_SUCCESS != ompi_btl_usnic_find_ip(module,
                                               module->local_addr.mac)) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: did not find IP interfaces for %s; skipping",
                            ibv_get_device_name(module->device));
        return OMPI_ERROR;
    }
    inet_ntop(AF_INET, &module->if_ipv4_addr,
              my_ip_string, sizeof(my_ip_string));
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: IP address for %s:%d: %s",
                        ibv_get_device_name(module->device),
                        module->port_num,
                        my_ip_string);


    /* Get this port's bandwidth */
    if (0 == module->super.btl_bandwidth) {
        if (OMPI_SUCCESS !=
            ompi_common_verbs_port_bw(&port->port_attr,
                                      &module->super.btl_bandwidth)) {

            /* If we don't get OMPI_SUCCESS, then we weren't able
               to figure out what the bandwidth was of this port.
               That's a bad sign.  Let's ignore this port. */
            opal_show_help("help-mpi-btl-usnic.txt", "verbs_port_bw failed",
                           true,
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name);
            return OMPI_ERROR;
        }
    }
    module->local_addr.link_speed_mbps = module->super.btl_bandwidth;
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: bandwidth for %s:%d = %u",
                        ibv_get_device_name(module->device),
                        module->port_num,
                        module->super.btl_bandwidth);

    return OMPI_SUCCESS;
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
        OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
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
        OMPI_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
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
                                "btl:usnic:filter_module: parsed %s device name: %s",
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
                           true, name, ompi_process_info.nodename,
                           tmp, "Invalid specification (missing \"/\")");
            free(tmp);
            continue;
        }
        *str = '\0';
        argv_prefix = atoi(str + 1);
        if (argv_prefix < 1 || argv_prefix > 32) {
            opal_show_help("help-mpi-btl-usnic.txt", "invalid if_inexclude",
                           true, name, ompi_process_info.nodename,
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
                           true, name, ompi_process_info.nodename, tmp,
                           "Invalid specification (inet_pton() failed)");
            free(tmp);
            continue;
        }
        opal_output_verbose(20, USNIC_OUT,
                            "btl:usnic:filter_module: parsed %s address+prefix: %s / %u",
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
        filter->elts[filter->n_elt].addr =
            ompi_btl_usnic_get_ipv4_subnet(addr, argv_prefix);
        filter->elts[filter->n_elt].prefixlen = argv_prefix;
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
static bool filter_module(ompi_btl_usnic_module_t *module,
                          usnic_if_filter_t *filter,
                          bool filter_incl)
{
    int i;
    uint32_t module_mask;
    bool match;

    module_mask = ompi_btl_usnic_get_ipv4_subnet(module->if_ipv4_addr,
                                                 module->if_cidrmask);
    match = false;
    for (i = 0; i < filter->n_elt; ++i) {
        if (filter->elts[i].is_netmask) {
            /* conservative: we also require the prefixlen to match */
            if (filter->elts[i].prefixlen == module->if_cidrmask &&
                filter->elts[i].addr == module_mask) {
                match = true;
                break;
            }
        }
        else {
            if (strcmp(filter->elts[i].if_name,
                       ibv_get_device_name(module->device)) == 0) {
                match = true;
                break;
            }
        }
    }

    /* Turn the match result into whether we should keep it or not */
    return match ^ !filter_incl;
}

/* could take indent as a parameter instead of hard-coding it */
static void dump_endpoint(ompi_btl_usnic_endpoint_t *endpoint)
{
    int i;
    ompi_btl_usnic_frag_t *frag;
    ompi_btl_usnic_send_segment_t *sseg;
    int ep_jobid;
    int ep_rank;
    struct in_addr ia;
    char ep_addr_str[INET_ADDRSTRLEN];
    char tmp[128], str[2048];

    ep_jobid = endpoint->endpoint_proc->proc_ompi->proc_name.jobid;
    ep_rank = endpoint->endpoint_proc->proc_ompi->proc_name.vpid;

    memset(ep_addr_str, 0x00, sizeof(ep_addr_str));
    ia.s_addr = endpoint->endpoint_remote_addr.ipv4_addr;
    inet_ntop(AF_INET, &ia, ep_addr_str, sizeof(ep_addr_str));

    opal_output(0, "    endpoint %p, %s job=%"PRIu32" rank=%"PRIu32" rts=%s s_credits=%"PRIi32"\n",
           (void *)endpoint, ep_addr_str, ep_jobid, ep_rank,
           (endpoint->endpoint_ready_to_send ? "true" : "false"),
           endpoint->endpoint_send_credits);
    opal_output(0, "      endpoint->frag_send_queue:\n");

    OPAL_LIST_FOREACH(frag, &endpoint->endpoint_frag_send_queue,
                      ompi_btl_usnic_frag_t) {
        ompi_btl_usnic_small_send_frag_t *ssfrag;
        ompi_btl_usnic_large_send_frag_t *lsfrag;

        snprintf(str, sizeof(str), "      --> frag %p, %s", (void *)frag,
                 usnic_frag_type(frag->uf_type));
        switch (frag->uf_type) {
            case OMPI_BTL_USNIC_FRAG_LARGE_SEND:
                lsfrag = (ompi_btl_usnic_large_send_frag_t *)frag;
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
                                  ompi_btl_usnic_send_segment_t) {
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

            case OMPI_BTL_USNIC_FRAG_SMALL_SEND:
                ssfrag = (ompi_btl_usnic_small_send_frag_t *)frag;
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

            case OMPI_BTL_USNIC_FRAG_PUT_DEST:
                /* put_dest frags are just a typedef to generic frags */
                snprintf(tmp, sizeof(tmp), " put_addr=%p\n", frag->uf_dst_seg[0].seg_addr.pval);
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
                   usnic_seg_type(sseg->ss_base.us_type),
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
        ompi_btl_usnic_snprintf_bool_array(str, sizeof(str),
                                           endpoint->endpoint_rcvd_segs,
                                           WINDOW_SIZE);
        opal_output(0, "      rcvd_segs 0x%s", str);
    }
}

void ompi_btl_usnic_component_debug(void)
{
    int i;
    ompi_btl_usnic_module_t *module;
    ompi_btl_usnic_endpoint_t *endpoint;
    ompi_btl_usnic_send_segment_t *sseg;
    opal_list_item_t *item;

    opal_output(0, "*** dumping usnic state for MPI_COMM_WORLD rank %d ***\n",
                ompi_proc_local_proc->proc_name.vpid);
    for (i = 0; i < (int)mca_btl_usnic_component.num_modules; ++i) {
        module = mca_btl_usnic_component.usnic_active_modules[i];

        opal_output(0, "active_modules[%d]=%p %s max{frag,chunk,tiny}=%llu,%llu,%llu\n",
               i, (void *)module, module->if_name,
               (unsigned long long)module->max_frag_payload,
               (unsigned long long)module->max_chunk_payload,
               (unsigned long long)module->max_tiny_payload);

        opal_output(0, "  endpoints_with_sends:\n");
        OPAL_LIST_FOREACH(endpoint, &module->endpoints_with_sends,
                          ompi_btl_usnic_endpoint_t) {
            dump_endpoint(endpoint);
        }

        opal_output(0, "  endpoints_that_need_acks:\n");
        OPAL_LIST_FOREACH(endpoint, &module->endpoints_that_need_acks,
                          ompi_btl_usnic_endpoint_t) {
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
                          ompi_btl_usnic_send_segment_t) {
            opal_output(0, "    sseg %p\n", (void *)sseg);
        }

        ompi_btl_usnic_print_stats(module, "  manual", /*reset=*/false);
    }
}

#include "test/btl_usnic_component_test.h"
