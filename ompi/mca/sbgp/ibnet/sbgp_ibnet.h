/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#ifndef MCA_BCOL_ibnet_EXPORT_H
#define MCA_BCOL_ibnet_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "infiniband/verbs.h"
#include "opal/mca/mca.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/request/request.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/common/ofacm/connect.h"
#if OPAL_ENABLE_DEBUG
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#endif

BEGIN_C_DECLS

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#elif defined(__WINDOWS__)
#  define SPIN SwitchToThread()
#else  /* no switch available */
#  define SPIN
#endif

typedef enum {
    OFFLOAD_CONNECTX_B0,
    OFFLOAD_DISABLE
} coll_offload_support;

/**
 * Structure to hold the basic shared memory coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
struct mca_sbgp_ibnet_component_t {
    /** Base coll component */
    mca_sbgp_base_component_2_0_0_t super;

    /** Enable disable verbose mode */
    int verbose;

    /* Maximum allowed number of subroups */
    int max_sbgps;
    /* Enable disable default subnet id warning */
    bool warn_default_gid_prefix;
    bool warn_nonexistent_if;
    /* IB MTU requested by user */
    int    mtu;    /** MTU on this port */
    /** IB partition definition */
    uint32_t pkey_val;
    /* Keeping hca data */
    char *if_include;
    char **if_include_list;
    char *if_exclude;
    char **if_exclude_list;
    /** Dummy argv-style list; a copy of names from the
      if_[in|ex]clude list that we use for error checking (to ensure
      that they all exist) */
    char **if_list;
    /** List of iboffload devices that have at list one active port */
    opal_list_t devices;
    int curr_max_group_id;
    uint32_t total_active_ports;
};

/**
 * Convenience typedef
 */
typedef struct mca_sbgp_ibnet_component_t
mca_sbgp_ibnet_component_t;

/* IB port OBJ*/
struct mca_sbgp_ibnet_port_t {
    uint16_t             id;             /** Port number */
    int             stat;           /** Port status - Active,Init,etc.. */
    enum ibv_mtu    mtu;    /** MTU on this port */
    coll_offload_support coll_offload; /** Collectives offload mode */
    uint64_t        subnet_id;      /** Sunnet id for the port */
    /* uint8_t            src_path_bits;  */
    uint16_t        lid;
    uint16_t        lmc;
    /** Array of the peer's CPCs available on this port */
    uint32_t        num_cpcs;
    bool            used;
    ompi_common_ofacm_base_module_data_t *pm_cpc_data;
    ompi_common_ofacm_base_module_t *local_cpc;            /* selected cpc*/
    ompi_common_ofacm_base_module_data_t *remote_cpc_data; /* data for remote cpc */
};

typedef struct mca_sbgp_ibnet_port_t mca_sbgp_ibnet_port_t;

typedef enum {
    MCA_SBGP_IBNET_NONE          = 0,
    MCA_SBGP_IBNET_NODE_LEADER   = 1<<0,
    MCA_SBGP_IBNET_SOCKET_LEADER = 1<<1,
    MCA_SBGP_IBNET_SWITCH_LEADER = 1<<2
} mca_sbgp_ibnet_duty_t;

typedef enum {
    MCA_SBGP_IBNET_ALL_NET,
    MCA_SBGP_IBNET_NODE_NET,
    MCA_SBGP_IBNET_NONE_NET
} mca_sbgp_ibnet_mode_t;

struct mca_sbgp_ibnet_proc_t {
    opal_list_item_t super;
    ompi_proc_t      *ompi_proc;      /* Ompi proc pointer */
    int              ompi_proc_index; /* Index of the proc in array */
    uint32_t         rank;            /* vpid, remote proc rank */
    uint32_t         num_ports;       /* number of remote ports */
    int              *use_port;       /* the size of this array is equal to number of cgroups that points to this proc.
                                         Each cgroup has own index "I". The array keep remote port number that ne need to use
                                         for cgroup "I" - use_port[I]. We need it for iboffload module */
    mca_sbgp_ibnet_port_t *remote_ports_info; /* the array keeps remote port information */
    mca_sbgp_ibnet_duty_t  duty;              /* Socket leader, Node leader, switch leader, etc. */
};

typedef struct mca_sbgp_ibnet_proc_t mca_sbgp_ibnet_proc_t;
OBJ_CLASS_DECLARATION(mca_sbgp_ibnet_proc_t);

/* Device OBJ */
struct mca_sbgp_ibnet_device_t {
    opal_list_item_t super;
    struct ibv_device* ib_dev;              /* pointer to device, from device list */
    int device_index;                       /* device index in device list */
    struct ibv_device_attr ib_dev_attr;     /* attributes of the device */
    int num_act_ports;
    int num_allowed_ports;
    struct mca_sbgp_ibnet_port_t *ports;
    /* CPC stuff */
    ompi_common_ofacm_base_module_t **cpcs; /* Array of CPCs */
    uint8_t num_cpcs;                       /* Number of elements in cpc array */
};

typedef struct mca_sbgp_ibnet_device_t mca_sbgp_ibnet_device_t;
OBJ_CLASS_DECLARATION(mca_sbgp_ibnet_device_t);

struct mca_sbgp_ibnet_connection_group_info_t {
    int device_index;                       /* device index in device list */
    uint32_t port;                          /* port number */
    /* Used for detect number of a port to communicate with remote proc,
       index in use_port arrray in the mca_sbgp_ibnet_proc_t structure */
    uint32_t index;
    /* array of procs connected with this group */
    uint32_t num_procs;
    opal_pointer_array_t *ibnet_procs;
};
typedef struct mca_sbgp_ibnet_connection_group_info_t
               mca_sbgp_ibnet_connection_group_info_t;

/*
 ** Base sub-group module
 **/
struct mca_sbgp_ibnet_module_t {
    /** Collective modules all inherit from opal_object */
    mca_sbgp_base_module_t super;
    int group_id;
    /* opal_pointer_array_t *ibnet_procs; */
    /* number of connection groups */
    int num_cgroups;
    /*
     * Array of connection groups. There are same procs in these groups,
     * but they were created over different ports (and different devices maybe).
     */
    mca_sbgp_ibnet_connection_group_info_t *cgroups;
    mca_sbgp_ibnet_mode_t mode; /* working mode of the module, it is ALL by default */
};
typedef struct mca_sbgp_ibnet_module_t mca_sbgp_ibnet_module_t;
OBJ_CLASS_DECLARATION(mca_sbgp_ibnet_module_t);

/* Error and verbose prints */

static inline int mca_sbgp_ibnet_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#define IBNET_ERROR(args)                                       \
    do {                                                        \
        mca_sbgp_ibnet_err("[%s]%s[%s:%d:%s] IBNET ",           \
            orte_process_info.nodename,                         \
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
            __FILE__, __LINE__, __func__);                      \
        mca_sbgp_ibnet_err args;                                \
        mca_sbgp_ibnet_err("\n");                               \
    } while(0);

#if OPAL_ENABLE_DEBUG
#define IBNET_VERBOSE(level, args)                              \
    do {                                                        \
        if(mca_sbgp_ibnet_component.verbose >= level) {         \
            mca_sbgp_ibnet_err("[%s]%s[%s:%d:%s] IBNET ",       \
                    orte_process_info.nodename,                 \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                    __FILE__, __LINE__, __func__);              \
            mca_sbgp_ibnet_err args;                            \
            mca_sbgp_ibnet_err("\n");                           \
        }                                                       \
    } while(0);
#else
#define IBNET_VERBOSE(level, args)
#endif

#define MCA_SBGP_IBNET_PKEY_MASK 0x7fff

/* Error and verbose prints - end */

/* This routine is used to find the list of procs that run on the
 ** same host as the calling process.
 */
mca_sbgp_base_module_t *mca_sbgp_ibnet_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in, struct ompi_communicator_t *comm, char *key, void *output_data);

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_sbgp_ibnet_component_t mca_sbgp_ibnet_component;


END_C_DECLS

#endif /* MCA_BCOL_ibnet_EXPORT_H */
