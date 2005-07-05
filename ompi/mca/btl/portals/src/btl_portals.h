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
/*
 * @file
 */
#ifndef MCA_BTL_PORTALS_H
#define MCA_BTL_PORTALS_H

#include <portals3.h>

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_free_list.h"
#include "class/ompi_proc_table.h"

#include "btl_portals_endpoint.h"


/*
 * Portals BTL component.
 */
struct mca_btl_portals_component_t {
    /* base BTL component */
    mca_btl_base_component_1_0_0_t super;

    /* output channel for debugging.  Value settings when using
     * output_verbose:
     *
     *  - 0 : critical user information
     *  - 10: general execution diagnostic information
     *  - 20: initialization / shutdown diagnostic information
     *  - 30: basic debugging information
     *  - 90: useful only to developers
     *  - 100: lots and lots of performance impacting output
     */
    int portals_output;

#if BTL_PORTALS_UTCP
    /* ethernet interface to use - only has meaning with utcp
        reference */
    char *portals_ifname;
#endif

    /* Number of currently active portals modules.  We assume these
       never change between init and finalize, so these aren't thread
       locked */
    uint32_t portals_num_modules;
    /* List of currently available modules */
    struct mca_btl_portals_module_t *portals_modules;

    /* initial size of free lists */
    int portals_free_list_init_num;
    /* max size of free lists */
    int portals_free_list_max_num;
    /* numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /* lock for accessing component */
    opal_mutex_t portals_lock;
};
typedef struct mca_btl_portals_component_t mca_btl_portals_component_t;


#define MCA_BTL_PORTALS_EQ_RECV  0
#define MCA_BTL_PORTALS_EQ_SEND  1
#define MCA_BTL_PORTALS_EQ_RDMA  2
#define MCA_BTL_PORTALS_EQ_SIZE  3

struct mca_btl_portals_module_t {
    /* base BTL module interface */
    mca_btl_base_module_t super;

    /* registered callbacks */
    mca_btl_base_recv_reg_t portals_reg[MCA_BTL_TAG_MAX];

    /* list of connected procs */
    opal_list_t portals_endpoint_list;

    ompi_free_list_t portals_frag_eager;
    ompi_free_list_t portals_frag_max;
    ompi_free_list_t portals_frag_user;

    /* number of mds for recv frags */
    int portals_recv_mds_num;
    /* size of each md for first frags */
    int portals_recv_mds_size;

    /* size for event queue */
    int portals_eq_sizes[MCA_BTL_PORTALS_EQ_SIZE];
    /* frag receive event queue */
    ptl_handle_eq_t portals_eq_handles[MCA_BTL_PORTALS_EQ_SIZE];

    /* our portals network interface */
    ptl_handle_ni_t portals_ni_h;
    /* the limits returned from PtlNIInit for interface */
    ptl_ni_limits_t portals_ni_limits;

    /* number of dropped messages */
    ptl_sr_value_t portals_sr_dropped;

    /* lock for accessing module */
    opal_mutex_t portals_lock;
};
typedef struct mca_btl_portals_module_t mca_btl_portals_module_t;

/*
 * Component functions (btl_portals_component.c)
 */
int mca_btl_portals_component_open(void);
int mca_btl_portals_component_close(void);


mca_btl_base_module_t** mca_btl_portals_component_init(int *num_btls, 
                                                 bool has_progress_threads,
                                                 bool has_mpi_threads);

int mca_btl_portals_component_progress(void);

/*
 * Compatibility functions (btl_portals_compat_{}.c)
 *
 * Not part of the BTL interface.  Need to be implemented for every
 * version of Portals
 */
int mca_btl_portals_init(mca_btl_portals_component_t *comp);

/* 4th argument is a ptl_peers array, as that's what we'll get back
   from many of the access functions... */
int mca_btl_portals_add_procs_compat(mca_btl_portals_module_t* btl,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     ptl_process_id_t **ptl_peers);

/*
 * Module configuration functions (btl_portals.c)
 */
int mca_btl_portals_finalize(struct mca_btl_base_module_t* btl);

int mca_btl_portals_add_procs(struct mca_btl_base_module_t* btl,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_btl_base_endpoint_t** peers,
                                     ompi_bitmap_t* reachable);

int mca_btl_portals_del_procs(struct mca_btl_base_module_t* btl,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_btl_base_endpoint_t** peers);
/*
 * stubbed functions 
 */
int mca_btl_portals_register(struct mca_btl_base_module_t* btl,
                             mca_btl_base_tag_t tag,
                             mca_btl_base_module_recv_cb_fn_t cbfunc,
                             void* cbdata);

mca_btl_base_descriptor_t* 
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl, 
                      size_t size); 

int mca_btl_portals_free(struct mca_btl_base_module_t* btl, 
                         mca_btl_base_descriptor_t* des); 

mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size);

mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_dst(struct mca_btl_base_module_t* btl, 
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size); 

int mca_btl_portals_send(struct mca_btl_base_module_t* btl,
                         struct mca_btl_base_endpoint_t* btl_peer,
                         struct mca_btl_base_descriptor_t* descriptor, 
                         mca_btl_base_tag_t tag);

int mca_btl_portals_put(struct mca_btl_base_module_t* btl,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        struct mca_btl_base_descriptor_t* decriptor);


int mca_btl_portals_get(struct mca_btl_base_module_t* btl,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        struct mca_btl_base_descriptor_t* decriptor);


/*
 * global structures
 */
extern mca_btl_portals_component_t mca_btl_portals_component;
/* don't use, except as base for creating module instances */
extern mca_btl_portals_module_t mca_btl_portals_module;

#endif
