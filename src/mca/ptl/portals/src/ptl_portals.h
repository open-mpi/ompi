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
#ifndef MCA_PTL_PORTALS_H
#define MCA_PTL_PORTALS_H

#include <portals3.h>

#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_free_list.h"
#include "class/ompi_proc_table.h"


/*
 * Portals PTL component.
 */
struct mca_ptl_portals_component_t {
    /* base PTL component */
    mca_ptl_base_component_1_0_0_t super;

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

#if PTL_PORTALS_UTCP
    /* ethernet interface to use - only has meaning with utcp
        reference */
    char *portals_ifname;
#endif

    /* Number of currently active portals modules.  We assume these
       never change between init and finalize, so these aren't thread
       locked */
    uint32_t portals_num_modules;
    /* List of currently available modules */
    struct mca_ptl_portals_module_t **portals_modules;

    /* initial size of free lists */
    int portals_free_list_init_num;
    /* max size of free lists */
    int portals_free_list_max_num;
    /* numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /* free list of portals send fragments */
    ompi_free_list_t portals_send_frags;
    /* free list of portals recv fragments */
    ompi_free_list_t portals_recv_frags;

    /* queue of pending sends */
    ompi_list_t portals_pending_acks;

    /* lock for accessing component */
    ompi_mutex_t portals_lock;
};
typedef struct mca_ptl_portals_component_t mca_ptl_portals_component_t;


#define MCA_PTL_PORTALS_EQ_RECV  0
#define MCA_PTL_PORTALS_EQ_SEND  1
#define MCA_PTL_PORTALS_EQ_SIZE  2

struct mca_ptl_portals_module_t {
    /* base PTL module interface */
    mca_ptl_base_module_t super;

    /* number of mds for first frags */
    int first_frag_num_entries;
    /* size of each md for first frags */
    int first_frag_entry_size;

    /* size for event queue */
    int eq_sizes[MCA_PTL_PORTALS_EQ_SIZE];
    /* frag receive event queue */
    ptl_handle_eq_t eq_handles[MCA_PTL_PORTALS_EQ_SIZE];

    /* our portals network interface */
    ptl_handle_ni_t ni_handle;
    /* the limits returned from PtlNIInit for interface */
    ptl_ni_limits_t limits;

    /* number of dropped messages */
    ptl_sr_value_t dropped;
};
typedef struct mca_ptl_portals_module_t mca_ptl_portals_module_t;

struct mca_ptl_portals_recv_frag_t;
struct mca_ptl_portals_send_frag_t;

/*
 * Component functions (ptl_portals_component.c)
 */
int mca_ptl_portals_component_open(void);
int mca_ptl_portals_component_close(void);


mca_ptl_base_module_t** mca_ptl_portals_component_init(int *num_ptls, 
                                                 bool has_progress_threads,
                                                 bool has_mpi_threads);

int mca_ptl_portals_component_control(int param,
                                      void* value,
                                      size_t size);

int mca_ptl_portals_component_progress(mca_ptl_tstamp_t tstamp);

/*
 * Compatibility functions (ptl_portals_compat_{}.c)
 *
 * Need to be implemented for every version of Portals
 */
int mca_ptl_portals_init(mca_ptl_portals_component_t *comp);

int mca_ptl_portals_add_procs_compat(mca_ptl_portals_module_t* ptl,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     ptl_process_id_t **portals_procs);

/*
 * Module configuration functions (ptl_portals.c)
 */
int mca_ptl_portals_finalize(struct mca_ptl_base_module_t* ptl);

int mca_ptl_portals_add_procs(struct mca_ptl_base_module_t* ptl,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_ptl_base_peer_t** peers,
                                     ompi_bitmap_t* reachable);

int mca_ptl_portals_del_procs(struct mca_ptl_base_module_t* ptl,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_ptl_base_peer_t** peers);

int mca_ptl_portals_module_enable(struct mca_ptl_portals_module_t *ptl,
                                  int value);

int mca_ptl_portals_request_init(struct mca_ptl_base_module_t* ptl,
                                 struct mca_ptl_base_send_request_t* req);

void mca_ptl_portals_request_fini(struct mca_ptl_base_module_t* ptl,
                                  struct mca_ptl_base_send_request_t* req);


/*
 * Communication functions (ptl_portals_{send,recv}.c)
 */
void mca_ptl_portals_matched(struct mca_ptl_base_module_t* ptl,
                             struct mca_ptl_base_recv_frag_t* frag);

int mca_ptl_portals_send(struct mca_ptl_base_module_t* ptl,
                         struct mca_ptl_base_peer_t* ptl_peer,
                         struct mca_ptl_base_send_request_t*,
                         size_t offset,
                         size_t size,
                         int flags);

int mca_ptl_portals_process_send_event(ptl_event_t *ev);


/*
 * global structures
 */
extern mca_ptl_portals_component_t mca_ptl_portals_component;
/* don't use, except as base for creating module instances */
extern mca_ptl_portals_module_t mca_ptl_portals_module;

#endif
