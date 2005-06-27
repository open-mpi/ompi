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
#ifndef MCA_BMI_PORTALS_H
#define MCA_BMI_PORTALS_H

#include <portals3.h>

#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_free_list.h"
#include "class/ompi_proc_table.h"


/*
 * Portals BMI component.
 */
struct mca_bmi_portals_component_t {
    /* base BMI component */
    mca_bmi_base_component_1_0_0_t super;

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

#if BMI_PORTALS_UTCP
    /* ethernet interface to use - only has meaning with utcp
        reference */
    char *portals_ifname;
#endif

    /* Number of currently active portals modules.  We assume these
       never change between init and finalize, so these aren't thread
       locked */
    uint32_t portals_num_modules;
    /* List of currently available modules */
    struct mca_bmi_portals_module_t **portals_modules;

    /* initial size of free lists */
    int portals_free_list_init_num;
    /* max size of free lists */
    int portals_free_list_max_num;
    /* numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /* lock for accessing component */
    ompi_mutex_t portals_lock;
};
typedef struct mca_bmi_portals_component_t mca_bmi_portals_component_t;


#define MCA_BMI_PORTALS_EQ_RECV  0
#define MCA_BMI_PORTALS_EQ_SEND  1
#define MCA_BMI_PORTALS_EQ_SIZE  2

struct mca_bmi_portals_module_t {
    /* base BMI module interface */
    mca_bmi_base_module_t super;

    /* number of mds for first frags */
    int first_frag_num_entries;
    /* size of each md for first frags */
    int first_frag_entry_size;

    /* size for event queue */
    int eq_sizes[MCA_BMI_PORTALS_EQ_SIZE];
    /* frag receive event queue */
    ptl_handle_eq_t eq_handles[MCA_BMI_PORTALS_EQ_SIZE];

    /* our portals network interface */
    ptl_handle_ni_t ni_handle;
    /* the limits returned from PtlNIInit for interface */
    ptl_ni_limits_t limits;

    /* number of dropped messages */
    ptl_sr_value_t dropped;
};
typedef struct mca_bmi_portals_module_t mca_bmi_portals_module_t;

struct mca_bmi_portals_recv_frag_t;
struct mca_bmi_portals_send_frag_t;

/*
 * Component functions (bmi_portals_component.c)
 */
int mca_bmi_portals_component_open(void);
int mca_bmi_portals_component_close(void);


mca_bmi_base_module_t** mca_bmi_portals_component_init(int *num_bmis, 
                                                 bool has_progress_threads,
                                                 bool has_mpi_threads);

int mca_bmi_portals_component_progress(void);

/*
 * Compatibility functions (bmi_portals_compat_{}.c)
 *
 * Not part of the BMI interface.  Need to be implemented for every
 * version of Portals
 */
int mca_bmi_portals_init(mca_bmi_portals_component_t *comp);

int mca_bmi_portals_add_procs_compat(mca_bmi_portals_module_t* bmi,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     ptl_process_id_t **portals_procs);

/*
 * Module configuration functions (bmi_portals.c)
 */
int mca_bmi_portals_finalize(struct mca_bmi_base_module_t* bmi);

int mca_bmi_portals_add_procs(struct mca_bmi_base_module_t* bmi,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_bmi_base_endpoint_t** peers,
                                     ompi_bitmap_t* reachable);

int mca_bmi_portals_del_procs(struct mca_bmi_base_module_t* bmi,
                                     size_t nprocs,
                                     struct ompi_proc_t **procs,
                                     struct mca_bmi_base_endpoint_t** peers);
/*
 * stubbed functions 
 */
int mca_bmi_portals_register(struct mca_bmi_base_module_t* bmi,
                             mca_bmi_base_tag_t tag,
                             mca_bmi_base_module_recv_cb_fn_t cbfunc,
                             void* cbdata);

mca_bmi_base_descriptor_t* 
mca_bmi_portals_alloc(struct mca_bmi_base_module_t* bmi, 
                      size_t size); 

int mca_bmi_portals_free(struct mca_bmi_base_module_t* bmi, 
                         mca_bmi_base_descriptor_t* des); 

mca_bmi_base_descriptor_t* 
mca_bmi_portals_prepare_src(struct mca_bmi_base_module_t* bmi,
                            struct mca_bmi_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size);

mca_bmi_base_descriptor_t* 
mca_bmi_portals_prepare_dst(struct mca_bmi_base_module_t* bmi, 
                            struct mca_bmi_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size); 

int mca_bmi_portals_send(struct mca_bmi_base_module_t* bmi,
                         struct mca_bmi_base_endpoint_t* bmi_peer,
                         struct mca_bmi_base_descriptor_t* descriptor, 
                         mca_bmi_base_tag_t tag);

int mca_bmi_portals_put(struct mca_bmi_base_module_t* bmi,
                        struct mca_bmi_base_endpoint_t* bmi_peer,
                        struct mca_bmi_base_descriptor_t* decriptor);


int mca_bmi_portals_get(struct mca_bmi_base_module_t* bmi,
                        struct mca_bmi_base_endpoint_t* bmi_peer,
                        struct mca_bmi_base_descriptor_t* decriptor);


/*
 * global structures
 */
extern mca_bmi_portals_component_t mca_bmi_portals_component;
/* don't use, except as base for creating module instances */
extern mca_bmi_portals_module_t mca_bmi_portals_module;

#endif
