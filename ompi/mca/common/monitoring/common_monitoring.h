/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_MONITORING_H
#define MCA_COMMON_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>
#include <ompi/proc/proc.h>
#include <ompi/group/group.h>
#include <ompi/communicator/communicator.h>
#include <opal/class/opal_hash_table.h>
#include <opal/mca/base/mca_base_pvar.h>

#define MCA_MONITORING_MAKE_VERSION                                     \
    MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION, OMPI_RELEASE_VERSION)

#define OPAL_MONITORING_VERBOSE(x, ...)                                 \
    OPAL_OUTPUT_VERBOSE((x, mca_common_monitoring_output_stream_id, __VA_ARGS__))

#if OPAL_ENABLE_DEBUG
#define OPAL_MONITORING_PRINT_ERR(...)          \
    OPAL_MONITORING_VERBOSE(0, __VA_ARGS__)
#else /* if( ! OPAL_ENABLE_DEBUG ) */
#define OPAL_MONITORING_PRINT_ERR(...)          \
    OPAL_MONITORING_VERBOSE(1, __VA_ARGS__)
#endif /* OPAL_ENABLE_DEBUG */

#define OPAL_MONITORING_PRINT_WARN(...)         \
    OPAL_MONITORING_VERBOSE(5, __VA_ARGS__)

#define OPAL_MONITORING_PRINT_INFO(...)         \
    OPAL_MONITORING_VERBOSE(10, __VA_ARGS__)

extern int mca_common_monitoring_output_stream_id;
extern int mca_common_monitoring_enabled;
extern int mca_common_monitoring_current_state;

OMPI_DECLSPEC int mca_common_monitoring_get_messages_count (const struct mca_base_pvar_t *pvar,
                                                            void *value, void *obj_handle);

OMPI_DECLSPEC int mca_common_monitoring_get_messages_size (const struct mca_base_pvar_t *pvar,
                                                           void *value, void *obj_handle);

OMPI_DECLSPEC int mca_common_monitoring_get_rmessages_count (const struct mca_base_pvar_t *pvar,
                                                             void *value, void *obj_handle);

OMPI_DECLSPEC int mca_common_monitoring_get_rmessages_size (const struct mca_base_pvar_t *pvar,
                                                            void *value, void *obj_handle);

OMPI_DECLSPEC int mca_common_monitoring_set_flush(struct mca_base_pvar_t *pvar,
                                                  const void *value, void *obj);

OMPI_DECLSPEC int mca_common_monitoring_get_flush(const struct mca_base_pvar_t *pvar,
                                                  void *value, void *obj);

OMPI_DECLSPEC int mca_common_monitoring_messages_notify(mca_base_pvar_t *pvar,
                                                        mca_base_pvar_event_t event,
                                                        void *obj_handle, int *count);

OMPI_DECLSPEC int mca_common_monitoring_notify_flush(struct mca_base_pvar_t *pvar,
                                                     mca_base_pvar_event_t event,
                                                     void *obj, int *count);

OMPI_DECLSPEC int mca_common_monitoring_comm_size_notify(mca_base_pvar_t *pvar,
                                                         mca_base_pvar_event_t event,
                                                         void *obj_handle, int *count);

OMPI_DECLSPEC void mca_common_monitoring_register(void*pml_monitoring_component);
OMPI_DECLSPEC void mca_common_monitoring_init( void );
OMPI_DECLSPEC void mca_common_monitoring_finalize( void );
OMPI_DECLSPEC void mca_common_monitoring_send_data(int world_rank, size_t data_size, int tag);
OMPI_DECLSPEC void mca_common_monitoring_recv_data(int world_rank, size_t data_size, int tag);
OMPI_DECLSPEC int mca_common_monitoring_add_procs(struct ompi_proc_t **procs, size_t nprocs);

OMPI_DECLSPEC opal_hash_table_t*mca_common_monitoring_get_translation_ht();

static inline int mca_common_monitoring_get_world_rank(int dst, struct ompi_communicator_t*comm,
                                                       int*world_rank)
{
    opal_process_name_t tmp;
    
    /* find the processor of the destination */
    ompi_proc_t *proc = ompi_group_get_proc_ptr(comm->c_remote_group, dst, true);
    if( ompi_proc_is_sentinel(proc) ) {
        tmp = ompi_proc_sentinel_to_name((uintptr_t)proc);
    } else {
        tmp = proc->super.proc_name;
    }
    
    /* find its name*/
    uint64_t key = *((uint64_t*)&tmp);
    /**
     * If this fails the destination is not part of my MPI_COM_WORLD
     * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
     */
    return opal_hash_table_get_value_uint64(mca_common_monitoring_get_translation_ht(),
                                            key, (void *)world_rank);
}

/* Return the current status of the monitoring system 0 if off, 1 if the
 * seperation between internal tags and external tags is enabled. Any other
 * positive value if the segregation between point-to-point and collective is
 * disabled.
 */
static inline int mca_common_monitoring_filter( void )
{
    return 1 > mca_common_monitoring_current_state;
}

/* Collective operation monitoring */
struct mca_monitoring_coll_data_t;
typedef struct mca_monitoring_coll_data_t mca_monitoring_coll_data_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_monitoring_coll_data_t);

OMPI_DECLSPEC mca_monitoring_coll_data_t*mca_common_monitoring_coll_new(ompi_communicator_t*comm);
OMPI_DECLSPEC void mca_common_monitoring_coll_release(mca_monitoring_coll_data_t*data);
OMPI_DECLSPEC void mca_common_monitoring_coll_flush(FILE *pf, mca_monitoring_coll_data_t*data);
OMPI_DECLSPEC void mca_common_monitoring_coll_flush_all(FILE *pf);
OMPI_DECLSPEC void mca_common_monitoring_coll_o2a(uint64_t size, mca_monitoring_coll_data_t*data);
OMPI_DECLSPEC void mca_common_monitoring_coll_a2o(uint64_t size, mca_monitoring_coll_data_t*data);
OMPI_DECLSPEC void mca_common_monitoring_coll_a2a(uint64_t size, mca_monitoring_coll_data_t*data);

END_C_DECLS

#endif  /* MCA_COMMON_MONITORING_H */
