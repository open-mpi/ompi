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

#define MCA_MONITORING_MAKE_VERSION MCA_BASE_MAKE_VERSION(component,    \
                                                          OMPI_MAJOR_VERSION, \
                                                          OMPI_MINOR_VERSION, \
                                                          OMPI_RELEASE_VERSION)

extern int mca_common_monitoring_enabled;
extern int mca_common_monitoring_output_enabled;
extern int mca_common_monitoring_active;
extern int mca_common_monitoring_current_state;
extern char**mca_common_monitoring_current_filename;

OMPI_DECLSPEC int common_monitoring_get_messages_count (const struct mca_base_pvar_t *pvar,
                                                        void *value, void *obj_handle);

OMPI_DECLSPEC int common_monitoring_get_messages_size (const struct mca_base_pvar_t *pvar,
                                                       void *value, void *obj_handle);

OMPI_DECLSPEC int common_monitoring_get_rmessages_count (const struct mca_base_pvar_t *pvar,
                                                         void *value, void *obj_handle);

OMPI_DECLSPEC int common_monitoring_get_rmessages_size (const struct mca_base_pvar_t *pvar,
                                                        void *value, void *obj_handle);

OMPI_DECLSPEC int common_monitoring_set_flush(struct mca_base_pvar_t *pvar,
                                              const void *value, void *obj);

OMPI_DECLSPEC int common_monitoring_get_flush(const struct mca_base_pvar_t *pvar,
                                              void *value, void *obj);

OMPI_DECLSPEC int common_monitoring_messages_notify(mca_base_pvar_t *pvar,
                                                    mca_base_pvar_event_t event,
                                                    void *obj_handle, int *count);

OMPI_DECLSPEC int common_monitoring_notify_flush(struct mca_base_pvar_t *pvar,
                                                 mca_base_pvar_event_t event,
                                                 void *obj, int *count);

OMPI_DECLSPEC int common_monitoring_comm_size_notify(mca_base_pvar_t *pvar,
                                                     mca_base_pvar_event_t event,
                                                     void *obj_handle, int *count);

OMPI_DECLSPEC void common_monitoring_enable(bool enable, void*pml_monitoring_component);
OMPI_DECLSPEC void common_monitoring_finalize( void );
OMPI_DECLSPEC int common_monitoring_filter( void );
OMPI_DECLSPEC void common_monitoring_reset( void );
OMPI_DECLSPEC int common_monitoring_flush(int fd, char* filename);
OMPI_DECLSPEC void common_monitoring_send_data(int world_rank, size_t data_size, int tag);
OMPI_DECLSPEC void common_monitoring_recv_data(int world_rank, size_t data_size, int tag);
OMPI_DECLSPEC int common_monitoring_add_procs(struct ompi_proc_t **procs, size_t nprocs);

OMPI_DECLSPEC opal_hash_table_t*common_monitoring_get_translation_ht();

static inline int common_monitoring_get_world_rank(int dst, struct ompi_communicator_t*comm,
                                                   int*world_rank)
{
    /* find the processor of the destination */
    ompi_proc_t *proc = ompi_group_get_proc_ptr(comm->c_remote_group, dst, true);

    /* find its name*/
    uint64_t key = *((uint64_t*)&(proc->super.proc_name));
    /**
     * If this fails the destination is not part of my MPI_COM_WORLD
     * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
     */
    return opal_hash_table_get_value_uint64(common_monitoring_get_translation_ht(),
                                            key, (void *)world_rank);
}

END_C_DECLS

#endif  /* MCA_COMMON_MONITORING_H */
