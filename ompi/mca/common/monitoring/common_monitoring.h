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
#include <opal/mca/base/mca_base_pvar.h>

struct ompi_proc_t;

#define MCA_MONITORING_MAKE_VERSION MCA_BASE_MAKE_VERSION(component, \
                                                          OMPI_MAJOR_VERSION, \
                                                          OMPI_MINOR_VERSION, \
                                                          OMPI_RELEASE_VERSION)

OMPI_DECLSPEC int mca_common_monitoring_enabled;
OMPI_DECLSPEC int mca_common_monitoring_output_enabled;
OMPI_DECLSPEC int mca_common_monitoring_active;
OMPI_DECLSPEC int mca_common_monitoring_current_state;
OMPI_DECLSPEC char**mca_common_monitoring_current_filename;

OMPI_DECLSPEC int common_monitoring_get_messages_count (const struct mca_base_pvar_t *pvar,
                                                        void *value, void *obj_handle);

OMPI_DECLSPEC int common_monitoring_get_messages_size (const struct mca_base_pvar_t *pvar,
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

END_C_DECLS

#endif  /* MCA_COMMON_MONITORING_H */
