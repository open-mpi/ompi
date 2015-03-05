/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <pml_monitoring.h>

extern void monitor_send_data(int dest_rank, size_t data_size, int tag);
extern opal_hash_table_t *get_hashtable(void);
extern opal_hash_table_t *translation_ht;

int mca_pml_monitoring_isend_init(void *buf,
                                  size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst,
                                  int tag,
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request)
{
    return pml_selected_module.pml_isend_init(buf, count, datatype,
                                              dst, tag, mode, comm, request);
}

int mca_pml_monitoring_isend(void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int dst,
                             int tag,
                             mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{

    /* find the processor of teh destination */
    ompi_proc_t *proc = ompi_group_get_proc_ptr(comm->c_remote_group, dst);
    int world_rank;

    /* find its name*/
    uint64_t key = *((uint64_t*)&(proc->super.proc_name));
    /**
     * If this fails the destination is not part of my MPI_COM_WORLD
     * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
     */
    if(OPAL_SUCCESS == opal_hash_table_get_value_uint64(translation_ht, key, (void *)&world_rank)) {
        size_t type_size, data_size;
        ompi_datatype_type_size(datatype, &type_size);
        data_size = count*type_size;
        monitor_send_data(world_rank, data_size, tag);
    }

    return pml_selected_module.pml_isend(buf, count, datatype,
                                         dst, tag, mode, comm, request);
}

int mca_pml_monitoring_send(void *buf,
                            size_t count,
                            ompi_datatype_t *datatype,
                            int dst,
                            int tag,
                            mca_pml_base_send_mode_t mode,
                            struct ompi_communicator_t* comm)
{

    ompi_proc_t *proc = ompi_group_get_proc_ptr(comm->c_remote_group, dst);
    int world_rank;
    uint64_t key = *((uint64_t*) &(proc->super.proc_name));

    /**
     * If this fails the destination is not part of my MPI_COM_WORLD
     */
    if(OPAL_SUCCESS == opal_hash_table_get_value_uint64(translation_ht, key, (void *)&world_rank)) {
        size_t type_size, data_size;
        ompi_datatype_type_size(datatype, &type_size);
        data_size = count*type_size;
        monitor_send_data(world_rank, data_size, tag);
    }


    return pml_selected_module.pml_send(buf, count, datatype,
                                        dst, tag, mode, comm);
}

