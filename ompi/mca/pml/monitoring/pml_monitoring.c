/*
 * Copyright (c) 2013-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <pml_monitoring.h>
#include "opal/class/opal_hash_table.h"

/* array for stroring monitoring data*/
uint64_t* sent_data = NULL;
uint64_t* messages_count = NULL;
uint64_t* filtered_sent_data = NULL;
uint64_t* filtered_messages_count = NULL;

opal_hash_table_t *translation_ht = NULL;


mca_pml_monitoring_module_t mca_pml_monitoring = {
    mca_pml_monitoring_add_procs,
    mca_pml_monitoring_del_procs,
    mca_pml_monitoring_enable,
    NULL,
    mca_pml_monitoring_add_comm,
    mca_pml_monitoring_del_comm,
    mca_pml_monitoring_irecv_init,
    mca_pml_monitoring_irecv,
    mca_pml_monitoring_recv,
    mca_pml_monitoring_isend_init,
    mca_pml_monitoring_isend,
    mca_pml_monitoring_send,
    mca_pml_monitoring_iprobe,
    mca_pml_monitoring_probe,
    mca_pml_monitoring_start,
    mca_pml_monitoring_improbe,
    mca_pml_monitoring_mprobe,
    mca_pml_monitoring_imrecv,
    mca_pml_monitoring_mrecv,
    mca_pml_monitoring_dump,
    NULL,
    65535,
    INT_MAX
};

/**
 * This PML monitors only the processes in the MPI_COMM_WORLD. As OMPI is now lazily
 * adding peers on the first call to add_procs we need to check how many processes
 * are in the MPI_COMM_WORLD to create the storage with the right size.
 */
int mca_pml_monitoring_add_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    opal_process_name_t tmp, wp_name;
    size_t i, peer_rank, nprocs_world;
    uint64_t key;

    nprocs_world = ompi_comm_size((ompi_communicator_t*)&ompi_mpi_comm_world);
    if( NULL == translation_ht ) {
        translation_ht = OBJ_NEW(opal_hash_table_t);
        opal_hash_table_init(translation_ht, 2048);

        sent_data               = (uint64_t*)calloc(4 * nprocs_world, sizeof(uint64_t));
        messages_count          = sent_data + nprocs_world;
        filtered_sent_data      = messages_count + nprocs_world;
        filtered_messages_count = filtered_sent_data + nprocs_world;
    }

    /* For all procs in the same MPI_COMM_WORLD we need to add them to the hash table */
    for( i = 0; i < nprocs; i++ ) {

        /* Extract the peer procname from the procs array */
        if( ompi_proc_is_sentinel(procs[i]) ) {
            tmp = ompi_proc_sentinel_to_name((uintptr_t)procs[i]);
        } else {
            tmp = procs[i]->super.proc_name;
        }
        if( tmp.jobid != ompi_proc_local_proc->super.proc_name.jobid )
            continue;

        /* each process will only be added once, so there is no way it already exists in the hash */
        for( peer_rank = 0; peer_rank < nprocs_world; peer_rank++ ) {
            wp_name = ompi_group_get_proc_name(((ompi_communicator_t*)&ompi_mpi_comm_world)->c_remote_group, peer_rank);
            if( 0 != opal_compare_proc( tmp, wp_name) )
                continue;

            key = *((uint64_t*)&tmp);
            /* save the rank of the process in MPI_COMM_WORLD in the hash using the proc_name as the key */
            if( OPAL_SUCCESS != opal_hash_table_set_value_uint64(translation_ht,
                                                                 key, (void*)(uintptr_t)peer_rank) ) {
                return OMPI_ERR_OUT_OF_RESOURCE;  /* failed to allocate memory or growing the hash table */
            }
            break;
        }
    }
    return pml_selected_module.pml_add_procs(procs, nprocs);
}

/**
 * Pass the information down the PML stack.
 */
int mca_pml_monitoring_del_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    return pml_selected_module.pml_del_procs(procs, nprocs);
}

int mca_pml_monitoring_dump(struct ompi_communicator_t* comm,
                            int verbose)
{
    return pml_selected_module.pml_dump(comm, verbose);
}


void finalize_monitoring( void )
{
    free(sent_data);  /* a single allocation */
    opal_hash_table_remove_all( translation_ht );
    free(translation_ht);
}

void mca_pml_monitoring_reset( void )
{
    int nbprocs = ompi_comm_size((ompi_communicator_t*)&ompi_mpi_comm_world);
    memset(sent_data, 0, nbprocs * sizeof(uint64_t));
    memset(messages_count, 0, nbprocs * sizeof(uint64_t));
    memset(filtered_sent_data, 0, nbprocs * sizeof(uint64_t));
    memset(filtered_messages_count, 0, nbprocs * sizeof(uint64_t));
}

void monitor_send_data(int world_rank, size_t data_size, int tag)
{
    if( 0 == filter_monitoring() ) return;  /* right now the monitoring is not started */

    /* distinguishses positive and negative tags if requested */
    if( (tag < 0) && (1 == filter_monitoring()) ) {
        filtered_sent_data[world_rank] += data_size;
        filtered_messages_count[world_rank]++;
    } else { /* if filtered monitoring is not activated data is aggregated indifferently */
        sent_data[world_rank] += data_size;
        messages_count[world_rank]++;
    }
}

int mca_pml_monitoring_get_messages_count(const struct mca_base_pvar_t *pvar,
                                          void *value,
                                          void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    int i, comm_size = ompi_comm_size (comm);
    uint64_t *values = (uint64_t*) value;

    if(comm != &ompi_mpi_comm_world.comm || NULL == messages_count)
        return OMPI_ERROR;

    for (i = 0 ; i < comm_size ; ++i) {
        values[i] = messages_count[i];
    }

    return OMPI_SUCCESS;
}

int mca_pml_monitoring_get_messages_size(const struct mca_base_pvar_t *pvar,
                                         void *value,
                                         void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    int comm_size = ompi_comm_size (comm);
    uint64_t *values = (uint64_t*) value;
    int i;

    if(comm != &ompi_mpi_comm_world.comm || NULL == sent_data)
        return OMPI_ERROR;

    for (i = 0 ; i < comm_size ; ++i) {
        values[i] = sent_data[i];
    }

    return OMPI_SUCCESS;
}

static void output_monitoring( FILE *pf )
{
    if( 0 == filter_monitoring() ) return;  /* if disabled do nothing */

    int nbprocs = ompi_comm_size((ompi_communicator_t*)&ompi_mpi_comm_world);
    int my_rank = ompi_comm_rank((ompi_communicator_t*)&ompi_mpi_comm_world);

    for (int i = 0 ; i < nbprocs ; i++) {
        if(sent_data[i] > 0) {
            fprintf(pf, "I\t%d\t%d\t%" PRIu64 " bytes\t%" PRIu64 " msgs sent\n",
                    my_rank, i, sent_data[i], messages_count[i]);
        }
        /* reset phase array */
        sent_data[i] = 0;
        messages_count[i] = 0;
    }

    if( 1 == filter_monitoring() ) return;

    for (int i = 0 ; i < nbprocs ; i++) {
        if(filtered_sent_data[i] > 0) {
            fprintf(pf, "E\t%d\t%d\t%" PRIu64 " bytes\t%" PRIu64 " msgs sent\n",
                    my_rank, i, filtered_sent_data[i], filtered_messages_count[i]);
        }
        /* reset phase array */
        filtered_sent_data[i] = 0;
        filtered_messages_count[i] = 0;
    }
}


/*
   Flushes the monitoring into filename
   Useful for phases (see example in test/monitoring)
*/
int ompi_mca_pml_monitoring_flush(char* filename)
{
    FILE *pf = NULL;
    int my_rank = ompi_comm_rank((ompi_communicator_t*)&ompi_mpi_comm_world);

    if( NULL != filename )
        pf = fopen(filename, "w");
    if(NULL == pf)  /* No filename or error during open */
        return -1;

    fprintf(stderr, "Proc %d flushing monitoring to: %s\n", my_rank, filename);
    output_monitoring( pf );

    fclose(pf);
    return 0;
}
