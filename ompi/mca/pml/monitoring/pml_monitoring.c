/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <pml_monitoring.h>
#include "opal/class/opal_hash_table.h"
typedef struct _transtlator_t{
  int *ranks;
  int size;
} translator_t;


void initialize_monitoring( void );
void monitor_send_data(int world_rank, size_t data_size, int tag);
void finalize_monitoring( void );
int filter_monitoring( void ); /* returns 1 if we distinguish positive (point-to-point) and negative (collective and meta messages) tags*/
int ompi_mca_pml_monitoring_flush(char* filename);


MPI_Group group_world;

/* array for stroring monitoring data*/
uint64_t* sent_data = NULL;
uint64_t* messages_count = NULL;
uint64_t* filtered_sent_data = NULL;
uint64_t* filtered_messages_count = NULL;
uint64_t* all_sent_data = NULL;
uint64_t* all_messages_count = NULL;
uint64_t* all_filtered_sent_data = NULL;
uint64_t* all_filtered_messages_count = NULL;

static int init_done = 0;
static int nbprocs = -1;
static int my_rank = -1;
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

int mca_pml_monitoring_add_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    /**
     * Create the monitoring hashtable only for my MPI_COMM_WORLD. We choose
     * to ignore by now all other processes.
     */
    if(NULL == translation_ht) {
        size_t i;
        uint64_t key;

        nbprocs = nprocs;

        translation_ht = OBJ_NEW(opal_hash_table_t);
        opal_hash_table_init(translation_ht, 2048);


        for( i = 0; i < nprocs; i++ ) {
            /* rank : ompi_proc_local_proc in procs */
            if( procs[i] == ompi_proc_local_proc)
                my_rank = i;
            key = *((uint64_t*)&(procs[i]->super.proc_name));
            /* store the rank (in COMM_WORLD) of the process
               with its name (a uniq opal ID) as key  in the hash table*/
            opal_hash_table_set_value_uint64(translation_ht,
                                             key,
                                             (void*)(uintptr_t)i);
        }
    }
    return pml_selected_module.pml_add_procs(procs, nprocs);
}


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
    if(filter_monitoring()){
        free(filtered_sent_data);
        free(filtered_messages_count);
    }

    free(sent_data);
    free(messages_count);
    opal_hash_table_remove_all( translation_ht );
    free(translation_ht);

}
void initialize_monitoring( void )
{
    sent_data      = (uint64_t*)calloc(nbprocs, sizeof(uint64_t));
    messages_count = (uint64_t*)   calloc(nbprocs, sizeof(uint64_t));
    all_sent_data      = (uint64_t*)calloc(nbprocs, sizeof(uint64_t));
    all_messages_count = (uint64_t*)   calloc(nbprocs, sizeof(uint64_t));

    if(filter_monitoring()){
        filtered_sent_data      = (uint64_t*)calloc(nbprocs, sizeof(uint64_t));
        filtered_messages_count = (uint64_t*)   calloc(nbprocs, sizeof(uint64_t));
        all_filtered_sent_data      = (uint64_t*)calloc(nbprocs, sizeof(uint64_t));
        all_filtered_messages_count = (uint64_t*)   calloc(nbprocs, sizeof(uint64_t));
    }

    init_done = 1;
}



void monitor_send_data(int world_rank, size_t data_size, int tag)
{
    if ( !init_done )
        initialize_monitoring();

    /* distinguishses positive and negative tags if requested */
    if((tag<0) && (filter_monitoring())){
        filtered_sent_data[world_rank] += data_size;
        filtered_messages_count[world_rank]++;
    }else{ /* if filtered monitoring is not activated data is aggregated indifferently */
        sent_data[world_rank] += data_size;
        messages_count[world_rank]++;
    }
    /*printf("%d Send dest = %d(%d:comm_world=%d), size = %ld ajouté dans : %d\n",my_rank, dest_rank, comm->c_my_rank, MPI_COMM_WORLD->c_my_rank, data_size, rank); fflush(stdout);*/


}

int mca_pml_monitoring_get_messages_count (const struct mca_base_pvar_t *pvar, void *value, void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    int comm_size = ompi_comm_size (comm);
    uint64_t *values = (uint64_t*) value;
    int i;

    if(comm != &ompi_mpi_comm_world.comm)
        return OMPI_ERROR;

    for (i = 0 ; i < comm_size ; ++i) {
        values[i] = messages_count[i];
    }

    return OMPI_SUCCESS;
}

int mca_pml_monitoring_get_messages_size (const struct mca_base_pvar_t *pvar, void *value, void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    int comm_size = ompi_comm_size (comm);
    uint64_t *values = (uint64_t*) value;
    int i;

    if(comm != &ompi_mpi_comm_world.comm)
        return OMPI_ERROR;

    for (i = 0 ; i < comm_size ; ++i) {
        values[i] = sent_data[i];
    }

    return OMPI_SUCCESS;
}

static void output_monitoring( FILE *pf )
{
    int i;

    if ( !init_done ) return;

    for (i = 0 ; i < nbprocs ; i++) {
        if(all_sent_data[i] > 0) {
            /* aggregate data in general array*/
            all_sent_data[i] += sent_data[i];
            all_messages_count[i] += messages_count[i];
            fprintf(pf, "I\t%d\t%d\t" PRIu64 " bytes\t" PRIu64 " msgs sent\n",
                    my_rank, i, all_sent_data[i], all_messages_count[i]);
            fflush(pf);
        }
        /* reset phase array */
        sent_data[i] = 0;
        messages_count[i] = 0;
    }

    if( !filter_monitoring() ) return;
    
    for (i = 0 ; i < nbprocs ; i++) {
        if(all_filtered_sent_data[i] > 0) {
            /* aggregate data in general array*/
            all_filtered_sent_data[i] += filtered_sent_data[i];
            all_filtered_messages_count[i] += filtered_messages_count[i];
            fprintf(pf, "E\t%d\t%d\t" PRIu64 " bytes\t" PRIu64 " msgs sent\n",
                    my_rank, i, all_filtered_sent_data[i], all_filtered_messages_count[i]);
            fflush(pf);
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
    FILE *pf = stderr;

    if ( !init_done ) return -1;

    if( NULL != filename )
        pf = fopen(filename, "w");

    if(!pf)
        return -1;

    fprintf(stderr, "Proc %d flushing monitoring to: %s\n", my_rank, filename);
    output_monitoring( pf );

    if( NULL != filename )
        fclose(pf);
    return 0;
}
