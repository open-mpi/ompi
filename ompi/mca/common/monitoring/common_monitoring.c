/*
 * Copyright (c) 2013-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Inria.  All rights reserved.
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
#include <common_monitoring.h>
#include <ompi/constants.h>
#include <ompi/communicator/communicator.h>
#include <opal/mca/base/mca_base_component_repository.h>
#include <opal/class/opal_hash_table.h>
#include <opal/util/output.h>
#include <math.h>

/*** Monitoring specific variables ***/
/* Keep tracks of how many components are currently using the common part */
uint32_t mca_common_monitoring_hold = 0;
/* Output parameters */
int mca_common_monitoring_output_stream_id = -1;
static opal_output_stream_t mca_common_monitoring_output_stream_obj = {
    .lds_verbose_level = 0,
    .lds_want_syslog = false,
    .lds_prefix = NULL,
    .lds_suffix = NULL,
    .lds_is_debugging = true,
    .lds_want_stdout = false,
    .lds_want_stderr = true,
    .lds_want_file = false,
    .lds_want_file_append = false,
    .lds_file_suffix = NULL
};

/*** MCA params to mark the monitoring as enabled. ***/
/* This signals that the monitoring will highjack the PML or OSC */
int mca_common_monitoring_enabled = 0;
/* Signals there will be an output of the monitored data at component close */
int mca_common_monitoring_output_enabled = 0;
int mca_common_monitoring_active = 0;
int mca_common_monitoring_current_state = 0;
/* File where to output the monitored data */
char** mca_common_monitoring_current_filename = NULL;

/* array for stroring monitoring data*/
uint64_t* sent_data = NULL;
uint64_t* recv_data = NULL;
uint64_t* messages_count = NULL;
uint64_t* rmessages_count = NULL;
uint64_t* filtered_sent_data = NULL;
uint64_t* filtered_messages_count = NULL;

uint64_t* size_histogram = NULL;
const int max_size_histogram = 66;
double log10_2 = 0.;

int rank_world = -1;
int nprocs_world = 0;

opal_hash_table_t *translation_ht = NULL;

/* Reset all the monitoring arrays */
static void mca_common_monitoring_reset( void );

/* Flushes the monitored data and reset the values */
static int mca_common_monitoring_flush(int fd, char* filename);

/* Return the current status of the monitoring system 0 if off, 1 if the
 * seperation between internal tags and external tags is enabled. Any other
 * positive value if the segregation between point-to-point and collective is
 * disabled.
 */
int mca_common_monitoring_filter( void )
{
    return mca_common_monitoring_current_state;
}

inline opal_hash_table_t*mca_common_monitoring_get_translation_ht()
{
    return translation_ht;
}

int mca_common_monitoring_set_flush(struct mca_base_pvar_t *pvar,
                                    const void *value, void *obj)
{
    if( NULL != *mca_common_monitoring_current_filename ) {
        free(*mca_common_monitoring_current_filename);
    }
    if( NULL == *(char**)value || 0 == strlen((char*)value) ) {  /* No more output */
        *mca_common_monitoring_current_filename = NULL;
    } else {
        *mca_common_monitoring_current_filename = strdup((char*)value);
        if( NULL == *mca_common_monitoring_current_filename )
            return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

int mca_common_monitoring_get_flush(const struct mca_base_pvar_t *pvar,
                                    void *value, void *obj)
{
    return OMPI_SUCCESS;
}

int mca_common_monitoring_notify_flush(struct mca_base_pvar_t *pvar,
                                       mca_base_pvar_event_t event,
                                       void *obj, int *count)
{
    int rank, size;

    switch (event) {
    case MCA_BASE_PVAR_HANDLE_BIND:
        mca_common_monitoring_reset();
        *count = (NULL == mca_common_monitoring_current_filename
                  || NULL == *mca_common_monitoring_current_filename
                  ? 0 : strlen(*mca_common_monitoring_current_filename));
    case MCA_BASE_PVAR_HANDLE_UNBIND:
        return OMPI_SUCCESS;
    case MCA_BASE_PVAR_HANDLE_START:
        mca_common_monitoring_current_state = mca_common_monitoring_enabled;
        mca_common_monitoring_output_enabled = 0;  /* we can't control the monitoring via MPIT and expect
                                                    * accurate answer upon MPI_Finalize. */
        return OMPI_SUCCESS;
    case MCA_BASE_PVAR_HANDLE_STOP:
        return mca_common_monitoring_flush(3, *mca_common_monitoring_current_filename);
    }
    return OMPI_ERROR;
}

int mca_common_monitoring_messages_notify(mca_base_pvar_t *pvar,
                                          mca_base_pvar_event_t event,
                                          void *obj_handle,
                                          int *count)
{
    switch (event) {
    case MCA_BASE_PVAR_HANDLE_BIND:
        /* Return the size of the communicator as the number of values */
        *count = ompi_comm_size ((ompi_communicator_t *) obj_handle);
    case MCA_BASE_PVAR_HANDLE_UNBIND:
        return OMPI_SUCCESS;
    case MCA_BASE_PVAR_HANDLE_START:
        mca_common_monitoring_current_state = mca_common_monitoring_enabled;
        return OMPI_SUCCESS;
    case MCA_BASE_PVAR_HANDLE_STOP:
        mca_common_monitoring_current_state = 0;
        return OMPI_SUCCESS;
    }

    return OMPI_ERROR;
}

void mca_common_monitoring_init( void )
{
    char hostname[OPAL_MAXHOSTNAMELEN] = "NA";
    /* If first to enable the common parts */
    if( 1 == opal_atomic_add_32(&mca_common_monitoring_hold, 1) ) {
        /* Initialize constant */
        log10_2 = log10(2.);
        /* Open the opal_output stream */
        gethostname(hostname, sizeof(hostname));
        asprintf(&mca_common_monitoring_output_stream_obj.lds_prefix,
                 "[%s:%05d] monitoring: ", hostname, getpid());
        mca_common_monitoring_output_stream_id =
            opal_output_open(&mca_common_monitoring_output_stream_obj);
        /* Initialize proc translation hashtable */
        translation_ht = OBJ_NEW(opal_hash_table_t);
        opal_hash_table_init(translation_ht, 2048);
        OPAL_MONITORING_PRINT_INFO("common_component_init");
    }
}

void mca_common_monitoring_finalize( void )
{
    if( 0 == opal_atomic_sub_32(&mca_common_monitoring_hold, 1) /* Release if last component */
        && mca_common_monitoring_enabled && mca_common_monitoring_active ) {
        OPAL_MONITORING_PRINT_INFO("common_component_finish");
        /* If we are not drived by MPIT then dump the monitoring information */
        if( mca_common_monitoring_output_enabled ) {
  	    mca_common_monitoring_flush(mca_common_monitoring_output_enabled,
                                        *mca_common_monitoring_current_filename);
        }
        /* Disable all monitoring */
        mca_common_monitoring_active = 0;
        mca_common_monitoring_enabled = 0;
        /* Close the opal_output stream */
        opal_output_close(mca_common_monitoring_output_stream_id);
        free(mca_common_monitoring_output_stream_obj.lds_prefix);
        /* Free internal data structure */
        free(sent_data);  /* a single allocation */
        opal_hash_table_remove_all( translation_ht );
        OBJ_RELEASE(translation_ht);
    }
}

void mca_common_monitoring_enable(bool enable, void*pml_monitoring_component)
{
    /* If we reach this point we were succesful at hijacking the interface of
     * the real PML, and we are now correctly interleaved between the upper
     * layer and the real PML.
     */
    if( NULL != pml_monitoring_component ) {
        (void)mca_base_pvar_register("ompi", "pml", "monitoring", "flush", "Flush the monitoring information"
                                     "in the provided file", OPAL_INFO_LVL_1, MCA_BASE_PVAR_CLASS_GENERIC,
                                     MCA_BASE_VAR_TYPE_STRING, NULL, MPI_T_BIND_NO_OBJECT,
                                     0,
                                     mca_common_monitoring_get_flush, mca_common_monitoring_set_flush,
                                     mca_common_monitoring_notify_flush, pml_monitoring_component);
    }
    
    (void)mca_base_pvar_register("ompi", "pml", "monitoring", "messages_count", "Number of messages "
                                 "sent to each peer in a communicator", OPAL_INFO_LVL_4,
                                 MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY,
                                 mca_common_monitoring_get_messages_count, NULL,
                                 mca_common_monitoring_messages_notify, NULL);
    
    (void)mca_base_pvar_register("ompi", "pml", "monitoring", "messages_size", "Size of messages "
                                 "sent to each peer in a communicator", OPAL_INFO_LVL_4,
                                 MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY,
                                 mca_common_monitoring_get_messages_size, NULL,
                                 mca_common_monitoring_messages_notify, NULL);

    (void)mca_base_pvar_register("ompi", "osc", "monitoring", "messages_count", "Number of messages "
                                 "receive from each peer in a communicator", OPAL_INFO_LVL_4,
                                 MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY,
                                 mca_common_monitoring_get_rmessages_count, NULL,
                                 mca_common_monitoring_messages_notify, NULL);
    
    (void)mca_base_pvar_register("ompi", "osc", "monitoring", "messages_size", "Size of messages "
                                 "receive from each peer in a communicator", OPAL_INFO_LVL_4,
                                 MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY,
                                 mca_common_monitoring_get_rmessages_size, NULL,
                                 mca_common_monitoring_messages_notify, NULL);
}

int mca_common_monitoring_comm_size_notify(mca_base_pvar_t *pvar,
                                           mca_base_pvar_event_t event,
                                           void *obj_handle,
                                           int *count)
{
    if (MCA_BASE_PVAR_HANDLE_BIND == event) {
        /* Return the size of the communicator as the number of values */
        *count = ompi_comm_size ((ompi_communicator_t *) obj_handle);
    }

    return OMPI_SUCCESS;
}

/**
 * This PML monitors only the processes in the MPI_COMM_WORLD. As OMPI is now lazily
 * adding peers on the first call to add_procs we need to check how many processes
 * are in the MPI_COMM_WORLD to create the storage with the right size.
 */
int mca_common_monitoring_add_procs(struct ompi_proc_t **procs,
                                    size_t nprocs)
{
    opal_process_name_t tmp, wp_name;
    size_t i, peer_rank;
    uint64_t key;
    if( 0 > rank_world )
        rank_world = ompi_comm_rank((ompi_communicator_t*)&ompi_mpi_comm_world);
    if( !nprocs_world )
        nprocs_world = ompi_comm_size((ompi_communicator_t*)&ompi_mpi_comm_world);
    
    if( NULL == sent_data ) {
        int array_size = 6 + max_size_histogram * nprocs_world;
        sent_data               = (uint64_t*)calloc(array_size, sizeof(uint64_t));
        recv_data               = sent_data + nprocs_world;
        messages_count          = recv_data + nprocs_world;
        rmessages_count         = messages_count + nprocs_world;
        filtered_sent_data      = rmessages_count + nprocs_world;
        filtered_messages_count = filtered_sent_data + nprocs_world;

        size_histogram = filtered_messages_count + nprocs_world;
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
    return OMPI_SUCCESS;
}

static void mca_common_monitoring_reset( void )
{
    int array_size = 6 + max_size_histogram * nprocs_world;
    memset(sent_data, 0, array_size * sizeof(uint64_t));
}

void mca_common_monitoring_send_data(int world_rank, size_t data_size, int tag)
{
    if( 0 == mca_common_monitoring_filter() ) return;  /* right now the monitoring is not started */

    /* Keep tracks of the data_size distribution */
    if( 0 == data_size ) {
        opal_atomic_add_64(&size_histogram[world_rank * max_size_histogram], 1);
    } else {
        int log2_size = log10(data_size)/log10_2;
        opal_atomic_add_64(&size_histogram[world_rank * max_size_histogram + log2_size + 1], 1);
    }
        
    /* distinguishses positive and negative tags if requested */
    if( (tag < 0) && (1 == mca_common_monitoring_filter()) ) {
        filtered_sent_data[world_rank] += data_size;
        filtered_messages_count[world_rank]++;
    } else { /* if filtered monitoring is not activated data is aggregated indifferently */
        sent_data[world_rank] += data_size;
        messages_count[world_rank]++;
    }
}

void mca_common_monitoring_recv_data(int world_rank, size_t data_size, int tag)
{
    if( 0 == mca_common_monitoring_filter() ) return;  /* right now the monitoring is not started */
    recv_data[world_rank] += data_size;
    rmessages_count[world_rank]++;
}

int mca_common_monitoring_get_messages_count(const struct mca_base_pvar_t *pvar,
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

int mca_common_monitoring_get_messages_size(const struct mca_base_pvar_t *pvar,
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

int mca_common_monitoring_get_rmessages_count(const struct mca_base_pvar_t *pvar,
                                              void *value,
                                              void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    int i, comm_size = ompi_comm_size (comm);
    uint64_t *values = (uint64_t*) value;

    if(comm != &ompi_mpi_comm_world.comm || NULL == messages_count)
        return OMPI_ERROR;

    for (i = 0 ; i < comm_size ; ++i) {
        values[i] = rmessages_count[i];
    }

    return OMPI_SUCCESS;
}

int mca_common_monitoring_get_rmessages_size(const struct mca_base_pvar_t *pvar,
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
        values[i] = recv_data[i];
    }

    return OMPI_SUCCESS;
}

static void mca_common_monitoring_output( FILE *pf, int my_rank, int nbprocs )
{
    /* Dump outgoing messages */
    fprintf(pf, "# POINT TO POINT\n");
    for (int i = 0 ; i < nbprocs ; i++) {
        if(messages_count[i] > 0) {
            fprintf(pf, "I\t%d\t%d\t%" PRIu64 " bytes\t%" PRIu64 " msgs sent\t",
                    my_rank, i, sent_data[i], messages_count[i]);
            for(int j = 0 ; j < max_size_histogram ; ++j)
                fprintf(pf, "%" PRIu64 "%s", size_histogram[i * nbprocs + j],
                        j < max_size_histogram - 1 ? "," : "\n");
        }
        /* reset phase array */
        sent_data[i] = 0;
        messages_count[i] = 0;
    }

    /* Dump outgoing synchronization/collective messages */
    if( 1 == mca_common_monitoring_filter() ) {
        for (int i = 0 ; i < nbprocs ; i++) {
            if(filtered_messages_count[i] > 0) {
                fprintf(pf, "E\t%d\t%d\t%" PRIu64 " bytes\t%" PRIu64 " msgs sent\n",
                        my_rank, i, filtered_sent_data[i], filtered_messages_count[i]);
            }
            /* reset phase array */
            filtered_sent_data[i] = 0;
            filtered_messages_count[i] = 0;
        }
    }

    /* Dump incoming messages */
    fprintf(pf, "# RMA\n");
    for (int i = 0 ; i < nbprocs ; i++) {
        if(rmessages_count[i] > 0) {
            fprintf(pf, "R\t%d\t%d\t%" PRIu64 " bytes\t%" PRIu64 " msgs sent\n",
                    my_rank, i, recv_data[i], rmessages_count[i]);
        }
        /* reset phase array */
        recv_data[i] = 0;
        rmessages_count[i] = 0;
    }

    /* Dump collectives */
    mca_common_monitoring_coll_flush_all(pf);
}

/*
 * Flushes the monitoring into filename
 * Useful for phases (see example in test/monitoring)
 */
static int mca_common_monitoring_flush(int fd, char* filename)
{
    if( 0 == mca_common_monitoring_filter() || 0 == fd ) /* if disabled do nothing */
        return OMPI_SUCCESS;

    if( 1 == fd ) {
        OPAL_MONITORING_PRINT_INFO("Proc %d flushing monitoring to stdout", rank_world);
        mca_common_monitoring_output( stdout, rank_world, nprocs_world );
    } else if( 2 == fd ) {
        OPAL_MONITORING_PRINT_INFO("Proc %d flushing monitoring to stderr", rank_world);
        mca_common_monitoring_output( stderr, rank_world, nprocs_world );
    } else {
        FILE *pf = NULL;
        char* tmpfn = NULL;

        if( NULL == filename ) { /* No filename */
            OPAL_MONITORING_PRINT_ERR("Error while flushing: no filename provided");
            return -1;
        } else {
            asprintf(&tmpfn, "%s.%d.prof", filename, rank_world);
            pf = fopen(tmpfn, "w");
            free(tmpfn);
        }

        if(NULL == pf) {  /* Error during open */
            OPAL_MONITORING_PRINT_ERR("Error while flushing to: %s.%d.prof",
                                    filename, rank_world);
            return -1;
        }

        OPAL_MONITORING_PRINT_INFO("Proc %d flushing monitoring to: %s.%d.prof",
                                rank_world, filename, rank_world);

        mca_common_monitoring_output( pf, rank_world, nprocs_world );

        fclose(pf);
    }
    return OMPI_SUCCESS;
}
