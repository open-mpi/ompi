/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include <stdio.h>
#include "mpi.h"

#include "ompi/mca/dpm/dpm.h"

static ompi_communicator_t *newcomp;
static ompi_proc_t *sender;

static void xnt(ompi_communicator_t *newcomm,
                ompi_proc_t *remote_proc,
                void *cbdata)
{
    bool *lock = (bool*)cbdata;
    newcomp = newcomm;
    sender = remote_proc;
    *lock = false;
}

int main(int argc, char* argv[])
{
    char port[1024];
    bool lock;
    FILE *fp;
    int rank, rc;
    int msg;

    /* program requires an argument specifying the file where
     * the connection info is to be found
     */
    if (2 != argc) {
        fprintf(stderr, "Usage: pconnect <filename>\n");
        exit(1);
    }

    MPI_Init(&argc, &argv);

    /* read the file */
    fp = fopen(argv[1], "r");
    fgets(port, 1024, fp);
    port[strlen(port)-1] = '\0';  /* remove newline */
    fclose(fp);

    /* start the connect */
    lock = true;
    if (OMPI_SUCCESS != ompi_dpm.pconnect(port, NULL, xnt, &lock)) {
        fprintf(stderr, "Failed to start connect\n");
        goto cleanup;
    }

    /* wait for completion */
    OMPI_WAIT_FOR_COMPLETION(lock);

    /* allocate comm_cid */
    rank = ompi_comm_rank(MPI_COMM_SELF);
    rc = ompi_comm_nextcid(newcomp,                 /* new communicator */
                           MPI_COMM_SELF,           /* old communicator */
                           NULL,                    /* bridge comm */
                           &rank,                   /* local leader */
                           &sender->super.proc_name,      /* remote leader */
                           OMPI_COMM_CID_INTRA_OOB, /* mode */
                           true);                   /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        fprintf(stderr, "Failed to negotiate cid\n");
        goto cleanup;
    }

    /* activate comm and init coll-component */
    rc = ompi_comm_activate(&newcomp,                /* new communicator */
                            MPI_COMM_SELF,           /* old communicator */
                            NULL,                    /* bridge comm */
                            &rank,                   /* local leader */
                            &sender->super.proc_name,      /* remote leader */
                            OMPI_COMM_CID_INTRA_OOB, /* mode */
                            true);                   /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        fprintf(stderr, "Failed to activate communicator\n");
        goto cleanup;
    }

    fprintf(stderr, "HANDSHAKE COMPLETE\n");

    msg = 38;
    MPI_Send(&msg, 1, MPI_INT, 0, 1, newcomp);
    MPI_Comm_disconnect(&newcomp);

    fprintf(stderr, "MESSAGE SENT\n");

 cleanup:
    MPI_Finalize();
    return 0;
}
