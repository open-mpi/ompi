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
    int msg=0;

    /* program requires an argument specifying the file where
     * the connection info is to be stored
     */
    if (2 != argc) {
        fprintf(stderr, "Usage: paccept <filename>\n");
        exit(1);
    }

    MPI_Init(&argc, &argv);

    /* get a port */
    if (OMPI_SUCCESS != ompi_dpm.open_port(port, ORTE_RML_TAG_INVALID)) {
        fprintf(stderr, "Failed to open port\n");
        goto cleanup;
    }

    /* put it in the file */
    fp = fopen(argv[1], "w");
    fprintf(fp, "%s\n", port);
    fclose(fp);

    /* register the accept */
    lock = true;
    if (OMPI_SUCCESS != ompi_dpm.paccept(port, xnt, &lock)) {
        fprintf(stderr, "Failed to setup accept\n");
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
                           false);                  /* send or recv first */
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
                            false);                  /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        fprintf(stderr, "Failed to activate communicator\n");
        goto cleanup;
    }

    fprintf(stderr, "HANDSHAKE COMPLETE\n");
    MPI_Recv(&msg, 1, MPI_INT, 0, 1, newcomp, MPI_STATUS_IGNORE);
    MPI_Comm_disconnect(&newcomp);
    fprintf(stderr, "MESSAGE RECVD: %d\n", msg);

    ompi_dpm.pclose(port);

 cleanup:
    MPI_Finalize();
    return 0;
}
