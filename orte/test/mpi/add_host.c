#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/param.h>

#include "opal/runtime/opal.h"

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg, rc;
    MPI_Comm parent, child;
    int rank, size;
    const char *hostname;
    pid_t pid;
    char *env_rank,*env_nspace;
    MPI_Info info;

    env_rank = getenv("PMIX_RANK");
    env_nspace = getenv("PMIX_NAMESPACE");
    pid = getpid();
    hostname = opal_gethostname();

    printf("[%s:%s pid %ld] starting up on node %s!\n", env_nspace, env_rank, (long)pid, hostname);

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf("%d completed MPI_Init\n", rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_get_parent(&parent);
    /* If we get COMM_NULL back, then we're the parent */
    if (MPI_COMM_NULL == parent) {
        pid = getpid();
        printf("Parent [pid %ld] about to spawn!\n", (long)pid);
        MPI_Info_create(&info);
        MPI_Info_set(info, "add-host", "rhc002:24");
        if (MPI_SUCCESS != (rc = MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, 3, info,
                                                0, MPI_COMM_WORLD, &child, MPI_ERRCODES_IGNORE))) {
            printf("Child failed to spawn\n");
            return rc;
        }
        printf("Parent done with spawn\n");
        if (0 == rank) {
            msg = 38;
            printf("Parent sending message to child\n");
            MPI_Send(&msg, 1, MPI_INT, 0, 1, child);
        }
        MPI_Comm_disconnect(&child);
        printf("Parent disconnected\n");
        /* do it again */
        MPI_Info_set(info, "add-host", "rhc003:24");
        if (MPI_SUCCESS != (rc = MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, 3, info,
                                                0, MPI_COMM_WORLD, &child, MPI_ERRCODES_IGNORE))) {
            printf("Child failed to spawn\n");
            return rc;
        }
        printf("Parent done with second spawn\n");
        if (0 == rank) {
            msg = 38;
            printf("Parent sending message to second children\n");
            MPI_Send(&msg, 1, MPI_INT, 0, 1, child);
        }
        MPI_Comm_disconnect(&child);
        printf("Parent disconnected again\n");
    }
    /* Otherwise, we're the child */
    else {
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &size);
        pid = getpid();
        printf("Hello from the child %d of %d on host %s pid %ld\n", rank, 3, hostname, (long)pid);
        if (0 == rank) {
            MPI_Recv(&msg, 1, MPI_INT, 0, 1, parent, MPI_STATUS_IGNORE);
            printf("Child %d received msg: %d\n", rank, msg);
        }
        MPI_Comm_disconnect(&parent);
        printf("Child %d disconnected\n", rank);
    }

    MPI_Finalize();
    fprintf(stderr, "%d: exiting\n", pid);
    return 0;
}
