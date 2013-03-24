#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg, rc, i;
    MPI_Comm parent, child;
    int rank, size;
    char hostname[512];
    pid_t pid;
    MPI_Info info;
    char *keyval, *tmp;

    pid = getpid();
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Comm_get_parent(&parent);
    /* If we get COMM_NULL back, then we're the parent */
    if (MPI_COMM_NULL == parent) {
        if (argc < 2) {
            fprintf(stderr, "Usage: info_spawn key:value key:value...\n");
            exit(1);
        }

        pid = getpid();
        printf("Parent [pid %ld] about to spawn!\n", (long)pid);
        MPI_Info_create(&info);
        for (i=1; i < argc; i++) {
            tmp = strdup(argv[i]);
            keyval = strchr(tmp, ':');
            if (NULL == keyval) {
                fprintf(stderr, "Missing colon separator in key-value\n");
                exit(1);
            }
            *keyval = '\0';
            keyval++;
            fprintf(stderr, "SETTING %s TO %s\n", tmp, keyval);
            MPI_Info_set(info, tmp, keyval);
            free(tmp);
        }
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
    } 
    /* Otherwise, we're the child */
    else {
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &size);
        gethostname(hostname, 512);
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
