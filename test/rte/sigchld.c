/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "runtime/ompi_rte_wait.h"
#include "runtime/ompi_progress.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>

int count = 0;

static void
callback(pid_t pid, int status, void *data)
{
    printf("callback for %d, %d\n", pid, status);
    count--;
}


int
main(int argc, char *argv[])
{
    pid_t pid, ret;
    int status = -1;

    MPI_Init(&argc, &argv);
    ompi_rte_wait_init();

    pid = fork();
    if (pid > 0) {
        count++;
        ompi_rte_wait_cb(pid, callback, NULL);
    } else {
        sleep(10);
        printf("pid %d exiting\n", getpid());
        exit(0);
    }

    pid = fork();
    if (pid > 0) {
        ret = ompi_rte_waitpid(pid, &status, 0);
        printf("pid %d waitpid, status %d\n", ret, status);
    } else {
        sleep(5);
        printf("pid %d exiting\n", getpid());
        exit(0);
    }

    pid = fork();
    if (pid > 0) {
        count++;
        ompi_rte_wait_cb(pid, callback, NULL);
    } else {
        printf("pid %d exiting\n", getpid());
        exit(0);
    }
    
    while (count > 0) { ompi_progress(); }

    ompi_rte_wait_finalize();
    MPI_Finalize();


    return 0;
}
