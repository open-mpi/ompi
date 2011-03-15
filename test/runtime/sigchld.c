/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "orte/runtime/orte_wait.h"
#include "opal/runtime/opal_progress.h"
#include "runtime/runtime.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>

int count = 0;

static void callback(pid_t pid, int status, void *data)
{
    printf("callback for %d, %d\n", pid, status);
    count--;
}


int main(int argc, char *argv[])
{
    pid_t pid, ret;
    int status = -1;

    orte_init(true);

    pid = fork();
    if (pid > 0) {
        count++;
        printf("parent launched child #1 PID %d\n", pid);
        orte_wait_cb(pid, callback, NULL);
    } else {
        printf("child pid %d sleeping 10 seconds\n", getpid());
        sleep(10);
        printf("pid %d exiting after sleeping 10 seconds\n", getpid());
        exit(0);
    }

    pid = fork();
    if (pid > 0) {
        printf("parent launched child #2 PID %d\n", pid);
        ret = orte_waitpid(pid, &status, 0);
        printf("pid %d waitpid, status %d\n", ret, status);
    } else {
        printf("child pid %d sleeping 5 seconds\n", getpid());
        sleep(5);
        printf("pid %d exiting after sleeping 5 seconds\n", getpid());
        exit(0);
    }

    pid = fork();
    if (pid > 0) {
        count++;
        printf("parent launched child #3 PID %d\n", pid);
        orte_wait_cb(pid, callback, NULL);
    } else {
        printf("pid %d exiting after not sleeping at all\n", getpid());
        exit(0);
    }
    
    while (count > 0) { 
        opal_progress(); 
    }

    orte_finalize();
    return 0;
}
