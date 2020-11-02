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
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "pmix_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/few.h"
#include "src/util/basename.h"
#include "src/util/argv.h"
#include "include/pmix_common.h"

int pmix_few(char *argv[], int *status)
{
#if defined(HAVE_FORK) && defined(HAVE_EXECVE) && defined(HAVE_WAITPID)
    pid_t pid, ret;

    if ((pid = fork()) < 0) {
      return PMIX_ERROR;
    }

    /* Child execs.  If it fails to exec, exit. */

    else if (0 == pid) {
      execvp(argv[0], argv);
      exit(errno);
    }

    /* Parent loops waiting for the child to die. */

    else {
      do {
        /* If the child exited, return */

        if (pid == (ret = waitpid(pid, status, 0))) {
          break;
        }

        /* If waitpid was interrupted, loop around again */

        else if (ret < 0) {
          if (EINTR == errno) {
            continue;
          }

          /* Otherwise, some bad juju happened -- need to quit */

          return PMIX_ERROR;
        }
      } while (true);
    }

    /* Return the status to the caller */

    return PMIX_SUCCESS;
#else
    return PMIX_ERR_NOT_SUPPORTED;
#endif
}
