/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <stdio.h>
#include <errno.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/constants.h"
#include "util/few.h"

int ompi_few(char *argv[], int *status)
{
#ifndef WIN32
    pid_t pid, ret;

    if ((pid = fork()) < 0) {
      return OMPI_ERR_IN_ERRNO;
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

          return OMPI_ERR_IN_ERRNO;
        }
      } while (true);
    }

    /* Return the status to the caller */

    return OMPI_SUCCESS;
#else

    /* Welcome to windows land. This is apparently a simple fork() exec() 
       procedure and hence should not be too difficult to implement */
       HANDLE new_process;
       STARTUPINFO si;
       PROCESS_INFORMATION pi;
       DWORD process_stat;

       /* it does seem as if the environment is getting propogated, so I 
          will follow the same procedure in my code */

          /* ANJU: Have to implement signal handling */

       ZeroMemory (&si, sizeof(si));
       ZeroMemory (&pi, sizeof(pi));
       
       GetStartupInfo (&si);
       if (!CreateProcess (argv[0], 
                           argv, 
                           NULL,
                           NULL,
                           TRUE,
                           0,  
                           NULL,
                           NULL,
                           &si,
                           &pi)){
         /* actual error can be got by simply calling GetLastError() */
         return OMPI_ERROR; 
       }

       /* wait for child to die */
       WaitForSingleObject(pi.hProcess, INFINITE);
       GetExitCodeProcess(pi.hProcess, &process_stat);

       SetLastError(process_stat);
       return OMPI_SUCCESS;
#endif
}
