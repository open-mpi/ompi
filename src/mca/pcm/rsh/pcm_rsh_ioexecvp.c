/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include <stdlib.h>
#include <ctype.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <sys/wait.h>
#include <sys/param.h>

#include "include/constants.h"
#include "mca/pcm/rsh/pcm_rsh.h"
#include "util/argv.h"

/*
 *	ioexecvp
 *
 *	Function:	- execute command
 *			- can direct command stdout to buffer and/or stdout
 *			- stderr is checked and passed through
 *	Accepts		- command argv
 *			- print stdout flag
 *			- ptr to buffer (for stdout data)
 *			- size of buffer
 *	Returns		- 0 or OMPI_ERROR
 */
int
mca_pcm_rsh_ioexecvp(char **cmdv, int showout, char *outbuff, 
                           int outbuffsize, int stderr_is_err)
{
  int kidstdout[2];		/* child stdout pipe */
  int kidstderr[2];		/* child stderr pipe */
  int ret;			/* read() return value */
  int err;			/* error indicator */
  int status;			/* exit status */
  int pid;			/* child process id */
  char *ptr = 0;		/* buffer pointer */
  fd_set readset;		/* fd's for read select */
  fd_set errset;		/* fd's for error select */
  int nfds = 1;			/* num fd's in readset */
  char temp[256];		/* string holding space */
  int want_out = 0;		/* want stdout in select */
  int stdout_err = 0;
  int stderr_err = 0;
  int i;
  int announce = 0;
  char *stderr_announce;

  if (stderr_is_err == 1) {
    stderr_announce = "ERROR: LAM/MPI unexpectedly received the following on stderr:\n";
  } else {
    stderr_announce = "WARNING: LAM/MPI unexpectedly received the following on stderr:\n";
  }

  /* Create child stdout/stderr pipes and fork the child process
     (command).  */

  if (pipe(kidstdout) || pipe(kidstderr))
    return (OMPI_ERROR);

  if ((pid = fork()) < 0) {
    return (OMPI_ERROR);
  }

  else if (pid == 0) {		       /* child */
    if ((dup2(kidstderr[1], 2) < 0) || (dup2(kidstdout[1], 1) < 0)) {
      perror(cmdv[0]);
      exit(errno);
    }

    if (close(kidstdout[0]) || close(kidstderr[0]) ||
	close(kidstdout[1]) || close(kidstderr[1])) {
      perror(cmdv[0]);
      exit(errno);
    }

    /* Ensure that we close all other file descriptors */

    for (i = 3; i < FD_SETSIZE; i++)
      close(i);

    execvp(cmdv[0], cmdv);
    exit(errno);
  }

  if (close(kidstdout[1]) || close(kidstderr[1]))
    return (OMPI_ERROR);

  /* We must be able to monitor both stdout and stderr; it is possible
     that we may be trying to capture the stdout but also need to
     monitor output on stderr (e.g., recon, lamboot).  So make a
     FD_SET with potentially both of the file descriptors and do a
     select on it.  */

  FD_ZERO(&readset);
  FD_SET(kidstderr[0], &readset);
  nfds = kidstderr[0] + 1;
  if (showout || (outbuff != 0)) {
    ptr = outbuff;
    FD_SET(kidstdout[0], &readset);
    nfds = (nfds > kidstdout[0] + 1) ? nfds : kidstdout[0] + 1;
    want_out = 1;
  }

  err = 0;
  while (err == 0 && nfds > 0) {

    /* Check to see if select() gets interrupted.  */

    errset = readset;
    ret = select(nfds, &readset, NULL, &errset, NULL);
    if (ret == -1) {
      if (errno == EINTR)
	continue;
      else {

	/* Need to simply break on error instead of returning so that
	   we can still reap the child properly */

	err = OMPI_ERROR;
	break;
      }
    }

    /* Check for error condition on stderr.  Don't need to close it
      here -- it will get closed unconditionally later.  */

    if (FD_ISSET(kidstderr[0], &errset) != 0) {
      stderr_err = 1;
    }

    /* See if there was something on stderr */

    if (FD_ISSET(kidstderr[0], &readset) != 0) {
      while (1) {
	ret = read(kidstderr[0], temp, 256);
	/* Error? */
	if (ret < 0) {
	  if (errno == EINTR)
	    continue;
	  else {
	    stderr_err = 1;
	    err = OMPI_ERROR;
	    break;
	  }
	}
	/* Good bytes */
	else if (ret > 0) {
	  if (announce == 0)
	    write(2, stderr_announce, strlen(stderr_announce));
	  announce = 1;
	  write(2, temp, ret);
	  fflush(stderr);

          if (stderr_is_err == 1) {
            errno = EFAULT;
            err = OMPI_ERROR;
          }
	}
	/* Zero bytes */
	else {
	  /* This is likely to indicate that this pipe has closed */
	  stderr_err = 1;
	  break;
	}
      }
    }

    /* Check for error condition on stdout.  Don't need to close it
       here -- it will get closed unconditionally later.  */

    if (FD_ISSET(kidstdout[0], &errset) != 0)
      stdout_err = 1;

    /* See if there is something on stdout (and if we care) */

    if ((showout || (outbuff != 0)) &&
	FD_ISSET(kidstdout[0], &readset) != 0) {
      while (1) {
	ret = read(kidstdout[0], temp, 256);
	/* Error? */
	if (ret < 0) {
	  if (errno == EINTR)
	    continue;
	  else {
	    stdout_err = 1;
	    err = OMPI_ERROR;
	    break;
	  }
	}
	/* Good bytes */
	else if (ret > 0) {
	  if (outbuffsize > 0) {
	    memcpy(ptr, temp, (ret > outbuffsize) ? outbuffsize : ret);
	    /* Doesn't matter if we overshoot here */
	    outbuffsize -= ret;
	    ptr += ret;
	    if (outbuffsize > 0)
	      *ptr = '\0';
	  }
	  if (showout) {
	    write(1, temp, ret);
	    fflush(stdout);
	  }
	}
	/* Zero bytes */
	else {
	  stdout_err = 1;
	  break;
	}
      }
    }

    /* Reset stderr, 'cause we're always interested in that, unless it
       errored out */

    nfds = 0;
    if (!stderr_err) {
      FD_SET(kidstderr[0], &readset);
      nfds = kidstderr[0] + 1;
    }

    /* See if we want to reset stdout */

    if (!stdout_err && (want_out || outbuffsize > 0)) {
      FD_SET(kidstdout[0], &readset);
      nfds = (nfds > kidstdout[0] + 1) ? nfds : kidstdout[0] + 1;
    }
  }

  /* Close the pipes of the parent process.  */

  if (close(kidstdout[0]) || close(kidstderr[0])) {
    err = OMPI_ERROR;
  }

  /* Wait for the command to exit.  */

  do {
    if (waitpid(pid, &status, 0) < 0) {
      return (OMPI_ERROR);
    }
  } while (!WIFEXITED(status));

  if (WEXITSTATUS(status)) {
    errno = WEXITSTATUS(status);

    return (OMPI_ERROR);
  }

  return (err);
}

