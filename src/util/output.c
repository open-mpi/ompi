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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif
#include <string.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "include/constants.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "threads/mutex.h"

/*
 * Private data
 */
static int verbose_stream = -1;
static ompi_output_stream_t verbose = {
  /* debugging */
  false,
  /* verbose level */
  0,
  /* syslog */
  false, 0, NULL, NULL, 
  /* stdout */
  false,
  /* stderr */
  true,
  /* file */
  false, false, NULL
};


/*
 * Private functions
 */
static int do_open(int output_id, ompi_output_stream_t *lds);
static int open_file(int i);
static void free_descriptor(int output_id);
static void output(int output_id, char *format, va_list arglist);


/*
 * Internal data structures and helpers for the generalized output
 * stream mechanism.
 */
struct output_desc_t {
  bool ldi_used;
  bool ldi_enabled;
  int ldi_verbose_level;

  bool ldi_syslog;
  int ldi_syslog_priority;
#ifndef WIN32
  char *ldi_syslog_ident;
#else 
  HANDLE ldi_syslog_ident;
#endif
  char *ldi_prefix;
  int ldi_prefix_len;

  bool ldi_stdout;
  bool ldi_stderr;

  bool ldi_file;
  bool ldi_file_want_append;
  char *ldi_file_suffix;
  int ldi_fd;
  int ldi_file_num_lines_lost;
};
typedef struct output_desc_t output_desc_t;

#define OMPI_OUTPUT_MAX_STREAMS 32


/*
 * Local state
 */
static bool initialized = false;
static output_desc_t info[OMPI_OUTPUT_MAX_STREAMS];
static char *temp_str = 0;
static size_t temp_str_len = 0;
static ompi_mutex_t mutex;
static bool syslog_opened = false;


/*
 * Setup the output stream infrastructure
 */
bool ompi_output_init(void)
{
  int i;
  for (i = 0; i < OMPI_OUTPUT_MAX_STREAMS; ++i) {
    info[i].ldi_used = false;
    info[i].ldi_enabled = false;

    info[i].ldi_syslog = false;
    info[i].ldi_file = false;
    info[i].ldi_file_suffix = NULL;
    info[i].ldi_file_want_append = false;
    info[i].ldi_fd = -1;
    info[i].ldi_file_num_lines_lost = 0;
  }

  /* Initialize the mutex that protects the output */

  OBJ_CONSTRUCT(&mutex, ompi_mutex_t);
  initialized = true;

  /* Open the default verbose stream */

  verbose_stream = ompi_output_open(&verbose);
  return true;
}


/*
 * Open a stream
 */
int ompi_output_open(ompi_output_stream_t *lds)
{
  return do_open(-1, lds);
}


/*
 * Reset the parameters on a stream
 */
int ompi_output_reopen(int output_id, ompi_output_stream_t *lds)
{
  return do_open(output_id, lds);
}


/*
 * Enable and disable outptu streams
 */
bool ompi_output_switch(int output_id, bool enable)
{
  bool ret = false;

  /* Setup */

  if (!initialized)
    ompi_output_init();

  if (output_id >= 0 && output_id < OMPI_OUTPUT_MAX_STREAMS) {
    ret = info[output_id].ldi_enabled;
    info[output_id].ldi_enabled = enable;
  }

  return ret;
}


/*
 * Reopen all the streams; used during checkpoint/restart.
 */
void ompi_output_reopen_all(void)
{
  int i;
  ompi_output_stream_t lds;

  for (i = 0; i < OMPI_OUTPUT_MAX_STREAMS; ++i) {

    /* scan till we find ldi_used == 0, which is the end-marker */ 

    if (!info[i].ldi_used) {
      break;
    }

    /* 
     * set this to zero to ensure that ompi_output_open will return this same
     * index as the output stream id 
     */
    info[i].ldi_used = false;

    lds.lds_want_syslog = info[i].ldi_syslog;
    lds.lds_syslog_priority = info[i].ldi_syslog_priority;
    lds.lds_syslog_ident = info[i].ldi_syslog_ident;
    lds.lds_prefix = info[i].ldi_prefix;
    lds.lds_want_stdout = info[i].ldi_stdout;
    lds.lds_want_stderr = info[i].ldi_stderr;
    lds.lds_want_file = (-1 == info[i].ldi_fd) ? false : true;
    /* open all streams in append mode */
    lds.lds_want_file_append = true;
    lds.lds_file_suffix = info[i].ldi_file_suffix;

    /* 
     * call ompi_output_open to open the stream. The return value is
     * guaranteed to be i.  So we can ignore it. 
     */
    ompi_output_open(&lds);
  }
}


/*
 * Close a stream
 */
void ompi_output_close(int output_id)
{
  int i;

  /* Setup */

  if (!initialized) {
    ompi_output_init();
  }

  /* If it's valid, used, enabled, and has an open file descriptor,
     free the resources associated with the descriptor */

   if (output_id >= 0 && output_id < OMPI_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {
     free_descriptor(output_id);
   }

  /* If no one has the syslog open, we should close it */

  OMPI_THREAD_LOCK(&mutex);
  for (i = 0; i < OMPI_OUTPUT_MAX_STREAMS; ++i) {
    if (info[i].ldi_used && info[i].ldi_syslog) {
      break;
    }
  }
#ifndef WIN32
  if (i >= OMPI_OUTPUT_MAX_STREAMS && syslog_opened) {
    closelog();
  }
#else 
    DeregisterEventSource(info[output_id].ldi_syslog_ident);
#endif

  /* Somewhat of a hack to free up the temp_str */

  if (NULL != temp_str) {
    free(temp_str);
    temp_str = NULL;
    temp_str_len = 0;
  }
  OMPI_THREAD_UNLOCK(&mutex);
}


/*
 * Main function to send output to a stream
 */
void ompi_output(int output_id, char *format, ...)
{
  va_list arglist;
  va_start(arglist, format);
  output(output_id, format, arglist);
  va_end(arglist);
}


/*
 * Send a message to a stream if the verbose level is high enough
 */
void ompi_output_verbose(int level, int output_id, char *format, ...)
{
  if (info[output_id].ldi_verbose_level >= level) {
    va_list arglist;
    va_start(arglist, format);
    output(output_id, format, arglist);
    va_end(arglist);
  }
}


/*
 * Set the verbosity level of a stream
 */
void ompi_output_set_verbosity(int output_id, int level)
{
  info[output_id].ldi_verbose_level = level;
}


/*
 * Shut down the output stream system
 */
void ompi_output_finalize(void)
{
  if (initialized) {
    if (verbose_stream != -1) {
      ompi_output_close(verbose_stream);
    }
    verbose_stream = -1;
  }
  OBJ_DESTRUCT(&mutex);
}

/************************************************************************/

/*
 * Back-end of open() and reopen().  Necessary to have it as a
 * back-end function so that we can do the thread locking properly
 * (especially upon reopen).
 */
static int do_open(int output_id, ompi_output_stream_t *lds)
{
  int i;

  /* Setup */

  if (!initialized) {
    ompi_output_init();
  }

  /* If output_id == -1, find an available stream, or return
     OMPI_ERROR */

  if (-1 == output_id) {
    OMPI_THREAD_LOCK(&mutex);
    for (i = 0; i < OMPI_OUTPUT_MAX_STREAMS; ++i) {
      if (!info[i].ldi_used) {
        break;
      }
    }
    if (i >= OMPI_OUTPUT_MAX_STREAMS) {
      OMPI_THREAD_UNLOCK(&mutex);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
  } 

  /* Otherwise, we're reopening, so we need to free all previous
     resources, close files, etc. */

  else {
    free_descriptor(output_id);
    i = output_id;
  }

  /* Special case: if we got NULL for lds, then just use the default
     verbose */

  if (NULL == lds) {
    lds = &verbose;
  }

  /* Got a stream -- now initialize it and open relevant outputs */

  info[i].ldi_used = true;
  if (-1 == output_id) {
      OMPI_THREAD_UNLOCK(&mutex);
  }
  info[i].ldi_enabled = lds->lds_is_debugging ? 
    (bool) OMPI_ENABLE_DEBUG : true;
  info[i].ldi_verbose_level = lds->lds_verbose_level;

  info[i].ldi_syslog = lds->lds_want_syslog;
  if (lds->lds_want_syslog) {
#ifndef WIN32
    if (NULL != lds->lds_syslog_ident) {
      info[i].ldi_syslog_ident = strdup(lds->lds_syslog_ident);
	  openlog(lds->lds_syslog_ident, LOG_PID, LOG_USER);
    } else {
      info[i].ldi_syslog_ident = NULL;
      openlog("ompi", LOG_PID, LOG_USER);
    }
#else
    if (NULL == (info[i].ldi_syslog_ident = 
                 RegisterEventSource(NULL, TEXT("Open MPI: ")))) {
        /* handle the error */
        return OMPI_ERROR;
    }
#endif
    syslog_opened = true;
    info[i].ldi_syslog_priority = lds->lds_syslog_priority;
  }

  if (NULL != lds->lds_prefix) {
    info[i].ldi_prefix = strdup(lds->lds_prefix);
    info[i].ldi_prefix_len = strlen(lds->lds_prefix);
  } else {
    info[i].ldi_prefix = NULL;
    info[i].ldi_prefix_len = 0;
  }

  info[i].ldi_stdout = lds->lds_want_stdout;
  info[i].ldi_stderr = lds->lds_want_stderr;

  info[i].ldi_fd = -1;
  info[i].ldi_file = lds->lds_want_file;
  info[i].ldi_file_suffix = (NULL == lds->lds_file_suffix) ? NULL :
      strdup(lds->lds_file_suffix);
  info[i].ldi_file_want_append = lds->lds_want_file_append;
  info[i].ldi_file_num_lines_lost = 0;
  
  /* Don't open a file in the session directory now -- do that lazily
     so that if there's no output, we don't have an empty file */

  return i;
}


static int open_file(int i)
{
    int flags;
    char *dir, *filename;

    /* Setup the filename and open flags */

    dir = orte_process_info.proc_session_dir;
    if (NULL != dir) {
        filename = (char*) malloc(MAXPATHLEN);
        if (NULL == filename) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        strcpy(filename, dir);
        strcat(filename, "/output-");
        if (info[i].ldi_file_suffix != NULL) {
            strcat(filename, info[i].ldi_file_suffix);
        } else {
            info[i].ldi_file_suffix = NULL;
            strcat(filename, "output.txt");
        }
        flags = O_CREAT | O_RDWR;
        if (!info[i].ldi_file_want_append) {
            flags |= O_TRUNC;
        }

        /* Actually open the file */

        info[i].ldi_fd = open(filename, flags, 0644);
        if (-1 == info[i].ldi_fd) {
            info[i].ldi_used = false;
            return OMPI_ERR_IN_ERRNO;
        }

        /* Make the file be close-on-exec to prevent child inheritance
           problems */
#ifndef WIN32
		/* TODO: Need to find out the equivalent in windows */
        fcntl(info[i].ldi_fd, F_SETFD, 1);
#endif
        free(filename);
    }

    /* Return successfully even if the session dir did not exist yet;
       we'll try opening it later */

    return OMPI_SUCCESS;
}


/*
 * Free all the resources associated with a descriptor.
 */
static void free_descriptor(int output_id)
{
  output_desc_t *ldi;

  if (output_id >= 0 && output_id < OMPI_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {
    ldi = &info[output_id];

    if (-1 != ldi->ldi_fd) {
      close(ldi->ldi_fd);
    }
    ldi->ldi_used = false;

    /* If we strduped a prefix, suffix, or syslog ident, free it */

    if (NULL != ldi->ldi_prefix) {
      free(ldi->ldi_prefix);
    }
    ldi->ldi_prefix = NULL;

    if (NULL != ldi->ldi_file_suffix) {
      free(ldi->ldi_file_suffix);
    }
    ldi->ldi_file_suffix = NULL;

#ifndef WIN32
    if (NULL != ldi->ldi_syslog_ident) {
      free(ldi->ldi_syslog_ident);
    }
    ldi->ldi_syslog_ident = NULL;
#endif
  }  
}


/*
 * Do the actual output.  Take a va_list so that we can be called from
 * multiple different places, even functions that took "..." as input
 * arguments.
 */
static void output(int output_id, char *format, va_list arglist)
{
  size_t len, total_len;
  bool want_newline = false;
  char *str;
  output_desc_t *ldi;

  /* Setup */

  if (!initialized) {
    ompi_output_init();
  }

  /* If it's valid, used, and enabled, output */

  if (output_id >= 0 && output_id < OMPI_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {
    ldi = &info[output_id];
    
    /* Make the formatted string */

    OMPI_THREAD_LOCK(&mutex);
    vasprintf(&str, format, arglist);
    total_len = len = strlen(str);
    if ('\n' != str[len - 1]) {
      want_newline = true;
      ++total_len;
    }
    if (NULL != ldi->ldi_prefix) {
      total_len += strlen(ldi->ldi_prefix);
    }
    if (temp_str_len < total_len + want_newline) {
      if (NULL != temp_str) {
	free(temp_str);
      }
      temp_str = (char*) malloc(total_len * 2);
      temp_str_len = total_len * 2;
    }
    if (NULL != ldi->ldi_prefix) {
      if (want_newline) {
	snprintf(temp_str, temp_str_len, "%s%s\n", ldi->ldi_prefix, str);
      } else {
	snprintf(temp_str, temp_str_len, "%s%s", ldi->ldi_prefix, str);
      }
    } else {
      if (want_newline) {
	snprintf(temp_str, temp_str_len, "%s\n", str);
      } else {
	snprintf(temp_str, temp_str_len, "%s", str);
      }
    }

    /* Syslog output */

    if (ldi->ldi_syslog) {
#ifndef WIN32
      syslog(ldi->ldi_syslog_priority, str);
#endif
    }

    /* stdout output */

    if (ldi->ldi_stdout) {
      printf(temp_str);
      fflush(stdout);
    }

    /* stderr output */

    if (ldi->ldi_stderr) {
      fprintf(stderr, temp_str);
      fflush(stderr);
    }

    /* File output -- first check to see if the file opening was
       delayed.  If so, try to open it.  If we failed to open it, then
       just discard (there are big warnings in the ompi_output.h docs
       about this!). */

    if (ldi->ldi_file) {
      if (ldi->ldi_fd == -1) {
          if (OMPI_SUCCESS != open_file(output_id)) {
              ++ldi->ldi_file_num_lines_lost;
          } else if (ldi->ldi_file_num_lines_lost > 0) {
              char buffer[BUFSIZ];
              memset(buffer, 0, BUFSIZ);
              snprintf(buffer, BUFSIZ - 1, "[WARNING: %d lines lost because the Open MPI process session directory did\n not exist when ompi_output() was invoked]\n", ldi->ldi_file_num_lines_lost);
              write(ldi->ldi_fd, buffer, strlen(buffer));
              ldi->ldi_file_num_lines_lost = 0;
          }
      }
      if (ldi->ldi_fd != -1) {
        write(ldi->ldi_fd, temp_str, total_len);
      }
    }
    OMPI_THREAD_UNLOCK(&mutex);

    free(str);
  }  
}
