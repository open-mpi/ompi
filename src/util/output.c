/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdlib.h>
#include <syslog.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "include/constants.h"
#include "util/output.h"
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
static void free_descriptor(int output_id);
static void output(int output_id, char *format, va_list arglist);
static char *ompi_vsnprintf(char *format, va_list arglist);


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
  char *ldi_syslog_ident;

  char *ldi_prefix;
  int ldi_prefix_len;

  bool ldi_stdout;
  bool ldi_stderr;

  int ldi_fd;
  char *ldi_file_suffix;
};
typedef struct output_desc_t output_desc_t;

#define OMPI_OUTPUT_MAX_STREAMS 32


/*
 * Local state
 */
static bool initialized = false;
static output_desc_t info[OMPI_OUTPUT_MAX_STREAMS];
static char *temp_str = 0;
static int temp_str_len = 0;
static ompi_mutex_t mutex;


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
    info[i].ldi_fd = -1;
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
    lds.lds_want_file = (-1 == info[i].ldi_fd) ? false: true;
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
  if (i >= OMPI_OUTPUT_MAX_STREAMS) {
    closelog();
  }

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
#if __STDC__
  va_start(arglist, format);
#else
  va_start(arglist);
#endif
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
#if __STDC__
    va_start(arglist, format);
#else
    va_start(arglist);
#endif
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
  int flags;
  char *filename;

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
  OMPI_THREAD_UNLOCK(&mutex);
  info[i].ldi_enabled = lds->lds_is_debugging ? 
    (bool) OMPI_ENABLE_DEBUG : true;
  info[i].ldi_verbose_level = 0;

  info[i].ldi_syslog = lds->lds_want_syslog;
  if (lds->lds_want_syslog) {
    if (NULL != lds->lds_syslog_ident) {
      info[i].ldi_syslog_ident = strdup(lds->lds_syslog_ident);
      openlog(lds->lds_syslog_ident, LOG_PID, LOG_USER);
    } else {
      info[i].ldi_syslog_ident = NULL;
      openlog("ompi", LOG_PID, LOG_USER);
    }
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
  if (lds->lds_want_file) {

    /* Setup the filename and open flags */

#if 0
    filename = ompi_get_tmpdir();
#else
    ompi_output(0, "WARNING: need to implement session dir (%s, %d)\n",
               __FILE__, __LINE__);
    filename = malloc(256);
    strcpy(filename, "/tmp");
#endif
    strcat(filename, "/ompi-");
    if (lds->lds_file_suffix != NULL) {
      info[i].ldi_file_suffix = strdup(lds->lds_file_suffix);
      strcat(filename, lds->lds_file_suffix);
    } else {
      info[i].ldi_file_suffix = NULL;
      strcat(filename, "output.txt");
    }
    flags = O_CREAT | O_RDWR;
    if (!lds->lds_want_file_append) {
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

    fcntl(info[i].ldi_fd, F_SETFD, 1);
    free(filename);
  }

  return i;
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

    if (NULL != ldi->ldi_syslog_ident) {
      free(ldi->ldi_syslog_ident);
    }
    ldi->ldi_syslog_ident = NULL;
  }  
}


/*
 * Do the actual output.  Take a va_list so that we can be called from
 * multiple different places, even functions that took "..." as input
 * arguments.
 */
static void output(int output_id, char *format, va_list arglist)
{
  int len, total_len;
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
    str = ompi_vsnprintf(format, arglist);
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
      temp_str = malloc(total_len * 2);
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
      syslog(ldi->ldi_syslog_priority, str);
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

    /* File output */

    if (ldi->ldi_fd != -1) {
      write(ldi->ldi_fd, temp_str, total_len);
    }
    OMPI_THREAD_UNLOCK(&mutex);

    free(str);
  }  
}


/* 
 * ompi_vsnprintf
 *
 * Make a good guess about how long a printf-style varags formatted
 * string will be once all the % escapes are filled in.  We don't
 * handle every % escape here, but we handle enough, and then add a
 * fudge factor in at the end.  When we have that, alloc out a buffer
 * and snprintf into it.  The whole reason for this routine is because
 * we can't count on vsnprintf to be on every system.  :-( Ok, it's
 * not exactly the same as vsnprintf, but it's in the same spirit, so
 * it's ok to use a derrivative of the name...  
 */
static char *ompi_vsnprintf(char *format, va_list arglist)
{
  int i, len;
  char *sarg;
  int iarg;
  long larg;
  double darg;
  float farg;

  /* Important: keep a copy of the original arglist because when we
     traverse through the original arglist, it has internal state that
     knows where it is in the list of args.  At the end of this
     routine, we'll need the whole list in order to call vsprintf(),
     and there doesn't appear to be a way to "rewind" a va_alist. 

     Copy order taken from Autoconf docs
  */
  va_list arglist2;
#if OMPI_HAVE_VA_COPY
  va_copy(arglist2, arglist);
#elif OMPI_HAVE_UNDERSCORE_VA_COPY
  __va_copy(arglist2, arglist);
#else
  memcpy (&arglist2, &arglist, sizeof(va_list));
#endif

  /* Start off with a fudge factor of 128 to handle the % escapes that
     we aren't calculating here */

  len = strlen(format) + 128;
  for (i = 0; i < strlen(format); ++i) {
    if ('%' == format[i] && i + 1 < strlen(format) && '%' != format[i + 1]) {
      ++i;
      switch(format[i]) {
      case 's':
	sarg = va_arg(arglist, char*);
	/* If there's an arg, get the strlen, otherwise we'll use (null) */
	if (NULL != sarg)
	  len += strlen(sarg);
	else
	  len += 5;
	break;
	
      case 'd':
      case 'i':
	iarg = va_arg(arglist, int);
	/* Alloc for minus sign */
	if (iarg < 0)
	  ++len;
	/* Now get the log10 */
	do {
	  ++len;
	  iarg /= 10;
	} while (0 != iarg);
	break;
	
      case 'x':
      case 'X':
	iarg = va_arg(arglist, int);
	/* Now get the log16 */
	do {
	  ++len;
	  iarg /= 16;
	} while (0 != iarg);
	break;
	
      case 'f':
	farg = va_arg(arglist, int);
	/* Alloc for minus sign */
	if (farg < 0) {
	  ++len;
	  farg = -farg;
	}
	/* Alloc for 3 decimal places + '.' */
	len += 4;
	/* Now get the log10 */
	do {
	  ++len;
	  farg /= 10.0;
	} while (0 != farg);
	break;
	
      case 'g':
	darg = va_arg(arglist, int);
	/* Alloc for minus sign */
	if (darg < 0) {
	  ++len;
	  darg = -darg;
	}
	/* Alloc for 3 decimal places + '.' */
	len += 4;
	/* Now get the log10 */
	do {
	    ++len;
	    darg /= 10.0;
	} while (0 != darg);
	break;
	
      case 'l':
	/* Get %ld %lx %lX %lf */
	if (i + 1 < strlen(format)) {
	  ++i;
	  switch(format[i]) {
	  case 'x':
	  case 'X':
	    larg = va_arg(arglist, int);
	    /* Now get the log16 */
	    do {
	      ++len;
	      larg /= 16;
	    } while (0 != larg);
	    break;
	    
	  case 'f':
	    darg = va_arg(arglist, int);
	    /* Alloc for minus sign */
	    if (darg < 0) {
	      ++len;
	      darg = -darg;
	    }
	    /* Alloc for 3 decimal places + '.' */
	    len += 4;
	    /* Now get the log10 */
	    do {
	      ++len;
	      darg /= 10.0;
	    } while (0 != darg);
	    break;
	    
	  case 'd':
	  default:
	    larg = va_arg(arglist, int);
	    /* Now get the log10 */
	    do {
	      ++len;
	      larg /= 10;
	    } while (0 != larg);
	    break;
	  }
	}
	
      default:
	break;
      }
    }
  }

  /* Wasn't that simple?  Now malloc out a string and do the final
     formatting into that string. */
  
  sarg = malloc(len);
  vsprintf(sarg, format, arglist2);

  /* Return the new string */

  return sarg;
}
