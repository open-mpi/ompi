/*
 * $HEADER$
 */

#include "lam_config.h"

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

#include "lam/constants.h"
#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "lam/threads/mutex.h"


/*
 * Private data
 */
static int verbose_stream = -1;
static int verbose_level = 0;
static lam_output_stream_t verbose = {
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
static int do_open(int output_id, lam_output_stream_t *lds);
static void free_descriptor(int output_id);
static void output(int output_id, char *format, va_list arglist);
static char *lam_vsnprintf(char *format, va_list arglist);


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

#define LAM_OUTPUT_MAX_STREAMS 32


/*
 * Local state
 */
static bool initialized = false;
static output_desc_t info[LAM_OUTPUT_MAX_STREAMS];
static char *temp_str = 0;
static int temp_str_len = 0;
static lam_mutex_t mutex;


/**
 * Initializes the output stream system and opens a default
 * "verbose" stream.
 *
 * @retval true Upon success.
 * @retval false Upon failure.
 *
 * This should be the first function invoked in the output subsystem.
 * After this call, the default "verbose" stream is open and can be
 * written to via calls to lam_output_verbose() and
 * lam_output_error().  
 *
 * By definition, the default verbose stream has a handle ID of 0, and
 * has a verbose level of 0.
 */
bool lam_output_init(void)
{
  int i;
  for (i = 0; i < LAM_OUTPUT_MAX_STREAMS; ++i) {
    info[i].ldi_used = false;
    info[i].ldi_enabled = false;

    info[i].ldi_syslog = false;
    info[i].ldi_fd = -1;
  }

  /* Initialize the mutex that protects the output */

  lam_mutex_init(&mutex);
  initialized = true;

  /* Open the default verbose stream */

  verbose_stream = lam_output_open(&verbose);
  return true;
}


/**
 * Opens an output stream.
 *
 * @param lds A pointer to lam_output_stream_t describing what the
 * characteristics of the output stream should be.
 *
 * This function opens an output stream and returns an integer handle.
 * The caller is responsible for maintaining the handle and using it
 * in successive calls to LAM_OUTPUT(), lam_output(),
 * lam_output_switch(), and lam_output_close().
 *
 * It is safe to have multiple threads invoke this function
 * simultaneously; their execution will be serialized in an
 * unspecified manner.
 */
int lam_output_open(lam_output_stream_t *lds)
{
  return do_open(-1, lds);
}


/**
 * Re-opens / redirects an output stream.
 *
 * @param output_id Stream handle to reopen
 * @param lds A pointer to lam_output_stream_t describing what the
 * characteristics of the reopened output stream should be.
 *
 * This function redirects an existing stream into a new [set of]
 * location[s], as specified by the lds parameter.  If the output_is
 * passed is invalid, this call is effectively the same as opening a
 * new stream with a specific stream handle.
 */
int lam_output_reopen(int output_id, lam_output_stream_t *lds)
{
  return do_open(output_id, lds);
}


/**
 * Enables and disables output streams.
 *
 * @param output_id Stream handle to switch
 * @param enable Boolean indicating whether to enable the stream
 * output or not.
 *
 * @returns The previous enable state of the stream (true == enabled,
 * false == disabled).
 *
 * The output of a stream can be temporarily disabled by passing an
 * enable value to false, and later resumed by passing an enable value
 * of true.  This does not close the stream -- it simply tells the
 * lam_output subsystem to intercept and discard any output sent to
 * the stream via LAM_OUTPUT() or lam_output() until the output is
 * re-enabled.
 */
bool lam_output_switch(int output_id, bool enable)
{
  bool ret = false;

  /* Setup */

  if (!initialized)
    lam_output_init();

  if (output_id >= 0 && output_id < LAM_OUTPUT_MAX_STREAMS) {
    ret = info[output_id].ldi_enabled;
    info[output_id].ldi_enabled = enable;
  }

  return ret;
}


/**
 * \internal
 *
 * Reopens all existing output streams.
 *
 * This function should never be called by user applications; it is
 * typically only invoked after a restart (i.e., in a new process)
 * where output streams need to be re-initialized.
 */
void lam_output_reopen_all(void)
{
  int i;
  lam_output_stream_t lds;

  for (i = 0; i < LAM_OUTPUT_MAX_STREAMS; ++i) {

    /* scan till we find ldi_used == 0, which is the end-marker */ 
    if (!info[i].ldi_used)
      break;

    /* 
     * set this to zero to ensure that lam_output_open will return this same
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
     * call lam_output_open to open the stream. The return value is
     * guaranteed to be i.  So we can ignore it. 
     */
    lam_output_open(&lds);
  }
}


/**
 * Close an output stream.
 *
 * @param output_id Handle of the stream to close.
 *
 * Close an output stream.  No output will be sent to the stream after
 * it is closed.  Be aware that output handles tend to be re-used; it
 * is possible that after a stream is closed, if another stream is
 * opened, it will get the same handle value.
 */
void lam_output_close(int output_id)
{
  int i;

  /* Setup */

  if (!initialized)
    lam_output_init();

  /* If it's valid, used, enabled, and has an open file descriptor,
     free the resources associated with the descriptor */

   if (output_id >= 0 && output_id < LAM_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {
     free_descriptor(output_id);
   }

  /* If no one has the syslog open, we should close it */

  THREAD_LOCK(&mutex);
  for (i = 0; i < LAM_OUTPUT_MAX_STREAMS; ++i)
    if (info[i].ldi_used && info[i].ldi_syslog)
      break;
  if (i >= LAM_OUTPUT_MAX_STREAMS)
    closelog();

  /* Somewhat of a hack to free up the temp_str */

  if (NULL != temp_str) {
    LAM_FREE(temp_str);
    temp_str = NULL;
    temp_str_len = 0;
  }
  THREAD_UNLOCK(&mutex);
}


/**
 * Main function to send output to a stream.
 *
 * @param output_id Stream id returned from lam_output_open().
 * @param format printf-style format string.
 * @param varargs printf-style varargs list to fill the string
 * specified by the format parameter.
 *
 * This is the main function to send output to custom streams (note
 * that output to the default "verbose" stream is handled through
 * lam_output_verbose() and lam_output_error()).
 *
 * It is never necessary to send a trailing "\n" in the strings to
 * this function; some streams requires newlines, others do not --
 * this function will append newlines as necessary.
 *
 * Verbosity levels are ignored in this function.
 */
void lam_output(int output_id, char *format, ...)
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


/**
 * Send output to a stream only if the passed verbosity level is high
 * enough.
 *
 * @param output_id Stream id returned from lam_output_open().
 * @param level Target verbosity level.
 * @param format printf-style format string.
 * @param varargs printf-style varargs list to fill the string
 * specified by the format parameter.
 *
 * Output is only sent to the stream if the current verbosity level is
 * greater than or equal to the level parameter.  This mechanism can
 * be used to send "information" kinds of output to user applications,
 * but only when the user has asked for a high enough verbosity level.
 *
 * It is never necessary to send a trailing "\n" in the strings to
 * this function; some streams requires newlines, others do not --
 * this function will append newlines as necessary.
 *
 * This function is really a convenience wrapper around checking the
 * current verbosity level set on the stream, and if the passed level
 * is less than or equal to the stream's verbosity level, this
 * function will effectively invoke lam_output to send the output to
 * the stream.
 *
 * @see lam_output_set_verbosity()
 */
void lam_output_verbose(int output_id, int level, char *format, ...)
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


/**
 * Set the verbosity level for a stream.
 *
 * @param output_id Stream id returned from lam_output_open().
 * @param level New verbosity level
 *
 * This function sets the verbosity level on a given stream.  It will
 * be used for all future invocations of lam_output_verbose().
 */
void lam_output_set_verbosity(int output_id, int level)
{
  info[output_id].ldi_verbose_level = level;
}


/**
 * Shut down the output stream system.
 *
 * Shut down the output stream system, including the default verbose
 * stream.
 */
void lam_output_finalize(void)
{
  if (initialized) {
    if (verbose_stream != -1)
      lam_output_close(verbose_stream);
    verbose_stream = -1;
  }
}


/*
 * Back-end of open() and reopen().  Necessary to have it as a
 * back-end function so that we can do the thread locking properly
 * (especially upon reopen).
 */
static int do_open(int output_id, lam_output_stream_t *lds)
  {
  int i;
  int flags;
  char *filename;

  /* Setup */

  if (!initialized)
    lam_output_init();

  /* If output_id == -1, find an available stream, or return
     LAM_ERROR */

  if (-1 == output_id) {
    THREAD_LOCK(&mutex);
    for (i = 0; i < LAM_OUTPUT_MAX_STREAMS; ++i)
      if (!info[i].ldi_used)
        break;
    if (i >= LAM_OUTPUT_MAX_STREAMS) {
      THREAD_UNLOCK(&mutex);
      return LAM_ERR_OUT_OF_RESOURCE;
    }
  } 

  /* Otherwise, we're reopening, so we need to free all previous
     resources, close files, etc. */

  else {
    free_descriptor(output_id);
    i = output_id;
  }

  /* Got a stream -- now initialize it and open relevant outputs */

  info[i].ldi_used = true;
  THREAD_UNLOCK(&mutex);
  info[i].ldi_enabled = lds->lds_is_debugging ? (bool) LAM_ENABLE_DEBUG : true;
  info[i].ldi_verbose_level = 0;

  info[i].ldi_syslog = lds->lds_want_syslog;
  if (lds->lds_want_syslog) {
    if (NULL != lds->lds_syslog_ident) {
      info[i].ldi_syslog_ident = strdup(lds->lds_syslog_ident);
      openlog(lds->lds_syslog_ident, LOG_PID, LOG_USER);
    } else {
      info[i].ldi_syslog_ident = NULL;
      openlog("lam", LOG_PID, LOG_USER);
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

#if NEED_TO_IMPLEMENT_SESSION_DIRECTORY
    filename = lam_get_tmpdir();
#else
    filename = LAM_MALLOC(256);
    strcpy(filename, "/tmp");
#endif
    strcat(filename, "/lam-");
    if (lds->lds_file_suffix != NULL) {
      info[i].ldi_file_suffix = strdup(lds->lds_file_suffix);
      strcat(filename, lds->lds_file_suffix);
    } else {
      info[i].ldi_file_suffix = NULL;
      strcat(filename, "output.txt");
    }
    flags = O_CREAT | O_RDWR;
    if (!lds->lds_want_file_append)
      flags |= O_TRUNC;

    /* Actually open the file */

    info[i].ldi_fd = open(filename, flags, 0644);
    if (-1 == info[i].ldi_fd) {
      info[i].ldi_used = false;
      return LAM_ERR_IN_ERRNO;
    }

    /* Make the file be close-on-exec to prevent child inheritance
       problems */

    fcntl(info[i].ldi_fd, F_SETFD, 1);
    LAM_FREE(filename);
  }

  return i;
}


/*
 * Free all the resources associated with a descriptor.
 */
static void free_descriptor(int output_id)
{
  output_desc_t *ldi;

  if (output_id >= 0 && output_id < LAM_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {

    ldi = &info[output_id];

    if (-1 != ldi->ldi_fd)
      close(ldi->ldi_fd);
    ldi->ldi_used = false;

    /* If we strduped a prefix, suffix, or syslog ident, free it */

    if (NULL != ldi->ldi_prefix)
      LAM_FREE(ldi->ldi_prefix);
    ldi->ldi_prefix = NULL;

    if (NULL != ldi->ldi_file_suffix)
      LAM_FREE(ldi->ldi_file_suffix);
    ldi->ldi_file_suffix = NULL;

    if (NULL != ldi->ldi_syslog_ident)
      LAM_FREE(ldi->ldi_syslog_ident);
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

  if (!initialized)
    lam_output_init();

  /* If it's valid, used, and enabled, output */

  if (output_id >= 0 && output_id < LAM_OUTPUT_MAX_STREAMS &&
      info[output_id].ldi_used && 
      info[output_id].ldi_enabled) {
    ldi = &info[output_id];
    
    /* Make the formatted string */

    THREAD_LOCK(&mutex);
    str = lam_vsnprintf(format, arglist);
    total_len = len = strlen(str);
    if ('\n' != str[len - 1]) {
      want_newline = true;
      ++total_len;
    }
    if (NULL != ldi->ldi_prefix)
      total_len += strlen(ldi->ldi_prefix);
    if (temp_str_len < total_len + want_newline) {
      if (NULL != temp_str)
	LAM_FREE(temp_str);
      temp_str = malloc(total_len * 2);
      temp_str_len = total_len * 2;
    }
    if (NULL != ldi->ldi_prefix) {
      if (want_newline)
	snprintf(temp_str, temp_str_len, "%s%s\n", ldi->ldi_prefix, str);
      else
	snprintf(temp_str, temp_str_len, "%s%s", ldi->ldi_prefix, str);
    } else {
      if (want_newline) 
	snprintf(temp_str, temp_str_len, "%s\n", str);
      else
	snprintf(temp_str, temp_str_len, "%s", str);
    }

    /* Syslog output */

    if (ldi->ldi_syslog)
      syslog(ldi->ldi_syslog_priority, str);

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

    if (ldi->ldi_fd != -1)
      write(ldi->ldi_fd, temp_str, total_len);
    THREAD_UNLOCK(&mutex);

    LAM_FREE(str);
  }  
}


/* 
 * lam_vsnprintf
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
static char *lam_vsnprintf(char *format, va_list arglist)
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
#if LAM_HAVE_VA_COPY
  va_copy(arglist2, arglist);
#elif LAM_HAVE_UNDERSCORE_VA_COPY
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
  
  sarg = LAM_MALLOC(len);
  vsprintf(sarg, format, arglist2);

  /* Return the new string */

  return sarg;
}
