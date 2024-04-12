/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "pmix_common.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_SYSLOG_H
#    include <syslog.h>
#endif
#include <fcntl.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif

#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

/*
 * Private data
 */
static int verbose_stream = -1;
static pmix_output_stream_t verbose;
static char *output_dir = NULL;
static char *output_prefix = NULL;

/*
 * Internal data structures and helpers for the generalized output
 * stream mechanism.
 */
/*
 * Private functions
 */
static void construct(pmix_object_t *stream);
static void destruct(pmix_object_t *stream);
static int do_open(int output_id, pmix_output_stream_t *lds);
static int open_file(int i);
static void free_descriptor(int output_id);
static int make_string(char **out, char **no_newline_string, pmix_output_desc_t *ldi, const char *format,
                       va_list arglist);
static int output(int output_id, const char *format, va_list arglist);

#if defined(HAVE_SYSLOG)
#    define USE_SYSLOG 1
#else
#    define USE_SYSLOG 0
#endif

/* global state */
bool pmix_output_redirected_to_syslog = false;
int pmix_output_redirected_syslog_pri = 0;
pmix_output_desc_t pmix_output_info[PMIX_OUTPUT_MAX_STREAMS];
/*
 * Local state
 */
static bool initialized = false;
static int default_stderr_fd = -1;

#if defined(HAVE_SYSLOG)
static bool syslog_opened = false;
#endif
static char *redirect_syslog_ident = NULL;

PMIX_CLASS_INSTANCE(pmix_output_stream_t, pmix_object_t, construct, destruct);

/*
 * Setup the output stream infrastructure
 */
bool pmix_output_init(void)
{
    int i;
    char hostname[PMIX_MAXHOSTNAMELEN] = {0};
    char *str;

    if (initialized) {
        return true;
    }

    str = getenv("PMIX_OUTPUT_STDERR_FD");
    if (NULL != str) {
        default_stderr_fd = atoi(str);
    }
    str = getenv("PMIX_OUTPUT_REDIRECT");
    if (NULL != str) {
        if (0 == strcasecmp(str, "syslog")) {
            pmix_output_redirected_to_syslog = true;
        }
    }
    str = getenv("PMIX_OUTPUT_SYSLOG_PRI");
#ifdef HAVE_SYSLOG_H
    if (NULL != str) {
        if (0 == strcasecmp(str, "info")) {
            pmix_output_redirected_syslog_pri = LOG_INFO;
        } else if (0 == strcasecmp(str, "error")) {
            pmix_output_redirected_syslog_pri = LOG_ERR;
        } else if (0 == strcasecmp(str, "warn")) {
            pmix_output_redirected_syslog_pri = LOG_WARNING;
        } else {
            pmix_output_redirected_syslog_pri = LOG_ERR;
        }
    } else {
        pmix_output_redirected_syslog_pri = LOG_ERR;
    }
#endif

    str = getenv("PMIX_OUTPUT_SYSLOG_IDENT");
    if (NULL != str) {
        redirect_syslog_ident = strdup(str);
    }

    PMIX_CONSTRUCT(&verbose, pmix_output_stream_t);
    if (pmix_output_redirected_to_syslog) {
        verbose.lds_want_syslog = true;
        verbose.lds_syslog_priority = pmix_output_redirected_syslog_pri;
        if (NULL != str) {
            verbose.lds_syslog_ident = strdup(redirect_syslog_ident);
        }
        verbose.lds_want_stderr = false;
        verbose.lds_want_stdout = false;
    } else {
        verbose.lds_want_stderr = true;
    }
    gethostname(hostname, sizeof(hostname) - 1);
    hostname[sizeof(hostname) - 1] = '\0';
    if (0 > asprintf(&verbose.lds_prefix, "[%s:%05d] ", hostname, getpid())) {
        return PMIX_ERR_NOMEM;
    }

    for (i = 0; i < PMIX_OUTPUT_MAX_STREAMS; ++i) {
        pmix_output_info[i].ldi_used = false;
        pmix_output_info[i].ldi_enabled = false;

        pmix_output_info[i].ldi_syslog = pmix_output_redirected_to_syslog;
        pmix_output_info[i].ldi_file = false;
        pmix_output_info[i].ldi_file_suffix = NULL;
        pmix_output_info[i].ldi_file_want_append = false;
        pmix_output_info[i].ldi_fd = -1;
        pmix_output_info[i].ldi_file_num_lines_lost = 0;
    }

    initialized = true;

    /* Set some defaults */

    if (0 > asprintf(&output_prefix, "pmix-output-pid%d-", getpid())) {
        return false;
    }
    output_dir = strdup(pmix_tmp_directory());

    /* Open the default verbose stream */
    verbose_stream = pmix_output_open(&verbose);
    return true;
}

/*
 * Open a stream
 */
int pmix_output_open(pmix_output_stream_t *lds)
{
    return do_open(-1, lds);
}

/*
 * Reset the parameters on a stream
 */
int pmix_output_reopen(int output_id, pmix_output_stream_t *lds)
{
    return do_open(output_id, lds);
}

/*
 * Enable and disable output streams
 */
bool pmix_output_switch(int output_id, bool enable)
{
    bool ret = false;

    /* Setup */

    if (!initialized) {
        pmix_output_init();
    }

    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS) {
        ret = pmix_output_info[output_id].ldi_enabled;
        pmix_output_info[output_id].ldi_enabled = enable;
    }

    return ret;
}

/*
 * Reopen all the streams; used during checkpoint/restart.
 */
void pmix_output_reopen_all(void)
{
    char *str;
    char hostname[PMIX_MAXHOSTNAMELEN] = {0};

    str = getenv("PMIX_OUTPUT_STDERR_FD");
    if (NULL != str) {
        default_stderr_fd = atoi(str);
    } else {
        default_stderr_fd = -1;
    }

    gethostname(hostname, sizeof(hostname));
    if (NULL != verbose.lds_prefix) {
        free(verbose.lds_prefix);
        verbose.lds_prefix = NULL;
    }
    if (0 > asprintf(&verbose.lds_prefix, "[%s:%05d] ", hostname, getpid())) {
        verbose.lds_prefix = NULL;
        return;
    }
}

/*
 * Close a stream
 */
void pmix_output_close(int output_id)
{
    int i;

    /* Setup */

    if (!initialized) {
        return;
    }

    /* If it's valid, used, enabled, and has an open file descriptor,
     * free the resources associated with the descriptor */

    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS && pmix_output_info[output_id].ldi_used
        && pmix_output_info[output_id].ldi_enabled) {
        free_descriptor(output_id);

        /* If no one has the syslog open, we should close it */

        for (i = 0; i < PMIX_OUTPUT_MAX_STREAMS; ++i) {
            if (pmix_output_info[i].ldi_used && pmix_output_info[i].ldi_syslog) {
                break;
            }
        }

#if defined(HAVE_SYSLOG)
        if (i >= PMIX_OUTPUT_MAX_STREAMS && syslog_opened) {
            closelog();
        }
#endif
    }
}

/*
 * Main function to send output to a stream
 */
PMIX_EXPORT void pmix_output(int output_id, const char *format, ...)
{
    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS) {
        va_list arglist;
        va_start(arglist, format);
        output(output_id, format, arglist);
        va_end(arglist);
    }
}


/*
 * Set the verbosity level of a stream
 */
void pmix_output_set_verbosity(int output_id, int level)
{
    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS) {
        pmix_output_info[output_id].ldi_verbose_level = level;
    }
}

/*
 * Control where output flies will go
 */
void pmix_output_set_output_file_info(const char *dir, const char *prefix, char **olddir,
                                      char **oldprefix)
{
    if (NULL != olddir) {
        *olddir = strdup(output_dir);
    }
    if (NULL != oldprefix) {
        *oldprefix = strdup(output_prefix);
    }

    if (NULL != dir) {
        free(output_dir);
        output_dir = strdup(dir);
    }
    if (NULL != prefix) {
        free(output_prefix);
        output_prefix = strdup(prefix);
    }
}

void pmix_output_hexdump(int verbose_level, int output_id, void *ptr, int buflen)
{
    unsigned char *buf = (unsigned char *) ptr;
    char out_buf[120];
    int ret = 0;
    int out_pos = 0;
    int i, j;

    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS
        && pmix_output_info[output_id].ldi_verbose_level >= verbose_level) {
        pmix_output_verbose(verbose_level, output_id, "dump data at %p %d bytes\n", ptr, buflen);
        for (i = 0; i < buflen; i += 16) {
            out_pos = 0;
            ret = sprintf(out_buf + out_pos, "%06x: ", i);
            if (ret < 0)
                return;
            out_pos += ret;
            for (j = 0; j < 16; j++) {
                if (i + j < buflen)
                    ret = sprintf(out_buf + out_pos, "%02x ", buf[i + j]);
                else
                    ret = sprintf(out_buf + out_pos, "   ");
                if (ret < 0)
                    return;
                out_pos += ret;
            }
            ret = sprintf(out_buf + out_pos, " ");
            if (ret < 0)
                return;
            out_pos += ret;
            for (j = 0; j < 16; j++)
                if (i + j < buflen) {
                    ret = sprintf(out_buf + out_pos, "%c", isprint(buf[i + j]) ? buf[i + j] : '.');
                    if (ret < 0)
                        return;
                    out_pos += ret;
                }
            ret = sprintf(out_buf + out_pos, "\n");
            if (ret < 0)
                return;
            pmix_output_verbose(verbose_level, output_id, "%s", out_buf);
        }
    }
}

/*
 * Shut down the output stream system
 */
void pmix_output_finalize(void)
{
    if (initialized) {
        if (verbose_stream != -1) {
            pmix_output_close(verbose_stream);
        }
        free(verbose.lds_prefix);
        verbose_stream = -1;

        free(output_prefix);
        free(output_dir);
        PMIX_DESTRUCT(&verbose);
        initialized = false;
    }
}

/************************************************************************/

/*
 * Constructor
 */
static void construct(pmix_object_t *obj)
{
    pmix_output_stream_t *stream = (pmix_output_stream_t *) obj;

    stream->lds_verbose_level = 0;
    stream->lds_syslog_priority = 0;
    stream->lds_syslog_ident = NULL;
    stream->lds_prefix = NULL;
    stream->lds_suffix = NULL;
    stream->lds_is_debugging = false;
    stream->lds_want_syslog = false;
    stream->lds_want_stdout = false;
    stream->lds_want_stderr = false;
    stream->lds_want_file = false;
    stream->lds_want_file_append = false;
    stream->lds_file_suffix = NULL;
}
static void destruct(pmix_object_t *obj)
{
    pmix_output_stream_t *stream = (pmix_output_stream_t *) obj;

    if (NULL != stream->lds_file_suffix) {
        free(stream->lds_file_suffix);
        stream->lds_file_suffix = NULL;
    }
}

/*
 * Back-end of open() and reopen().
 */
static int do_open(int output_id, pmix_output_stream_t *lds)
{
    int i;
    bool redirect_to_file = false;
    char *str, *sfx;

    /* Setup */

    if (!initialized) {
        pmix_output_init();
    }

    str = getenv("PMIX_OUTPUT_REDIRECT");
    if (NULL != str && 0 == strcasecmp(str, "file")) {
        redirect_to_file = true;
    }
    sfx = getenv("PMIX_OUTPUT_SUFFIX");

    /* If output_id == -1, find an available stream, or return
     * PMIX_ERROR */

    if (-1 == output_id) {
        for (i = 0; i < PMIX_OUTPUT_MAX_STREAMS; ++i) {
            if (!pmix_output_info[i].ldi_used) {
                break;
            }
        }
        if (i >= PMIX_OUTPUT_MAX_STREAMS) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Otherwise, we're reopening, so we need to free all previous
     * resources, close files, etc. */

    else {
        free_descriptor(output_id);
        i = output_id;
    }

    /* Special case: if we got NULL for lds, then just use the default
     * verbose */

    if (NULL == lds) {
        lds = &verbose;
    }

    /* Got a stream -- now initialize it and open relevant outputs */

    pmix_output_info[i].ldi_used = true;
    pmix_output_info[i].ldi_enabled = lds->lds_is_debugging ? (bool) PMIX_ENABLE_DEBUG : true;
    pmix_output_info[i].ldi_verbose_level = lds->lds_verbose_level;

#if USE_SYSLOG
#    if defined(HAVE_SYSLOG)
    if (pmix_output_redirected_to_syslog) {
        pmix_output_info[i].ldi_syslog = true;
        pmix_output_info[i].ldi_syslog_priority = pmix_output_redirected_syslog_pri;
        if (NULL != redirect_syslog_ident) {
            pmix_output_info[i].ldi_syslog_ident = strdup(redirect_syslog_ident);
            openlog(redirect_syslog_ident, LOG_PID, LOG_USER);
        } else {
            pmix_output_info[i].ldi_syslog_ident = NULL;
            openlog("pmix", LOG_PID, LOG_USER);
        }
        syslog_opened = true;
    } else {
#    endif
        pmix_output_info[i].ldi_syslog = lds->lds_want_syslog;
        if (lds->lds_want_syslog) {

#    if defined(HAVE_SYSLOG)
            if (NULL != lds->lds_syslog_ident) {
                pmix_output_info[i].ldi_syslog_ident = strdup(lds->lds_syslog_ident);
                openlog(lds->lds_syslog_ident, LOG_PID, LOG_USER);
            } else {
                pmix_output_info[i].ldi_syslog_ident = NULL;
                openlog("pmix", LOG_PID, LOG_USER);
            }
#    endif
            syslog_opened = true;
            pmix_output_info[i].ldi_syslog_priority = lds->lds_syslog_priority;
        }

#    if defined(HAVE_SYSLOG)
    }
#    endif

#else
    pmix_output_info[i].ldi_syslog = false;
#endif

    if (NULL != lds->lds_prefix) {
        pmix_output_info[i].ldi_prefix = strdup(lds->lds_prefix);
        pmix_output_info[i].ldi_prefix_len = (int) strlen(lds->lds_prefix);
    } else {
        pmix_output_info[i].ldi_prefix = NULL;
        pmix_output_info[i].ldi_prefix_len = 0;
    }

    if (NULL != lds->lds_suffix) {
        pmix_output_info[i].ldi_suffix = strdup(lds->lds_suffix);
        pmix_output_info[i].ldi_suffix_len = (int) strlen(lds->lds_suffix);
    } else {
        pmix_output_info[i].ldi_suffix = NULL;
        pmix_output_info[i].ldi_suffix_len = 0;
    }

    if (pmix_output_redirected_to_syslog) {
        /* since all is redirected to syslog, ensure
         * we don't duplicate the output to the std places
         */
        pmix_output_info[i].ldi_stdout = false;
        pmix_output_info[i].ldi_stderr = false;
        pmix_output_info[i].ldi_file = false;
        pmix_output_info[i].ldi_fd = -1;
    } else {
        /* since we aren't redirecting to syslog, use what was
         * given to us
         */
        if (NULL != str && redirect_to_file) {
            pmix_output_info[i].ldi_stdout = false;
            pmix_output_info[i].ldi_stderr = false;
            pmix_output_info[i].ldi_file = true;
        } else {
            pmix_output_info[i].ldi_stdout = lds->lds_want_stdout;
            pmix_output_info[i].ldi_stderr = lds->lds_want_stderr;

            pmix_output_info[i].ldi_fd = -1;
            pmix_output_info[i].ldi_file = lds->lds_want_file;
        }
        if (NULL != sfx) {
            pmix_output_info[i].ldi_file_suffix = strdup(sfx);
        } else {
            pmix_output_info[i].ldi_file_suffix = (NULL == lds->lds_file_suffix) ? NULL
                                                                     : strdup(lds->lds_file_suffix);
        }
        pmix_output_info[i].ldi_file_want_append = lds->lds_want_file_append;
        pmix_output_info[i].ldi_file_num_lines_lost = 0;
    }

    /* Don't open a file in the session directory now -- do that lazily
     * so that if there's no output, we don't have an empty file */

    return i;
}

static int open_file(int i)
{
    int flags;
    char *filename;
    int n;

    /* first check to see if this file is already open
     * on someone else's stream - if so, we don't want
     * to open it twice
     */
    for (n = 0; n < PMIX_OUTPUT_MAX_STREAMS; n++) {
        if (i == n) {
            continue;
        }
        if (!pmix_output_info[n].ldi_used) {
            continue;
        }
        if (!pmix_output_info[n].ldi_file) {
            continue;
        }
        if (NULL != pmix_output_info[i].ldi_file_suffix && NULL != pmix_output_info[n].ldi_file_suffix) {
            if (0 != strcmp(pmix_output_info[i].ldi_file_suffix, pmix_output_info[n].ldi_file_suffix)) {
                break;
            }
        }
        if (NULL == pmix_output_info[i].ldi_file_suffix && NULL != pmix_output_info[n].ldi_file_suffix) {
            break;
        }
        if (NULL != pmix_output_info[i].ldi_file_suffix && NULL == pmix_output_info[n].ldi_file_suffix) {
            break;
        }
        if (pmix_output_info[n].ldi_fd < 0) {
            break;
        }
        pmix_output_info[i].ldi_fd = pmix_output_info[n].ldi_fd;
        return PMIX_SUCCESS;
    }

    /* Setup the filename and open flags */

    if (NULL != output_dir) {
        filename = (char *) malloc(PMIX_PATH_MAX);
        if (NULL == filename) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        pmix_strncpy(filename, output_dir, PMIX_PATH_MAX - 1);
        strcat(filename, "/");
        if (NULL != output_prefix) {
            strcat(filename, output_prefix);
        }
        if (pmix_output_info[i].ldi_file_suffix != NULL) {
            strcat(filename, pmix_output_info[i].ldi_file_suffix);
        } else {
            pmix_output_info[i].ldi_file_suffix = NULL;
            strcat(filename, "output.txt");
        }
        flags = O_CREAT | O_RDWR;
        if (!pmix_output_info[i].ldi_file_want_append) {
            flags |= O_TRUNC;
        }

        /* Actually open the file */
        pmix_output_info[i].ldi_fd = open(filename, flags, 0644);
        free(filename); /* release the filename in all cases */
        if (-1 == pmix_output_info[i].ldi_fd) {
            pmix_output_info[i].ldi_used = false;
            return PMIX_ERR_IN_ERRNO;
        }

        /* Make the file be close-on-exec to prevent child inheritance
         * problems */
        if (-1 == fcntl(pmix_output_info[i].ldi_fd, F_SETFD, 1)) {
            return PMIX_ERR_IN_ERRNO;
        }
    }

    /* Return successfully even if the session dir did not exist yet;
     * we'll try opening it later */

    return PMIX_SUCCESS;
}

/*
 * Free all the resources associated with a descriptor.
 */
static void free_descriptor(int output_id)
{
    pmix_output_desc_t *ldi;

    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS && pmix_output_info[output_id].ldi_used
        && pmix_output_info[output_id].ldi_enabled) {
        ldi = &pmix_output_info[output_id];

        if (-1 != ldi->ldi_fd) {
            close(ldi->ldi_fd);
        }
        ldi->ldi_used = false;

        /* If we strduped a prefix, suffix, or syslog ident, free it */

        if (NULL != ldi->ldi_prefix) {
            free(ldi->ldi_prefix);
        }
        ldi->ldi_prefix = NULL;

        if (NULL != ldi->ldi_suffix) {
            free(ldi->ldi_suffix);
        }
        ldi->ldi_suffix = NULL;

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

static int make_string(char **out, char **no_newline_string, pmix_output_desc_t *ldi, const char *format,
                       va_list arglist)
{
    size_t len, total_len, temp_str_len;
    bool want_newline = false;
    char *temp_str;

    /* Make the formatted string */
    *out = NULL;
    if (0 > vasprintf(no_newline_string, format, arglist)) {
        return PMIX_ERR_NOMEM;
    }
    total_len = len = strlen(*no_newline_string);
    if ('\n' != (*no_newline_string)[len - 1]) {
        want_newline = true;
        ++total_len;
    } else if (NULL != ldi->ldi_suffix) {
        /* if we have a suffix, then we don't want a
         * newline to appear before it
         */
        (*no_newline_string)[len - 1] = '\0';
        want_newline = true; /* add newline to end after suffix */
        /* total_len won't change since we just moved the newline
         * to appear after the suffix
         */
    }
    if (NULL != ldi->ldi_prefix) {
        total_len += strlen(ldi->ldi_prefix);
    }
    if (NULL != ldi->ldi_suffix) {
        total_len += strlen(ldi->ldi_suffix);
    }
    temp_str = (char *) malloc(total_len * 2);
    if (NULL == temp_str) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    temp_str_len = total_len * 2;
    if (NULL != ldi->ldi_prefix && NULL != ldi->ldi_suffix) {
        if (want_newline) {
            pmix_snprintf(temp_str, temp_str_len, "%s%s%s\n", ldi->ldi_prefix, *no_newline_string,
                     ldi->ldi_suffix);
        } else {
            pmix_snprintf(temp_str, temp_str_len, "%s%s%s", ldi->ldi_prefix, *no_newline_string,
                     ldi->ldi_suffix);
        }
    } else if (NULL != ldi->ldi_prefix) {
        if (want_newline) {
            pmix_snprintf(temp_str, temp_str_len, "%s%s\n", ldi->ldi_prefix, *no_newline_string);
        } else {
            pmix_snprintf(temp_str, temp_str_len, "%s%s", ldi->ldi_prefix, *no_newline_string);
        }
    } else if (NULL != ldi->ldi_suffix) {
        if (want_newline) {
            pmix_snprintf(temp_str, temp_str_len, "%s%s\n", *no_newline_string, ldi->ldi_suffix);
        } else {
            pmix_snprintf(temp_str, temp_str_len, "%s%s", *no_newline_string, ldi->ldi_suffix);
        }
    } else {
        if (want_newline) {
            pmix_snprintf(temp_str, temp_str_len, "%s\n", *no_newline_string);
        } else {
            pmix_snprintf(temp_str, temp_str_len, "%s", *no_newline_string);
        }
    }
    *out = temp_str;
    return PMIX_SUCCESS;
}

/*
 * Do the actual output.  Take a va_list so that we can be called from
 * multiple different places, even functions that took "..." as input
 * arguments.
 */
static int output(int output_id, const char *format, va_list arglist)
{
    int rc = PMIX_SUCCESS;
    char *str = NULL, *out = NULL;
    pmix_output_desc_t *ldi;

    /* Setup */

    if (!initialized) {
        pmix_output_init();
    }

    /* If it's valid, used, and enabled, output */

    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS && pmix_output_info[output_id].ldi_used
        && pmix_output_info[output_id].ldi_enabled) {
        ldi = &pmix_output_info[output_id];

        /* Make the strings */
        if (PMIX_SUCCESS != (rc = make_string(&out, &str, ldi, format, arglist))) {
            goto cleanup;
        }

        /* Syslog output -- does not use the newline-appended string */
#if defined(HAVE_SYSLOG)
        if (ldi->ldi_syslog) {
            syslog(ldi->ldi_syslog_priority, "%s", str);
        }
#endif

        /* stdout output */
        if (ldi->ldi_stdout) {
            if (0 > write(fileno(stdout), out, (int) strlen(out))) {
                rc = PMIX_ERROR;
                goto cleanup;
            }
            fflush(stdout);
        }

        /* stderr output */
        if (ldi->ldi_stderr) {
            if (0 > write((-1 == default_stderr_fd) ? fileno(stderr) : default_stderr_fd, out,
                          (int) strlen(out))) {
                rc = PMIX_ERROR;
                goto cleanup;
            }
            fflush(stderr);
        }

        /* File output -- first check to see if the file opening was
         * delayed.  If so, try to open it.  If we failed to open it,
         * then just discard (there are big warnings in the
         * pmix_pmix_output.h docs about this!). */

        if (ldi->ldi_file) {
            if (ldi->ldi_fd == -1) {
                if (PMIX_SUCCESS != open_file(output_id)) {
                    ++ldi->ldi_file_num_lines_lost;
                } else if (ldi->ldi_file_num_lines_lost > 0 && 0 <= ldi->ldi_fd) {
                    char buffer[BUFSIZ];
                    memset(buffer, 0, BUFSIZ);
                    pmix_snprintf(buffer, BUFSIZ - 1,
                             "[WARNING: %d lines lost because the PMIx process session directory "
                             "did\n not exist when pmix_output() was invoked]\n",
                             ldi->ldi_file_num_lines_lost);
                    if (0 > write(ldi->ldi_fd, buffer, (int) strlen(buffer))) {
                        rc = PMIX_ERROR;
                        goto cleanup;
                    }
                    ldi->ldi_file_num_lines_lost = 0;
                }
            }
            if (ldi->ldi_fd != -1) {
                if (0 > write(ldi->ldi_fd, out, (int) strlen(out))) {
                    rc = PMIX_ERROR;
                    goto cleanup;
                }
            }
        }
        free(str);
        str = NULL;
    }

cleanup:
    if (NULL != str) {
        free(str);
    }
    if (NULL != out) {
        free(out);
    }
    return rc;
}

int pmix_output_get_verbosity(int output_id)
{
    if (output_id >= 0 && output_id < PMIX_OUTPUT_MAX_STREAMS && pmix_output_info[output_id].ldi_used) {
        return pmix_output_info[output_id].ldi_verbose_level;
    } else {
        return -1;
    }
}
