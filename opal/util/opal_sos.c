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
 * Copyright (c) 2007      Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "opal/util/opal_sos.h"
#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"

/** Global variables */
opal_hash_table_t    opal_sos_table;
opal_mutex_t         opal_sos_table_lock;
bool opal_sos_print_low;

/* Local variables */
static bool opal_sos_initialized = false;
static const char *dash_line = "--------------------------------------------------------------------------";
static const char *stackhdr = "[STACK TRACE]:\n";

/* Local functions */
static void opal_sos_error_construct(opal_sos_error_t *obj);
static void opal_sos_error_destruct(opal_sos_error_t *obj);

/** OPAL SOS callback function pointers */
static opal_sos_print_callback_fn_t cur_print_callback;
static opal_sos_reporter_callback_fn_t cur_reporter_callback;
/* static opal_sos_print_callback_fn_t prev_print_callback; */
static opal_sos_reporter_callback_fn_t prev_reporter_callback;

OBJ_CLASS_INSTANCE(opal_sos_error_t,
                   opal_object_t,
                   opal_sos_error_construct,
                   opal_sos_error_destruct);

/**
 * Constructor
 */
static void opal_sos_error_construct(opal_sos_error_t *obj) 
{
    obj->errnum = 0;
    obj->file = NULL;
    obj->line = 0;
    obj->func = NULL;
    obj->msg = NULL;
    obj->prev = obj->next = OPAL_SOS_ERR_BASE;
}

/**
 * Destructor
 */
static void opal_sos_error_destruct(opal_sos_error_t *obj) 
{
    if (NULL != obj->file) {
        free(obj->file);
    }

    if (NULL != obj->func) {
        free(obj->func);
    }

    if (NULL != obj->msg) {
        free(obj->msg);
    }
}

/**
 * Initialize the OPAL SOS interface
 *
 */
void opal_sos_init(void)
{
    int value;

    if (opal_sos_initialized) {
        return;
    }

    mca_base_param_reg_int_name("opal", "sos_print_low",
                                "Set to non-zero to enable the print-at-bottom"
                                " preference for OPAL SOS. Enabling this option prints"
                                " out the errors, warnings or info messages as"
                                " soon as they are encountered.",
                                false, false, (int)false, &value);

    opal_sos_print_low = OPAL_INT_TO_BOOL(value);

    OBJ_CONSTRUCT(&opal_sos_table, opal_hash_table_t);
    opal_hash_table_init(&opal_sos_table, OPAL_SOS_ERR_TABLE_SIZE);
    OBJ_CONSTRUCT(&opal_sos_table_lock, opal_mutex_t);

    opal_sos_reg_reporter_callback(opal_sos_print_error, &prev_reporter_callback);
    opal_sos_initialized = true;
    return;
}

/**
 * Finalize the OPAL SOS interface
 *
 */
void opal_sos_finalize(void)
{
    OBJ_DESTRUCT(&opal_sos_table);
    OBJ_DESTRUCT(&opal_sos_table_lock);
    opal_sos_initialized = false;
    return;
}

/**
 * Free all the SOS errors represented by the error code pointed to by \c errnum
 *
 */
void opal_sos_free(int *errnum)
{
    opal_sos_error_t *opal_error, *attached_error;
    int err, attached_errnum;

    if (NULL == errnum) {
        return;
    } else if (true == OPAL_SOS_IS_NATIVE(*errnum)) {
        return;
    } else {
        err = *errnum;
    }

    *errnum = OPAL_SOS_GET_ERROR_CODE(err);

    do {
        /* Look for attached errors */
        if (0 != (attached_errnum = OPAL_SOS_GET_ATTACHED_INDEX(err))) {
            OPAL_THREAD_LOCK(&opal_sos_table_lock);
            if (OPAL_SUCCESS != opal_hash_table_get_value_uint32(&opal_sos_table,
                                                                 attached_errnum,
                                                                 (void **)&attached_error)) {
                goto cleanup;
            }
            OPAL_THREAD_UNLOCK(&opal_sos_table_lock);

            /* If there's an attached error trace, free it! */
            if (NULL != attached_error) {
                attached_errnum = attached_error->errnum;
                opal_sos_free(&attached_errnum);
            }
        }

        OPAL_THREAD_LOCK(&opal_sos_table_lock);
        if (OPAL_SUCCESS != opal_hash_table_get_value_uint32(&opal_sos_table,
                                                             OPAL_SOS_GET_INDEX(err),
                                                             (void **)&opal_error)) {
            goto cleanup;
        }
        OPAL_THREAD_UNLOCK(&opal_sos_table_lock);
        if (NULL == opal_error) {
            goto cleanup;
        }

        opal_sos_error_destruct(opal_error);
        /* Remove the entry from the SOS table */
        OPAL_THREAD_LOCK(&opal_sos_table_lock);
        opal_hash_table_remove_value_uint32(&opal_sos_table, OPAL_SOS_GET_INDEX(err));
        OPAL_THREAD_UNLOCK(&opal_sos_table_lock);

        err = opal_error->prev;
    } while (OPAL_SOS_ERR_BASE != err);

cleanup:
    OPAL_THREAD_UNLOCK(&opal_sos_table_lock);
}

opal_sos_error_t *
opal_sos_build_error(int errnum, bool show_stack, const char *errmsg, ...)
{
    opal_sos_error_t *opal_error;
    char *stackframe, msg[OPAL_SOS_MAX_ERR_LEN];
    va_list arglist;
    int ret_errno = 0, len;

    if (!opal_sos_initialized) {
        opal_sos_init();
    }

    opal_error = OBJ_NEW(opal_sos_error_t);
    if (NULL == opal_error) {
        return NULL; /* OPAL_ERR_OUT_OF_RESOURCE */
    }

    va_start(arglist, errmsg);
    len = vsnprintf(msg, OPAL_SOS_MAX_ERR_LEN, errmsg, arglist);
    va_end(arglist);
#if OPAL_WANT_PRETTY_PRINT_STACKTRACE
    if ((true == show_stack) &&
        (NULL != (stackframe = opal_stackframe_output_string()))) {
        len += strlen(stackhdr) + strlen(stackframe) + 2;
        if (len > OPAL_SOS_MAX_ERR_LEN)
            len = OPAL_SOS_MAX_ERR_LEN;

        opal_error->msg = (char *) malloc(len);
        if (NULL == opal_error->msg) {
            return NULL;
        }
        snprintf(opal_error->msg, len, "%s\n%s%s", msg, stackhdr, stackframe);
    } else {
        opal_error->msg = strdup(msg);
    }
#else
    opal_error->msg = strdup ("OPAL_WANT_PRETTY_PRINT_STACKTRACE disabled");
#endif

    /* Check if errnum is a native error code and encode it into
       the encoded error code if it is native */
    if (OPAL_SOS_IS_NATIVE(errnum)) {
        OPAL_SOS_SET_ERROR_CODE(ret_errno, errnum);
    } else {
        /* Extract the native error code from the encoded error and 
           encode it back again into the newly encoded error code */
        OPAL_SOS_SET_ERROR_CODE(ret_errno, OPAL_SOS_GET_ERROR_CODE(errnum));
        opal_error->prev = errnum;
    }

    opal_error->errnum = ret_errno;
    return opal_error;
}

int opal_sos_reporter(const char *file, int line, const char *func, 
                      opal_sos_severity_t severity, opal_sos_error_t *opal_error)
{
    opal_sos_error_t *prev_error;
    int ret_errno = 0, hash;

    if (NULL == opal_error) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Doing more strict validation here since if either of the file,
     * func or msg are not known we replace it by <unknown> to avoid any issues
     * during dss pack/unpack 
     */
    opal_error->file = (NULL != file)?strdup(file):strdup("<unknown>");
    opal_error->func = (NULL != func)?strdup(func):strdup("<unknown>");
    opal_error->line = line;

    ret_errno = opal_error->errnum;
    /* Encode the severity level into the return error code */
    OPAL_SOS_SET_SEVERITY(ret_errno, severity);
    hash = opal_sos_hash_error(opal_error);
    OPAL_SOS_SET_INDEX(ret_errno, hash);
    opal_error->errnum = ret_errno;

    if (opal_sos_print_low) {
        opal_sos_report_error(opal_error);
    }

    /* Add the error object to the error table */
    OPAL_THREAD_LOCK(&opal_sos_table_lock);

    if (OPAL_SUCCESS !=
        opal_hash_table_set_value_uint32(&opal_sos_table,
                                         OPAL_SOS_GET_INDEX(ret_errno),
                                         (void *)opal_error)) {
        OPAL_THREAD_UNLOCK(&opal_sos_table_lock);
        OBJ_DESTRUCT(opal_error);
        return OPAL_ERROR;
    }

    /* Get the previous error in the error call stack and update
       its next error pointer */
    prev_error = NULL;
    opal_hash_table_get_value_uint32(&opal_sos_table,
                                         OPAL_SOS_GET_INDEX(opal_error->prev),
                                         (void **)&prev_error);
    if (NULL != prev_error) {
        prev_error->next = opal_error->errnum;
    }
    OPAL_THREAD_UNLOCK(&opal_sos_table_lock);

    return ret_errno;
}

void
opal_sos_report_error(opal_sos_error_t *error)
{
    opal_sos_severity_t severity;
    char *pretty_error;
    int errnum, ret;

    if (NULL == error)
        return;

    severity = (opal_sos_severity_t)OPAL_SOS_GET_SEVERITY(error->errnum);

    /* An OPAL SOS encoded error number holds no meaning outside
     * the context of Open MPI. We convert it back to the native
     * error code before reporting it. */
    if (true == OPAL_SOS_IS_NATIVE(error->errnum)) {
        errnum = error->errnum;
    } else {
        errnum = OPAL_SOS_GET_ERROR_CODE(error->errnum);
    }

    /* Prettify the error for printing it locally */
    ret = opal_sos_prettify_error(error->msg, &pretty_error);

    (*cur_reporter_callback)(severity, errnum, "<%s> at %s:%d:%s():\n%s",
                             opal_sos_severity2str(severity), error->file, 
                             error->line, error->func, 
                             ((0 > ret) ? error->msg : pretty_error));

    if (ret > 0) {
        free(pretty_error);
    }

    /* Call the previous reporter callback which should be the selected
     * ORTE notifier components */
    if (NULL != prev_reporter_callback) {
        prev_reporter_callback(severity, errnum, "<%s> at %s:%d:%s():\n%s",
                               opal_sos_severity2str(severity), error->file, 
                               error->line, error->func, error->msg);
    }
}

void opal_sos_print(int errnum, bool show_history)
{
    opal_sos_error_t *opal_error, *prev_opal_error, *attached_error;
    int tmp, attached_errnum, prev_severity, severity;

    opal_show_help("opal_sos_reporter.txt", "msg header", false, dash_line);
    tmp = errnum;
    prev_opal_error = NULL;
    do {
        /* If there is an error attached to this error, print it out. */
        if (0 != (attached_errnum = OPAL_SOS_GET_ATTACHED_INDEX(errnum))) {
            OPAL_THREAD_LOCK(&opal_sos_table_lock);
            if (OPAL_SUCCESS != opal_hash_table_get_value_uint32(&opal_sos_table,
                                                                 attached_errnum,
                                                                 (void **)&attached_error)) {
                goto cleanup;
            }
            OPAL_THREAD_UNLOCK(&opal_sos_table_lock);

            if (NULL != attached_error) {
                opal_sos_print(attached_error->errnum, show_history);
            }
        }

        OPAL_THREAD_LOCK(&opal_sos_table_lock);
        if (OPAL_SUCCESS !=
            opal_hash_table_get_value_uint32(&opal_sos_table,
                                             OPAL_SOS_GET_INDEX(errnum),
                                             (void **)&opal_error)) {
            goto cleanup;
        }
        OPAL_THREAD_UNLOCK(&opal_sos_table_lock);
        if (NULL == opal_error) {
            return;
        }

        if (NULL != prev_opal_error) {
            prev_severity = OPAL_SOS_GET_SEVERITY(prev_opal_error->errnum);
            severity = OPAL_SOS_GET_SEVERITY(errnum);

            /* If show_history is enabled, or if the preceeding error
               was of higher severity, then report the error */
            if (show_history || (prev_severity <= severity))
                /* Print the error denoted by errnum. */
                opal_sos_report_error(prev_opal_error);
        }

        prev_opal_error = opal_error;
        /* Get the previous error */
        errnum = opal_error->prev;
        /* Terminating condition */
        if (OPAL_SOS_ERR_BASE == errnum) {
            opal_sos_report_error(opal_error);
        }
    } while (errnum != OPAL_SOS_ERR_BASE);
    opal_show_help("opal_sos_reporter.txt", "msg header", false, dash_line);
    errnum = tmp;
    return;

cleanup:
    OPAL_THREAD_UNLOCK(&opal_sos_table_lock);
}

void opal_sos_print_error(opal_sos_severity_t severity, int errnum, const char *errmsg, ...)
{
    va_list arglist;
    va_start(arglist, errmsg);
    opal_show_vhelp("opal_sos_reporter.txt", "general message", false, arglist);
    va_end(arglist);
}

void opal_sos_log(int errnum)
{
    opal_sos_print(errnum, false);
    opal_sos_free(&errnum);
}

int opal_sos_prettify_error(const char *error, char **pretty_error)
{
    char *str, *token, *saveptr, *errdup;
    const char *prefix = "\n| |  ";
    int len = 0, plen, left;

    if (NULL == error) {
        return OPAL_ERROR;
    }

    *pretty_error = (char *) malloc(OPAL_SOS_MAX_ERR_LEN);
    if (NULL == *pretty_error) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    *(*pretty_error) = '\0';

    plen = strlen(prefix);

    if (NULL != (errdup = strdup(error))) {
        for (str = errdup, len = 0; len < OPAL_SOS_MAX_ERR_LEN; str = NULL) {
            if (NULL == (token = strtok_r(str, "\n", &saveptr))) {
                break;
            }

            left = strlen(token);
            if ((len + left) > OPAL_SOS_MAX_ERR_LEN) {
                left = OPAL_SOS_MAX_ERR_LEN - len;
            }
            strncat(*pretty_error, token, left);
            len += left;

            left = plen;
            if ((len + left) > OPAL_SOS_MAX_ERR_LEN) {
                left = OPAL_SOS_MAX_ERR_LEN - len;
            }
            strncat(*pretty_error, prefix, left);
            len += left;
        }
        free(errdup);
        errdup = NULL;
    }

    return len;
}

const char *opal_sos_severity2str(opal_sos_severity_t severity)
{
    switch(severity) {
    case OPAL_SOS_SEVERITY_EMERG:  return "EMERGENCY";
    case OPAL_SOS_SEVERITY_ALERT:  return "ALERT MESSAGE";
    case OPAL_SOS_SEVERITY_CRIT:   return "CRITICAL MESSAGE";
    case OPAL_SOS_SEVERITY_ERROR:  return "ERROR";
    case OPAL_SOS_SEVERITY_WARN:   return "WARNING";
    case OPAL_SOS_SEVERITY_NOTICE: return "NOTICE";
    case OPAL_SOS_SEVERITY_INFO:   return "INFO MESSAGE";
    case OPAL_SOS_SEVERITY_DEBUG:  return "DEBUG MESSAGE";
    default: return "UNKNOWN ERROR";
    }
}

int opal_sos_hash_error(opal_sos_error_t *error)
{
    int hash, c;
    char *msg;

    /* Naive string hash function to create a key based on the error
    details, namely length of the file name, length of the function
    name and the sum of the characters in the error message */

    hash = error->errnum;
    if (NULL != error->file) {
        hash += strlen(error->file);
    }
    if (NULL != error->func) {
        hash += strlen(error->func);
    }
    if (NULL != error->msg) {
        msg = error->msg;
        while ('\0' != (c = *msg++)) {
            hash += c;
        }
    }

    return (hash & (OPAL_SOS_ERR_TABLE_SIZE - 1));
}

int opal_sos_reg_print_callback(opal_sos_print_callback_fn_t new_func,
                                opal_sos_print_callback_fn_t *prev_func)
{
    /* Preserve the previous print callback */
    *prev_func = cur_print_callback;

    /* Update the current print callback */
    cur_print_callback = new_func;
    return OPAL_SUCCESS;
}

int opal_sos_reg_reporter_callback(opal_sos_reporter_callback_fn_t new_func,
                                   opal_sos_reporter_callback_fn_t *prev_func)
{
    /* Preserve the previous reporter callback */
    *prev_func = cur_reporter_callback;

    /* Update the current reporter callback */
    cur_reporter_callback = new_func;
    return OPAL_SUCCESS;
}
