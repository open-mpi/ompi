/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SOS_H
#define OPAL_SOS_H

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif

#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/mutex.h"
#include "opal/util/output.h"

#ifdef __STDC_VERSION__
# if __STDC_VERSION__ < 199901L
#  if defined(__GNUC__) && __GNUC__ >= 2
#   define OPAL_SOS_FUNCTION __FUNCTION__
#  else
#   define OPAL_SOS_FUNCION "<unknown>"
#  endif
# else
#  define OPAL_SOS_FUNCTION __func__
# endif
#else
# define OPAL_SOS_FUNCTION __func__
#endif

/* Internal use only */
#define OPAL_SOS_ERR_BASE  OPAL_SUCCESS

/**
 * Size of the OPAL SOS error table.
 *
 * Since the index into the error table that is encoded in the error
 * code is 9-bit long, setting a higher value than (1 << 9) would make
 * no difference at all.
 */
#define OPAL_SOS_ERR_TABLE_SIZE 512

/**
 * Maximum length for the error string stored per error code in the
 * OPAL SOS error table.
 */
#define OPAL_SOS_MAX_ERR_LEN 1024

/**
 * Reports an error to OPAL SOS reporter.
 *
 * Encodes an informational message with severity \c severity and
 * other passed arguments like errnum, errmsg etc. It also remembers
 * the line number, file name and the function name where the error
 * has occurred.
 * If the MCA parameter \c opal_sos_print_low is set, the error message
 * is displayed on stderr using the "show help" subsystem. By default,
 * informational messages are not printed out on stderr.
 * If \c show_stack is set, the stacktrace is saved and/or printed
 * along with the corresponding \c errmsg.
 */
#define OPAL_SOS_REPORT(severity, arg) opal_sos_reporter(__FILE__, __LINE__,    \
                                                         OPAL_SOS_FUNCTION,     \
                                                         severity,              \
                                                         opal_sos_build_error arg)

/**
 * Print or store an event with the maximum severity (EMERG).
 */
#define OPAL_SOS_EMERG(arg)  OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_EMERG, arg)

/**
 * Report an event of severity "ALERT".
 */
#define OPAL_SOS_ALERT(arg)  OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_ALERT, arg)

/**
 * Report events with severity marked as "CRITICAL".
 */
#define OPAL_SOS_CRIT(arg) 	 OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_CRIT,  arg)

/**
 * Prints and/or logs an error.
 * This function can be used to log or print error events.
 */
#define OPAL_SOS_ERROR(arg)  OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_ERROR, arg)

/**
 * Prints and/or logs a warning.
 *
 * This function is similar to OPAL_SOS_INFO but with a higher
 * severity. These events are printed out on the output stream
 * by default.
 */
#define OPAL_SOS_WARN(arg) 	 OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_WARN,  arg)

/**
 * Report an error event with severity "NOTICE".
 */
#define OPAL_SOS_NOTICE(arg) OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_NOTICE,arg)

/**
 * Prints or logs an informational message in the OPAL SOS framework.
 * Events with this severity are not printed, by default. However,
 * they are still stored in the SOS table.
 */
#define OPAL_SOS_INFO(arg) 	 OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_INFO,  arg)

/**
 * Log debug events in the SOS framework.
 */
#define OPAL_SOS_DEBUG(arg)  OPAL_SOS_REPORT(OPAL_SOS_SEVERITY_DEBUG, arg)

/**
 * Frees all the (entire stack of) OPAL SOS error objects associated 
 * with the encoded error code obtained after dereferencing the 
 * pointer \c errnum.
 */
#define OPAL_SOS_FREE(perrnum) opal_sos_free(perrnum)

/**
 * Print the warnings/errors/informational messages previously logged
 * in to the SOS framework.
 *
 * This function prints the error details encoded by \c errnum.
 * If \c show_history is true, the entire history for the error
 * represented by \c errnum is printed on the output stream.
 */
#define OPAL_SOS_PRINT(errnum, show_history)                             \
    opal_sos_print(errnum, show_history)

/**
 * Attach the history from one error code to another error code
 * Returns the target encoded error \c errtgt with history of \c
 * errnum associated to it.
 */
#define OPAL_SOS_ATTACH(errtgt, errnum)                                  \
    (errtgt = -((-errtgt & ~0xFF80000L) |                                \
    ((OPAL_SOS_GET_INDEX(errnum) & 0x1FFL) * 0x80000L)))

/**
 * Returns the index of the error attached to errnum using OPAL_SOS_ATTACH().
 */
#define OPAL_SOS_GET_ATTACHED_INDEX(errnum) ((int) ((-errnum & 0xFF80000L) >> 19))

/** 
 * Returns the native error code for the given encoded error code \c
 * errnum. \c errnum can be a native error code itself.
 */
#define OPAL_SOS_GET_ERROR_CODE(errnum)                                 \
    ((errnum >= 0) ? errnum : (int) -(-errnum & 0x3FFL))

/**
 * Sets the native error code for the potentially encoded error code.
 *
 * The lower 10 bits are reserved for the native error code. This
 * macro sets the lower 10 bits of errnum to nativeerr. 
 */
#define OPAL_SOS_SET_ERROR_CODE(errnum, nativeerr)                      \
    (errnum = -((-errnum & ~0x3FFL) | (-nativeerr & 0x3FFL)))

/**
 * Macro to check if the error encoded by \c errnum is a native error
 * or an OPAL SOS encoded error.
 */
#define OPAL_SOS_IS_NATIVE(errnum)  ((-errnum & ~0x3FFL) == 0)

/**
 * Returns the severity level for the potentially encoded error code.
 *
 * The severity is encoded in the last three bits of the first nibble. 
 */
#define OPAL_SOS_GET_SEVERITY(errnum) ((int)((-errnum >> 28) & 0x7L))

/**
 * Sets the severity level for the given error code \c errnum.
 * 
 * This macros do not do strict error checking of the specified
 * severity levels.
 */
#define OPAL_SOS_SET_SEVERITY(errnum, severity)                         \
    (errnum = -((-errnum & ~0x70000000L) | ((severity & 0x7L) * 0x10000000L)))

/**
 * Macro to get the encoded error severity level as a string.
 *
 * This macro accepts the argument \c severity and calls the corresponding
 * function opal_sos_severity2str to convert it to a string. The result
 * is returned in a static buffer that should not be freed with free().
 */
#define OPAL_SOS_SEVERITY2STR(severity) opal_sos_severity2str(severity)

/**
 * Log an encoded error \c errnum.
 *
 * This macro prints out and consequently frees the entire stack of
 * errors associated with the \c errnum.
 */
#define OPAL_SOS_LOG(errnum) opal_sos_log(errnum)

/**
 * \internal
 * Returns the index into the error table of the error encoded by \c errnum.
 *
 * The index is 9-bit long stored from bit 11 to bit 20 in the encoded
 * error code. 
 */
#define OPAL_SOS_GET_INDEX(errnum) ((int)((-errnum & 0x7FC00L) >> 10))

/**
 * \internal
 * Sets the index into the error table for the error encoded by \c errnum.
 */
#define OPAL_SOS_SET_INDEX(errnum, index)                               \
    (errnum = -((-errnum & ~0x7FC00L) | ((index & 0x1FFL) * 0x400L)))

BEGIN_C_DECLS

/** This MCA parameter sos_print_low can be set to non-zero to enable
 *  the print-at-bottom preference for OPAL SOS. */
OPAL_DECLSPEC extern bool opal_sos_print_low;

/* Severity levels for OPAL SOS */
typedef enum {
    OPAL_SOS_SEVERITY_EMERG  = LOG_EMERG,
    OPAL_SOS_SEVERITY_ALERT  = LOG_ALERT,
    OPAL_SOS_SEVERITY_CRIT   = LOG_CRIT,
    OPAL_SOS_SEVERITY_ERROR  = LOG_ERR,
    OPAL_SOS_SEVERITY_WARN   = LOG_WARNING,
    OPAL_SOS_SEVERITY_NOTICE = LOG_NOTICE,
    OPAL_SOS_SEVERITY_INFO   = LOG_INFO,
    OPAL_SOS_SEVERITY_DEBUG  = LOG_DEBUG
} opal_sos_severity_t;

typedef struct opal_sos_error_t {
    /** Class parent */
    opal_object_t super;

    /**
     * The encoded error code for a given type of error.
     *
     * errnum encodes a native error code (lower 10 bits) with the
     * current severity (higher 2 bits) and an index into the error
     * table along with the associated error, if there is one.
     */
    int errnum;

    /** File in which the error occured */
    char *file;

    /** Line number on which the error was encountered */
    int line;

    /** This is an optional parameter that indicates the function in
        which the error occured */
    char *func;

    /** The actual error message or string for the error indicated by
        \c errnum */
    char *msg;

    /** Encoded error numbers of the previous and the next error.
        These are used are used to maintain the history of an error.
        The complete history of an error can be printed later using
        OPAL_SOS_PRINT() */
    int prev;
    int next;
} opal_sos_error_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_sos_error_t);

/**
 * Signature for OPAL SOS print function callback type.
 */
typedef void (*opal_sos_print_callback_fn_t) (int errcode);

/**
 * Signature for OPAL SOS reporter function callback type.
 */
typedef void (*opal_sos_reporter_callback_fn_t) (opal_sos_severity_t severity, int errcode, 
                                                 const char *msg, ...)
                                                 __opal_attribute_format_funcptr__(__printf__, 3, 4);

/**
 * A global handle that points to the local OPAL SOS table.
 * This is used by the notifier components to reference the local OPAL
 * SOS table, especially for packing/unpacking and sending it over to
 * the HNP.
 */
OPAL_DECLSPEC extern opal_hash_table_t opal_sos_table;

/**
 * A global handle that points to the OPAL SOS table lock.
 *
 */
OPAL_DECLSPEC extern opal_mutex_t opal_sos_table_lock;

/**
 * \internal
 *
 * Initialize OPAL SOS.
 *
 * This function initializes and sets up the structures required to
 * track the data handled by OPAL SOS. It is invoked by
 * opal_util().
 */
void opal_sos_init(void);

/**
 * \internal
 *
 * Shut down OPAL SOS.
 *
 * Invoked by opal_finalize() to deallocate the structures needed by
 * OPAL SOS.
 */
void opal_sos_finalize(void);

/**
 * Prints or relays the error locally or using the selected notifier
 * components.
 */
void
opal_sos_report_error(opal_sos_error_t *error);

/** 
 * Builds an OPAL SOS error object given the parameters errnum,
 * show_stack and errmsg.
 * NOTE: This function only partially populates the SOS error object
 * structure, setting the error message details but nothing about where
 * the error occurred. Filling up the rest of the error object is left
 * to OPAL SOS reporter which then handles the error appropriately. 
 * 
 * @param errnum 
 * @param show_stack 
 * @param errmsg 
 * 
 * @return 
 */
OPAL_DECLSPEC opal_sos_error_t *
opal_sos_build_error(int errnum, bool show_stack, 
                     const char *errmsg, ...)
                     __opal_attribute_format_funcptr__(__printf__, 3, 4);

/**
 * OPAL SOS reporter logs the error in the OPAL SOS error table or
 * prints it out depending on the associated reporter callback. It can 
 * also relay the error messages to the selected notifier components
 * using the OPAL SOS reporter callback interface.
 *
 * @param file
 * @param line
 * @param func
 * @param opal_error
 *
 * @return encoded error code
 */
OPAL_DECLSPEC int opal_sos_reporter(const char *file, int line, const char *func, 
                                    opal_sos_severity_t severity, 
                                    opal_sos_error_t *opal_error);

/**
 * Prints the error encoded by the error number \c errnum
 *
 * @param errnum
 * @param show_history
 *
 */
OPAL_DECLSPEC void opal_sos_print(int errnum, bool show_history);

OPAL_DECLSPEC int opal_sos_prettify_error(const char *error, char **pretty_error);

/**
 * Prints a single error represented by the OPAL SOS error object
 * opal_sos_error_t.
 */
OPAL_DECLSPEC void opal_sos_print_error(opal_sos_severity_t severity, 
                                        int errnum, const char *errmsg, ...)
                                        __opal_attribute_format_funcptr__(__printf__, 3, 4);

/**
 * Frees the error object represented by the error code \c errnum.
 */
OPAL_DECLSPEC void opal_sos_free(int *errnum);

/**
 * Logs (prints and frees) the error object represented by \c errnum.
 */
OPAL_DECLSPEC void opal_sos_log(int errnum);

/**
 * Returns the OPAL SOS severity level as a string.
 *
 */
const char *opal_sos_severity2str(opal_sos_severity_t severity);

/** 
 * \internal
 * Return a unique key into the hash table (opal_sos_error_table)
 * depending on the type and location of the error.
 * 
 */
int opal_sos_hash_error(opal_sos_error_t *error);

/**
 * Registers a print callback function for OPAL_SOS_PRINT()
 */
OPAL_DECLSPEC int 
opal_sos_reg_print_callback(opal_sos_print_callback_fn_t new_func,
                            opal_sos_print_callback_fn_t *prev_func);

/**
 * Registers a reporter callback function for OPAL_SOS_INFO(), 
 * OPAL_SOS_WARN() and OPAL_SOS_ERROR()
 */
OPAL_DECLSPEC int 
opal_sos_reg_reporter_callback(opal_sos_reporter_callback_fn_t new_func,
                               opal_sos_reporter_callback_fn_t *prev_func);

END_C_DECLS

#endif /* OPAL_SOS_H */
