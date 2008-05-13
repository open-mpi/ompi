/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _ORTE_OUTPUT_H_
#define _ORTE_OUTPUT_H_

#include "orte_config.h"
#include "orte/types.h"

#include "opal/util/output.h"

#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

/**
 * Initializes the output stream system and opens a default
 * "verbose" stream.
 *
 * @retval success Upon success.
 * @retval error Upon failure.
 *
 * This should be the first function invoked in the output
 * subsystem.  After this call, the default "verbose" stream is open
 * and can be written to via calls to orte_output_verbose() and
 * orte_output_error().
 *
 * By definition, the default verbose stream has a handle ID of 0,
 * and has a verbose level of 0.
 */
ORTE_DECLSPEC int orte_output_init(void);

/**
 * Shut down the output stream system.
 *
 * Shut down the output stream system, including the default verbose
 * stream.
 */
ORTE_DECLSPEC void orte_output_finalize(void);

/**
 * Opens an output stream.
 *
 * @param lds A pointer to opal_output_stream_t describing what the
 * characteristics of the output stream should be.
 *
 * This function opens an output stream and returns an integer
 * handle.  The caller is responsible for maintaining the handle and
 * using it in successive calls to ORTE_OUTPUT(), orte_output(),
 * orte_output_switch(), and orte_output_close().
 *
 * If lds is NULL, the default descriptions will be used, meaning
 * that output will only be sent to stderr.
 *
 * It is safe to have multiple threads invoke this function
 * simultaneously; their execution will be serialized in an
 * unspecified manner.
 *
 * Be sure to see orte_output() for a description of how
 * the various streams work.
 *
 * In addition, the caller is required to provide at least one string "tag"
 * describing the expected content to come out of this stream. For example,
 * the RMAPS framework will open two streams:
 *
 * debug_stream = orte_output_open(NULL, "RMAPS", "debug")
 * display_map_stream = orte_output_open(NULL, "RMAPS", "map");
 *
 * The first stream will be used by all debugging output according to
 * verbosity. The second stream is used solely in those cases where
 * the user has requested --display-map. Thus, subsequent filters on
 * the output can know and appropriately tag the content being output
 * through the various streams
 *
 */
ORTE_DECLSPEC int orte_output_open(opal_output_stream_t *lds, const char *primary_tag, ...);

/**
 * Re-opens / redirects an output stream.
 *
 * @param output_id Stream handle to reopen
 * @param lds A pointer to opal_output_stream_t describing what the
 * characteristics of the reopened output stream should be.
 *
 * This function redirects an existing stream into a new [set of]
 * location[s], as specified by the lds parameter.  If the output_is
 * passed is invalid, this call is effectively the same as opening a
 * new stream with a specific stream handle.
 */
ORTE_DECLSPEC int orte_output_reopen(int output_id, opal_output_stream_t *lds);

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
 * enable value to false, and later resumed by passing an enable
 * value of true.  This does not close the stream -- it simply tells
 * the orte_output subsystem to intercept and discard any output sent
 * to the stream via ORTE_OUTPUT() or orte_output() until the output
 * is re-enabled.
 */
ORTE_DECLSPEC bool orte_output_switch(int output_id, bool enable);

/**
 * \internal
 *
 * Reopens all existing output streams.
 *
 * This function should never be called by user applications; it is
 * typically only invoked after a restart (i.e., in a new process)
 * where output streams need to be re-initialized.
 */
ORTE_DECLSPEC void orte_output_reopen_all(void);

/**
 * Close an output stream.
 *
 * @param output_id Handle of the stream to close.
 *
 * Close an output stream.  No output will be sent to the stream
 * after it is closed.  Be aware that output handles tend to be
 * re-used; it is possible that after a stream is closed, if another
 * stream is opened, it will get the same handle value.
 */
ORTE_DECLSPEC void orte_output_close(int output_id);

/**
 * Main function to send output to a stream.
 *
 * @param output_id Stream id returned from orte_output_open().
 * @param format printf-style format string.
 * @param varargs printf-style varargs list to fill the string
 * specified by the format parameter.
 *
 * This is the main function to send output to custom streams (note
 * that output to the default "verbose" stream is handled through
 * orte_output_verbose() and orte_output_error()).
 *
 * It is never necessary to send a trailing "\n" in the strings to
 * this function; some streams requires newlines, others do not --
 * this function will append newlines as necessary.
 *
 * Verbosity levels are ignored in this function.
 *
 * Note that for output streams that are directed to files, the
 * files are stored under the process' session directory.  If the
 * session directory does not exist when orte_output() is invoked,
 * the output will be discarded!  Once the session directory is
 * created, orte_output() will automatically create the file and
 * writing to it.
 */
ORTE_DECLSPEC void orte_output(int output_id, const char *format, ...)
    __opal_attribute_format__(__printf__, 2, 3);

/**
 * Send output to a stream only if the passed verbosity level is
 * high enough.
 *
 * @param output_id Stream id returned from orte_output_open().
 * @param level Target verbosity level.
 * @param format printf-style format string.
 * @param varargs printf-style varargs list to fill the string
 * specified by the format parameter.
 *
 * Output is only sent to the stream if the current verbosity level
 * is greater than or equal to the level parameter.  This mechanism
 * can be used to send "information" kinds of output to user
 * applications, but only when the user has asked for a high enough
 * verbosity level.
 *
 * It is never necessary to send a trailing "\n" in the strings to
 * this function; some streams requires newlines, others do not --
 * this function will append newlines as necessary.
 *
 * This function is really a convenience wrapper around checking the
 * current verbosity level set on the stream, and if the passed
 * level is less than or equal to the stream's verbosity level, this
 * function will effectively invoke orte_output to send the output to
 * the stream.
 *
 * @see orte_output_set_verbosity()
 */
ORTE_DECLSPEC void orte_output_verbose(int verbose_level, int output_id, const char *format, ...)
    __opal_attribute_format__(__printf__, 3, 4);


/**
 * Set the verbosity level for a stream.
 *
 * @param output_id Stream id returned from orte_output_open().
 * @param level New verbosity level
 *
 * This function sets the verbosity level on a given stream.  It
 * will be used for all future invocations of orte_output_verbose().
 */
ORTE_DECLSPEC void orte_output_set_verbosity(int output_id, int level);

/**
 * Get the verbosity level for a stream
 *
 * @param output_id Stream id returned from orte_output_open()
 * @returns Verbosity of stream
 */
ORTE_DECLSPEC int orte_output_get_verbosity(int output_id);


/**
 * Show help
 *
 * Sends show help messages to the HNP if on a backend node
 */
ORTE_DECLSPEC int orte_show_help(const char *filename, const char *topic, 
                                 bool want_error_header, ...);

#if ORTE_DISABLE_FULL_SUPPORT

#if OMPI_ENABLE_DEBUG
/**
 * Main macro for use in sending debugging output to output streams;
 * will be "compiled out" when ORTE is configured without
 * --enable-debug.
 *
 * @see orte_output()
 */
#define ORTE_OUTPUT(a) opal_output a

/** 
 * Macro for use in sending debugging output to the output
 * streams.  Will be "compiled out" when ORTE is configured
 * without --enable-debug.
 *
 * @see orte_output_verbose()
 */
#define ORTE_OUTPUT_VERBOSE(a) opal_output_verbose a
#else
/**
 * Main macro for use in sending debugging output to output streams;
 * will be "compiled out" when ORTE is configured without
 * --enable-debug.
 *
 * @see orte_output()
 */
#define ORTE_OUTPUT(a)

/** 
 * Macro for use in sending debugging output to the output
 * streams.  Will be "compiled out" when ORTE is configured
 * without --enable-debug.
 *
 * @see orte_output_verbose()
 */
#define ORTE_OUTPUT_VERBOSE(a)
#endif

#else

ORTE_DECLSPEC void orte_output_recv_output(int status, orte_process_name_t* sender,
                                           opal_buffer_t *buffer, orte_rml_tag_t tag,
                                           void* cbdata);

#if OMPI_ENABLE_DEBUG
/**
 * Main macro for use in sending debugging output to output streams;
 * will be "compiled out" when ORTE is configured without
 * --enable-debug.
 *
 * @see orte_output()
 */
#define ORTE_OUTPUT(a) orte_output a

/** 
 * Macro for use in sending debugging output to the output
 * streams.  Will be "compiled out" when ORTE is configured
 * without --enable-debug.
 *
 * @see orte_output_verbose()
 */
#define ORTE_OUTPUT_VERBOSE(a) orte_output_verbose a
#else
/**
 * Main macro for use in sending debugging output to output streams;
 * will be "compiled out" when ORTE is configured without
 * --enable-debug.
 *
 * @see orte_output()
 */
#define ORTE_OUTPUT(a)

/** 
 * Macro for use in sending debugging output to the output
 * streams.  Will be "compiled out" when ORTE is configured
 * without --enable-debug.
 *
 * @see orte_output_verbose()
 */
#define ORTE_OUTPUT_VERBOSE(a)
#endif

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
