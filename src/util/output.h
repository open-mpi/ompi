/*
 * $HEADER$
 */

/** @file
 * LAM output stream facility.
 *
 * The LAM output stream facility is used to send output from the LAM
 * libraries to output devices.  It is meant to fully replace all
 * forms of printf() (and friends).  Output streams are opened via the
 * lam_output_open() function call, and then sent output via
 * lam_output_verbose(), LAM_OUTPUT(), and lam_output().  Streams are
 * closed with lam_output_close().
 *
 * Streams can multiplex output to several kinds of outputs (one of
 * each):
 *
 * - the syslog
 * - standard output
 * - standard error
 * - file
 *
 * Which outputs to use are specified during lam_output_open().
 *
 * lam_output_open() returns an integer handle that is used in
 * successive calls to LAM_OUTPUT() and lam_output() to send output to
 * the stream.
 *
 * The default "verbose" stream is opened after invoking
 * lam_output_init() (and closed after invoking
 * lam_output_finalize()).  This stream outputs to stderr only, and
 * has a stream handle ID of 0.
 *
 * It is erroneous to have one thread close a stream and have another
 * try to write to it.  Multiple threads writing to a single stream
 * will be serialized in an unspecified order.
 */

#ifndef LAM_OUTPUT_H_
#define LAM_OUTPUT_H_

#include "lam_config.h"

#if __STDC__
#include <stdarg.h>
#else
#ifdef __cplusplus
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif /* __STDC__ */


/**
 * \class lam_output_stream_t 
 *
 * Structure used to request the opening of a LAM output stream.  A
 * pointer to this structure is passed to lam_output_open() to tell
 * the lam_output subsystem where to send output for a given stream.
 * It is valid to specify multiple destinations of output for a stream
 * -- output streams can be multiplexed to multiple different
 * destinations through the lam_output facility.
 *
 * Note that all strings in this struct are cached on the stream by
 * value; there is no need to keep them allocated after the return
 * from lam_output_open().
 *
 * @see output.h
 */
struct lam_output_stream_t {
  /**
   * Indicates whether the output of the stream is
   * debugging/developer-only output or not.
   *
   * This field should be "true" if the output is for debugging
   * purposes only.  In that case, the output will never be sent to
   * the stream unless LAM was configured with --enable-debug.
   */
  bool lds_is_debugging;

  /**
   * Indicate the starting verbosity level of the stream.
   *
   * Verbose levels are a convenience mechanisms, and are only
   * consulted when output is sent to a stream through the
   * lam_output_verbose() function.  Verbose levels are ignored in
   * LAM_OUTPUT() and lam_output().
   *
   * Valid verbose levels typically start at 0 (meaning "minimal
   * information").  Higher verbosity levels generally indicate that
   * more output and diagnostics should be displayed.
   */
  int lds_verbose_level;

  /**
   * Indicates whether output of the stream should be sent to the
   * syslog or not.
   *
   * If this field is true, output from this stream is sent to the
   * syslog, and the following fields are also examined:
   *
   * - lds_syslog_priority
   * - lds_syslog_ident
   * - lds_prefix
   *
   * If this field is false, the above three fields are ignored.
   */
  bool lds_want_syslog;
  /**
   * When lam_output_stream_t::lds_want_syslog is true, this field is
   * examined to see what priority output from the stream should be
   * sent to the syslog.
   *
   * This value should be set as per the syslog(3) man page.  It is
   * typically the OR value of "facilty" and "level" values described
   * in the man page.
   */
  int lds_syslog_priority;
  /**
   * When lam_output_stream_t::lds_want_syslog is true, this field is
   * examined to see what ident value should be passed to openlog(3).
   * 
   * If a NULL value is given, the string "lam" is used.
   */
  char *lds_syslog_ident;

  /**
   * String prefix added to all output on the stream.
   *
   * When this field is non-NULL, it is prefixed to all lines of
   * output on the stream.  When this field is NULL, no prefix is
   * added to each line of output in the stream.
   */
  char *lds_prefix;

  /**
   * Whether to send stream output to stdout or not.
   *
   * If this field is true, stream output is sent to stdout.
   */
  bool lds_want_stdout;
  /**
   * Whether to send stream output to stderr or not.
   *
   * If this field is true, stream output is sent to stderr.
   */
  bool lds_want_stderr;

  /**
   * Whether to send stream output to a file or not.
   *
   * When this field is true, stream output is sent to a file, and the
   * following fields are also examined:
   *
   * - lds_want_file_append
   * - lda_file_suffix
   */
  bool lds_want_file;
  /**
   * When lam_output_stream_t::lds_want_file is true, this field
   * indicates whether to append the file (if it exists) or overwrite
   * it.
   *
   * If false, the file is opened with the O_TRUNC flag.
   */
  bool lds_want_file_append;
  /**
   * When lam_output_stream_t::lds_want_file is true, this field
   * indicates the string suffix to add to the filename.
   *
   * The output file will be in the LAM session directory and have a
   * LAM-generated prefix (generally "$sessiondir/lam-").  The suffix
   * is intended to give stream users a chance to write their output
   * into unique files.  If this field is NULL, the suffix
   * "output.txt" is used.
   */
  char *lds_file_suffix;
};

typedef struct lam_output_stream_t lam_output_stream_t;


#ifdef __cplusplus
extern "C" {
#endif
  /**
   * Initializes the output stream system and opens a default
   * "verbose" stream.
   *
   * @retval true Upon success.
   * @retval false Upon failure.
   *
   * This should be the first function invoked in the output
   * subsystem.  After this call, the default "verbose" stream is open
   * and can be written to via calls to lam_output_verbose() and
   * lam_output_error().
   *
   * By definition, the default verbose stream has a handle ID of 0,
   * and has a verbose level of 0.
   */
  bool lam_output_init(void);

  /**
   * Shut down the output stream system.
   *
   * Shut down the output stream system, including the default verbose
   * stream.
   */
  void lam_output_finalize(void);

  /**
   * Opens an output stream.
   *
   * @param lds A pointer to lam_output_stream_t describing what the
   * characteristics of the output stream should be.
   *
   * This function opens an output stream and returns an integer
   * handle.  The caller is responsible for maintaining the handle and
   * using it in successive calls to LAM_OUTPUT(), lam_output(),
   * lam_output_switch(), and lam_output_close().
   *
   * It is safe to have multiple threads invoke this function
   * simultaneously; their execution will be serialized in an
   * unspecified manner.
   */
  int lam_output_open(lam_output_stream_t *lds);

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
  int lam_output_reopen(int output_id, lam_output_stream_t *lds);

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
   * the lam_output subsystem to intercept and discard any output sent
   * to the stream via LAM_OUTPUT() or lam_output() until the output
   * is re-enabled.
   */
  bool lam_output_switch(int output_id, bool enable);

  /**
   * \internal
   *
   * Reopens all existing output streams.
   *
   * This function should never be called by user applications; it is
   * typically only invoked after a restart (i.e., in a new process)
   * where output streams need to be re-initialized.
   */
  void lam_output_reopen_all(void);

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
  void lam_output_close(int output_id);

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
  void lam_output(int output_id, char *format, ...);

  /**
   * Send output to a stream only if the passed verbosity level is
   * high enough.
   *
   * @param output_id Stream id returned from lam_output_open().
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
   * function will effectively invoke lam_output to send the output to
   * the stream.
   *
   * @see lam_output_set_verbosity()
   */
  void lam_output_verbose(int output_id, int verbose_level, char *format, ...);

  /**
   * Set the verbosity level for a stream.
   *
   * @param output_id Stream id returned from lam_output_open().
   * @param level New verbosity level
   *
   * This function sets the verbosity level on a given stream.  It
   * will be used for all future invocations of lam_output_verbose().
   */
  void lam_output_set_verbosity(int output_id, int level);

#if LAM_ENABLE_DEBUG
  /**
   * Main macro for use in sending debugging output to output streams;
   * will be "compiled out" when LAM is configured without
   * --enable-debug.
   *
   * @see lam_output()
   */
#define LAM_OUTPUT(a) lam_output a
#else
  /**
   * Main macro for use in sending debugging output to output streams;
   * will be "compiled out" when LAM is configured without
   * --enable-debug.
   *
   * @see lam_output()
   */
#define LAM_OUTPUT(a)
#endif
#ifdef __cplusplus
}
#endif

#endif /* LAM_OUTPUT_H_ */

