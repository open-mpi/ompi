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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/util/output.h"

/* A bunch of wrappers around lower-level OPAL functions -- need no
   ORTE infrastructure */

int orte_output_reopen(int output_id, opal_output_stream_t *lds)
{
    /* this function just acts as a wrapper around the
     * corresponding opal_output fn
     */
    return opal_output_reopen(output_id, lds);
}

bool orte_output_switch(int output_id, bool enable)
{
    /* this function just acts as a wrapper around the
     * corresponding opal_output fn
     */
    return opal_output_switch(output_id, enable);
}

void orte_output_reopen_all(void)
{
    /* this function just acts as a wrapper around the
     * corresponding opal_output fn
     */
    opal_output_reopen_all();
}

void orte_output_set_verbosity(int output_id, int level)
{
    /* this function just acts as a wrapper around the
     * corresponding opal_output fn
     */
    opal_output_set_verbosity(output_id, level);
}

int orte_output_get_verbosity(int output_id)
{
    /* this function just acts as a wrapper around the
     * corresponding opal_output fn
     */
    return opal_output_get_verbosity(output_id);
}


/************************************************************************/

/* Section for systems without RML and/or HNP support (e.g., Cray) --
   just output directly; don't do any fancy RML sending to the HNP. */
#if ORTE_DISABLE_FULL_SUPPORT

static int stderr_stream = -1;

int orte_output_init(void)
{
    stderr_stream = opal_output_open(NULL);
    regiester_mca();
    if (0 == ORTE_PROC_MY_NAME->vpid && orte_help_want_aggregate) {
        orte_output(stderr_stream, "WARNING: orte_base_help_aggregate was set to true, but this system does not support help message aggregation");
    }
    return ORTE_SUCCESS;
}

void orte_output_finalize(void)
{
    opal_output_close(stderr_stream);
    return;
}

int orte_output_open(opal_output_stream_t *lds)
{
    return opal_output_open(lds);
}

void orte_output(int output_id, const char *format, ...)
{
    /* just call opal_output_vverbose with a verbosity of 0 */
    va_list arglist;
    va_start(arglist, format);
    opal_output_vverbose(0, output_id, format, arglist);
    va_end(arglist);
}

void orte_output_verbose(int verbose_level, int output_id, const char *format, ...)
{
    /* just call opal_output_verbose with the specified verbosity */
    va_list arglist;
    va_start(arglist, format);
    opal_output_vverbose(verbose_level, output_id, format, arglist);
    va_end(arglist);
}

void orte_output_close(int output_id)
{
    opal_output_close(output_id);
}


int orte_show_help(const char *filename, const char *topic, 
                   bool want_error_header, ...)
{
    va_list arglist;
    char *output;
    
    va_start(arglist, want_error_header);
    output = opal_show_help_vstring(filename, topic, want_error_header, 
                                    arglist);
    va_end(arglist);
    
    /* If nothing came back, there's nothing to do */
    if (NULL == output) {
        return ORTE_SUCCESS;
    }
    
    opal_output(stderr_stream, output);
    return ORTE_SUCCESS;
}


#else

/************************************************************************/

/* Section for systems that have full RML/HNP support */

/* defines used solely internal to orte_output */
#define ORTE_OUTPUT_OTHER       0x00
#define ORTE_OUTPUT_STDOUT      0x01
#define ORTE_OUTPUT_STDERR      0x02
#define ORTE_OUTPUT_SHOW_HELP   0x04

#define ORTE_OUTPUT_MAX_TAGS    10

/* List items for holding process names */
typedef struct {
    opal_list_item_t super;
    /* The process name */
    orte_process_name_t pnli_name;
} process_name_list_item_t;

static void process_name_list_item_constructor(process_name_list_item_t *obj);
OBJ_CLASS_INSTANCE(process_name_list_item_t, opal_list_item_t,
                   process_name_list_item_constructor, NULL);

/* List items for holding (filename, topic) tuples */
typedef struct {
    opal_list_item_t super;
    /* The filename */
    char *tli_filename;
    /* The topic */
    char *tli_topic;
    /* List of process names that have displayed this (filename, topic) */
    opal_list_t tli_processes;
    /* Time this message was displayed */
    time_t tli_time_displayed;
    /* Count of processes since last display (i.e., "new" processes
       that have showed this message that have not yet been output) */
    int tli_count_since_last_display;
} tuple_list_item_t;

static void tuple_list_item_constructor(tuple_list_item_t *obj);
static void tuple_list_item_destructor(tuple_list_item_t *obj);
OBJ_CLASS_INSTANCE(tuple_list_item_t, opal_list_item_t,
                   tuple_list_item_constructor,
                   tuple_list_item_destructor);


/* List of (filename, topic) tuples that have already been displayed */
static opal_list_t abd_tuples;

/* How long to wait between displaying duplicate show_help notices */
static struct timeval show_help_interval = { 5, 0 };

/* Timer for displaying duplicate help message notices */
time_t show_help_time_last_displayed = 0;
bool show_help_timer_set = false;
static opal_event_t show_help_timer_event;

/* Local static variables */
static int stdout_stream, stderr_stream;
static opal_output_stream_t stdout_lds, stderr_lds, orte_output_default;
static opal_value_array_t orte_output_streams;
static bool orte_output_ready = false;
static bool suppress_warnings = false;
static bool am_inside = false;

static void process_name_list_item_constructor(process_name_list_item_t *obj)
{
    obj->pnli_name = orte_globals_name_invalid;
}

static void tuple_list_item_constructor(tuple_list_item_t *obj)
{
    obj->tli_filename = NULL;
    obj->tli_topic = NULL;
    OBJ_CONSTRUCT(&(obj->tli_processes), opal_list_t);
    obj->tli_time_displayed = time(NULL);
    obj->tli_count_since_last_display = 0;
}

static void tuple_list_item_destructor(tuple_list_item_t *obj)
{
    opal_list_item_t *item, *next;

    if (NULL != obj->tli_filename) {
        free(obj->tli_filename);
    }
    if (NULL != obj->tli_topic) {
        free(obj->tli_topic);
    }
    for (item = opal_list_get_first(&(obj->tli_processes)); 
         opal_list_get_end(&(obj->tli_processes)) != item;
         item = next) {
        next = opal_list_get_next(item);
        opal_list_remove_item(&(obj->tli_processes), item);
        OBJ_RELEASE(item);
    }
}

/*
 * Check to see if a given (filename, topic) tuple has been displayed
 * already.  Return ORTE_SUCCESS if so, or ORTE_ERR_NOT_FOUND if not.
 *
 * Always return a tuple_list_item_t representing this (filename,
 * topic) entry in the list of "already been displayed tuples" (if it
 * wasn't in the list already, this function will create a new entry
 * in the list and return it).
 *
 * Note that a list is not an overly-efficient mechanism for this kind
 * of data.  The assupmtion is that there will only be a small numebr
 * of (filename, topic) tuples displayed so the storage required will
 * be fairly small, and linear searches will be fast enough.
 */
static int get_tli(const char *filename, const char *topic,
                   tuple_list_item_t **tli)
{
    opal_list_item_t *item;

    /* Search the list for a duplicate. */
    for (item = opal_list_get_first(&abd_tuples); 
         opal_list_get_end(&abd_tuples) != item;
         item = opal_list_get_next(item)) {
        (*tli) = (tuple_list_item_t*) item;
        if (0 == strcmp((*tli)->tli_filename, filename) &&
            0 == strcmp((*tli)->tli_topic, topic)) {
            return ORTE_SUCCESS;
        }
    }

    /* Nope, we didn't find it -- make a new one */
    *tli = OBJ_NEW(tuple_list_item_t);
    if (NULL == *tli) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (*tli)->tli_filename = strdup(filename);
    (*tli)->tli_topic = strdup(topic);
    opal_list_append(&abd_tuples, &((*tli)->super));
    return ORTE_ERR_NOT_FOUND;
}


static void output_vverbose(int verbose_level, int output_id,
                            int major_id, int minor_id,
                            const char *format, va_list arglist)
{
    char *output = NULL;
    opal_buffer_t buf;
    uint8_t flags;
    int rc;

    if (output_id < 0) {
        /* to be ignored */
        return;
    }

    /* If we were called before this subsystem was initialized (or
       after it was finalized), then just pass it down to
       opal_output().  What else can we do?  Shrug. */
    if (!orte_output_ready) {
        opal_output_vverbose(verbose_level, output_id, format, arglist);
        return;
    }

    /* Render the string.  If we get nothing back, there's nothing to
       do (e.g., verbose level was too high) */
    output = opal_output_vstring(verbose_level, output_id, format, arglist);
    if (NULL == output) {
        return;
    }

    /* Per a soon-to-be-filed trac ticket: because this function calls
       RML send, recursion is possible in two places:

       1. RML send itself calls orte_output()
       2. RML send can call progress, which might call something which
          calls orte_output

       So how to avoid the infinite loop?  #1 is more of an issue than
       #2, but it still *could* happen that #2 could cause infinit
       recursion.  It is not practical for RML send (and anything that
       it calls) to use opal_output instead of orte_output.

       We have some ideas how to avoid the recursion, but for the sake
       of getting this working, we're just going to opal_output the
       message for now.  Hence, the developer/user will always see the
       message, but tools who are parsing different channels for the
       output may see this message on the "wrong channel" (e.g., in
       the MPI process' stdout instead of the special channel from the
       HNP).
    */
    if (am_inside) {
        OPAL_OUTPUT_VERBOSE((20, orte_debug_output,
                             "%s orte_output recursion detected",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        opal_output(output_id, output);
        goto cleanup;
    }
    am_inside = true;
    
    /* if I am the HNP, then I need to just pass this on to the
     * opal_output_verbose function using the provided stream
     */
    if (orte_process_info.hnp) {
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                             "%s output to stream %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             output_id));
        opal_output(output_id, output);
        goto cleanup;
    }

    /* lookup the flags for this stream */
    flags = OPAL_VALUE_ARRAY_GET_ITEM(&orte_output_streams, uint8_t, output_id);
    
    /* If there's other flags besides STDOUT and STDERR set, then also
       output this locally via opal_output */
    if ((~(ORTE_OUTPUT_STDOUT | ORTE_OUTPUT_STDERR)) & flags) {
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                             "%s locally output to stream %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             output_id));
        /* pass the values to opal_output for local handling */
        opal_output(output_id, output);
    }

    /* We only relay stdout/stderr to the HNP.  Note that it is
       important to do the RML send last in this function -- do not
       move it earlier!  Putting it last ensures that we keep the same
       relative ordering of output from local calls to opal_output
       (e.g., for syslog or file, above) as the RML sends to the
       HNP. */
    if (ORTE_OUTPUT_STDOUT & flags || ORTE_OUTPUT_STDERR & flags) {
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                             "%s sending output \'%s\' from stream %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             output, output_id));

        /* If RML is not yet setup, or we haven't yet defined the HNP,
         * then just output this locally.
         * What else can we do?
         */
        if (NULL == orte_rml.send_buffer ||
            ORTE_PROC_MY_HNP->vpid == ORTE_VPID_INVALID) {
            opal_output(0, output);
        } else {
            /* setup a buffer to send to the HNP */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            /* pack a flag indicating the output channel */
            opal_dss.pack(&buf, &flags, 1, OPAL_UINT8);
            /* pack the string */
            opal_dss.pack(&buf, &output, 1, OPAL_STRING);
            /* send to the HNP */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, 
                                               ORTE_RML_TAG_OUTPUT, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            OBJ_DESTRUCT(&buf);
            am_inside = false;
        }
    }

 cleanup:
    if (NULL != output) {
        free(output);
    }
}

static void show_accumulated_duplicates(int fd, short event, void *context)
{
    opal_list_item_t *item;
    time_t now = time(NULL);
    tuple_list_item_t *tli;

    /* Loop through all the messages we've displayed and see if any
       processes have sent duplicates that have not yet been displayed
       yet */
    for (item = opal_list_get_first(&abd_tuples); 
         opal_list_get_end(&abd_tuples) != item;
         item = opal_list_get_next(item)) {
        tli = (tuple_list_item_t*) item;
        if (tli->tli_count_since_last_display > 0) {
            orte_output(stderr_stream,
                        "%d more process%s sent help message %s / %s",
                        tli->tli_count_since_last_display,
                        (tli->tli_count_since_last_display > 1) ? "es have" : " has",
                        tli->tli_filename, tli->tli_topic);
            tli->tli_count_since_last_display = 0;
        }
    }

    show_help_time_last_displayed = now;
    show_help_timer_set = false;
}

static int show_help(const char *filename, const char *topic,
                     const char *output, orte_process_name_t *sender)
{
    int rc;
    tuple_list_item_t *tli;
    process_name_list_item_t *pnli;
    time_t now = time(NULL);

    /* If we're aggregating, check for duplicates.  Otherwise, don't
       track duplicates at all and always display the message. */
    if (orte_output_ready && orte_help_want_aggregate) {
        rc = get_tli(filename, topic, &tli);
    } else {
        rc = ORTE_ERR_NOT_FOUND;
    }

    /* Was it already displayed? */
    if (ORTE_SUCCESS == rc) {
        /* Yes.  But do we want to print anything?  That's complicated.

           We always show the first message of a given (filename,
           topic) tuple as soon as it arrives.  But we don't want to
           show duplicate notices often, because we could get overrun
           with them.  So we want to gather them up and say "We got N
           duplicates" every once in a while.

           And keep in mind that at termination, we'll unconditionally
           show all accumulated duplicate notices.

           A simple scheme is as follows:
           - when the first of a (filename, topic) tuple arrives
             - print the message
             - if a timer is not set, set T=now
           - when a duplicate (filename, topic) tuple arrives
             - if now>(T+5) and timer is not set (due to
               non-pre-emptiveness of our libevent, a timer *could* be
               set!)
               - print all accumulated duplicates
               - reset T=now
             - else if a timer was not set, set the timer for T+5
             - else if a timer was set, do nothing (just wait)
           - set T=now when the timer expires
        */           
        ++tli->tli_count_since_last_display;
        if (now > show_help_time_last_displayed + 5 && !show_help_timer_set) {
            show_accumulated_duplicates(0, 0, NULL);
        } else if (!show_help_timer_set) {
            opal_evtimer_set(&show_help_timer_event,
                             show_accumulated_duplicates, NULL);
            opal_evtimer_add(&show_help_timer_event, &show_help_interval);
            show_help_timer_set = true;
        }
    } 
    /* Not already displayed */
    else if (ORTE_ERR_NOT_FOUND == rc) {
        orte_output(stderr_stream, output);
        if (!show_help_timer_set) {
            show_help_time_last_displayed = now;
        }
    }
    /* Some other error occurred */
    else {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* If we're aggregating, add this process name to the list */
    if (orte_output_ready && orte_help_want_aggregate) {
        pnli = OBJ_NEW(process_name_list_item_t);
        if (NULL == pnli) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        pnli->pnli_name = *sender;
        opal_list_append(&(tli->tli_processes), &(pnli->super));
    }
    return ORTE_SUCCESS;
}


/* Note that this function is called from ess/hnp, so don't make it
   static */
void orte_output_recv_output(int status, orte_process_name_t* sender,
                             opal_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    char *output=NULL;
    char *filename=NULL, *topic=NULL;
    uint8_t flag;
    int32_t n;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s got output from sender %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* unpack the flag indicating the output stream */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &flag, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if the flag is from show_help... */
    if (ORTE_OUTPUT_SHOW_HELP & flag) {        
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                             "%s got output from show_help",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* unpack the filename of the show_help text file */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &filename, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* unpack the topic tag */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &topic, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* unpack the resulting string */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &output, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* Send it to show_help */
        rc = show_help(filename, topic, output, sender);
    } else {
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                             "%s got output from stdout/err",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* this must be from stdout or stderr - get the string */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &output, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* output via appropriate channel */
        if (ORTE_OUTPUT_STDOUT & flag) {
            OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                                 "%s output %s to stdout",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 output));
            opal_output(stdout_stream, output);
        } else {
            OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                                 "%s output %s to stderr",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 output));
            opal_output(stderr_stream, output);
        }
    }
    
cleanup:
    if (NULL != output) {
        free(output);
    }
    if (NULL != filename) {
        free(filename);
    }
    if (NULL != topic) {
        free(topic);
    }
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_OUTPUT,
                                 ORTE_RML_NON_PERSISTENT, orte_output_recv_output, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
    }
}

int orte_output_init(void)
{
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output, "orte_output init"));

    /* define the default stream that has everything off */
    OBJ_CONSTRUCT(&orte_output_default, opal_output_stream_t);

    /* Show help duplicate detection */
    OBJ_CONSTRUCT(&abd_tuples, opal_list_t);

    /* setup array to track what output streams are
     * going to stdout and/or stderr
     */
    OBJ_CONSTRUCT(&orte_output_streams, opal_value_array_t);
    opal_value_array_init(&orte_output_streams, sizeof(uint8_t));
    
    /* reserve places for 50 streams - the array will
     * resize above that if required
     */
    opal_value_array_reserve(&orte_output_streams, 50);
    /* initialize the 0 position of the array as this
     * corresponds to the automatically-opened stderr
     * stream of opal_output
     */
    OPAL_VALUE_ARRAY_SET_ITEM(&orte_output_streams, uint8_t, 0, ORTE_OUTPUT_STDERR);

    /* if we are on the HNP, we need to open
     * dedicated orte_output streams for stdout/stderr
     * for our use so that we can control their behavior
     */
    if (orte_process_info.hnp) {
        /* setup stdout stream - we construct our own
         * stream object so we can control the behavior
         * for outputting stuff from remote procs
         */
        OBJ_CONSTRUCT(&stdout_lds, opal_output_stream_t);
        /* deliver to stdout only */
        stdout_lds.lds_want_stdout = true;
        stdout_stream = opal_output_open(&stdout_lds);
        OPAL_VALUE_ARRAY_SET_ITEM(&orte_output_streams, uint8_t, stdout_stream, ORTE_OUTPUT_STDOUT);
        /* setup stderr stream - we construct our own
         * stream object so we can control the behavior
         */
        OBJ_CONSTRUCT(&stderr_lds, opal_output_stream_t);
        /* deliver to stderr only */
        stderr_lds.lds_want_stderr = true;
        /* we filter the stderr */
        stderr_lds.lds_filter_flags = OPAL_OUTPUT_FILTER_STDERR;
        stderr_stream = opal_output_open(&stderr_lds);
        OPAL_VALUE_ARRAY_SET_ITEM(&orte_output_streams, uint8_t, stderr_stream, ORTE_OUTPUT_STDERR);
    }
    
    orte_output_ready = true;
    return ORTE_SUCCESS;
}

void orte_output_finalize(void)
{
    if (!orte_output_ready) {
        return;
    }

    /* Shutdown show_help, showing final messages */
    if (orte_process_info.hnp) {
        show_accumulated_duplicates(0, 0, NULL);
    }
    OBJ_DESTRUCT(&abd_tuples);
    if (show_help_timer_set) {
        opal_evtimer_del(&show_help_timer_event);
    }

    orte_output_ready = false;

    /* if we are the HNP, cancel the recv */
    if (orte_process_info.hnp) {
        OBJ_DESTRUCT(&orte_output_default);
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_OUTPUT);
        /* close our output streams */
        opal_output_close(stdout_stream);
        opal_output_close(stderr_stream);
        OBJ_DESTRUCT(&stdout_lds);
        OBJ_DESTRUCT(&stderr_lds);
        return;
    }

    /* if we are not the HNP, we just need to
     * cleanup our tracking array
     *
     * NOTE: we specifically do NOT call opal_output_close
     * on these streams! It may be that additional output
     * will be called after we close orte_output, and we
     * need to retain an ability to get those messages out.
     */
    OBJ_DESTRUCT(&orte_output_streams);
}

/* NOTE: HOW WE HANDLE THE ERROR CASE OF SOMEONE
 * CALLING AN ORTE_OUTPUT FUNCTION PRIOR TO CALLING
 * ORTE_OUTPUT_INIT
 *
 * if we are not finalizing, then this was called prior
 * to the normal init. Despite multiple attempts, we
 * haven't really found a good solution to this problem.
 * We could call orte_output_init to setup our own
 * internal tracking system, but that won't open up
 * an associated opal_output stream for this output_id!
 * Accordingly, opal_output will simply ignore anything
 * being output to output_id - with the caller none-the-wiser
 * that this is happening!
 *
 * After discussion, RHC and JMS decided that orte_output_init
 * comes -so- early in the orte_init procedure that the only
 * way this error can occur is for someone to call orte_output
 * PRIOR to calling orte_init. This is an obvious error, so the
 * main concern here is to avoid segfaulting. We therefore print
 * out an error message so the caller knows what happened, use
 * opal_output to let them see the error message (if possible),
 * and do nothing else
 */

int orte_output_open(opal_output_stream_t *lds)
{
    int stream;
    uint8_t flag = ORTE_OUTPUT_OTHER;
    
    if (!orte_output_ready) {
        /* see above discussion on how we handle this error */
        fprintf(stderr, "A call was made to orte_output_open %s\n",
                orte_finalizing ? "during or after calling orte_finalize" : "prior to calling orte_init");
        return ORTE_ERROR;
    }
    
    /* if we are the HNP, this function just acts as
     * a wrapper around the corresponding opal_output fn
     */
    if (orte_process_info.hnp) {
        stream = opal_output_open(lds);
        OPAL_OUTPUT_VERBOSE((5, orte_debug_output, "HNP opened stream %d", stream));
        goto track;
   }
    
    /* if we not the HNP, then we need to open the stream
     * and also record whether or not it is sending
     * output to stdout/stderr
     */
    if (NULL == lds) {
        /* we have to ensure that the opal_output stream
         * doesn't open stdout and stderr, so setup the
         * stream here and ensure the settings are
         * correct - otherwise, opal_output will default
         * the stream to having stderr active!
         */
        lds = &orte_output_default;
        flag = ORTE_OUTPUT_STDERR;
    } else {
        /* does this stream involve stdout? */
        if (lds->lds_want_stdout) {
            flag |= ORTE_OUTPUT_STDOUT;
            lds->lds_want_stdout = false;
        }
        /* does it involve stderr? */
        if (lds->lds_want_stderr) {
            flag |= ORTE_OUTPUT_STDERR;
            lds->lds_want_stderr = false;
        }
    }
    /* open the stream */
    stream = opal_output_open(lds);

    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s opened stream %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         stream));
    
track:
    /* track the settings - can't use macro as this may adjust size of array */
    opal_value_array_set_item(&orte_output_streams, stream, (void*)&flag);
    
    return stream;
}

void orte_output(int output_id, const char *format, ...)
{
    va_list arglist;
    char *output;
    
    if (!orte_output_ready) {
        /* if we are finalizing, then we have no way to process
         * this through the orte_output system - just drop it to
         * opal_output for handling
         */
        if (orte_finalizing) {
            va_start(arglist, format);
            opal_output_vverbose(0, output_id, format, arglist);
            return;
        } else {
            /* see above discussion as to how we handle this error */
            va_start(arglist, format);
            if (!suppress_warnings) {
                fprintf(stderr, "A call was made to orte_output prior to calling orte_init\n");
                fprintf(stderr, "The offending message is:\n");
            }
            output = opal_output_vstring(0, 0, format, arglist);
            fprintf(stderr, "%s\n", (NULL == output) ? "NULL" : output);
            if (NULL != output) free(output);
            return;
        }
    }
    
    /* just call output_vverbose with a verbosity of 0 */
    va_start(arglist, format);
    output_vverbose(0, output_id, ORTE_PROC_MY_NAME->jobid, ORTE_PROC_MY_NAME->vpid, format, arglist);
    va_end(arglist);
}

void orte_output_verbose(int verbose_level, int output_id, const char *format, ...)
{
    va_list arglist;
    char *output;

    if (!orte_output_ready) {
        /* if we are finalizing, then we have no way to process
         * this through the orte_output system - just drop it to
         * opal_output for handling
         */
        if (orte_finalizing) {
            va_start(arglist, format);
            opal_output_vverbose(verbose_level, output_id, format, arglist);
            return;
        } else {
            /* if we are not finalizing, then this was called prior
             * to the normal init - see above discussion as to
             * how this is being handled
             */
            va_start(arglist, format);
            if (!suppress_warnings) {
                fprintf(stderr, "A call was made to orte_output_verbose prior to calling orte_init\n");
                fprintf(stderr, "The offending message (verbosity=%d) is:\n", verbose_level);
            }
            output = opal_output_vstring(0, 0, format, arglist);
            fprintf(stderr, "%s\n", (NULL == output) ? "NULL" : output);
            if (NULL != output) free(output);
        }
    }
    
    /* just call output_verbose with the specified verbosity */
    va_start(arglist, format);
    output_vverbose(verbose_level, output_id, ORTE_PROC_MY_NAME->jobid, ORTE_PROC_MY_NAME->vpid, format, arglist);
    va_end(arglist);
}

void orte_output_close(int output_id)
{    
    if (!orte_output_ready && !orte_finalizing) {
        /* if we are finalizing, then we really don't want
         * to init this system - otherwise, this was called prior
         * to the normal init, see above discussion as to
         * how this was handled
         */
        fprintf(stderr, "A call was made to orte_output_close prior to calling orte_init\n");
        return;
    }
    
    /* cleanout the stream settings */
    OPAL_VALUE_ARRAY_SET_ITEM(&orte_output_streams, uint8_t, output_id, ORTE_OUTPUT_OTHER);
    opal_output_close(output_id);
}


int orte_show_help(const char *filename, const char *topic, 
                   bool want_error_header, ...)
{
    int rc = ORTE_SUCCESS;
    va_list arglist;
    char *output;
    
    va_start(arglist, want_error_header);
    output = opal_show_help_vstring(filename, topic, want_error_header, 
                                    arglist);
    va_end(arglist);

    /* If nothing came back, there's nothing to do */
    if (NULL == output) {
        return ORTE_SUCCESS;
    }

    if (!orte_output_ready) {
        /* if we are finalizing, then we have no way to process
         * this through the orte_output system - just drop it to
         * opal_show_help for handling
         *
         * If we are not finalizing, then this is probably a show_help
         * stemming from either a cmd-line request to display the usage
         * message, or a show_help related to a user error. In either case,
         * we can't do anything but just call opal_show_help
         *
         * Ensure we suppress the orte_output warnings for this case, then
         * re-enable them when we are done
         */
        suppress_warnings = true;
        rc = show_help(filename, topic, output, ORTE_PROC_MY_NAME);
        suppress_warnings = false;
        goto CLEANUP;
    }
    
    /* if we are the HNP, or the RML has not yet been setup,
     * or we don't yet know our HNP, then all we can do
     * is process this locally
     */
    if (orte_process_info.hnp ||
        NULL == orte_rml.send_buffer ||
        ORTE_PROC_MY_HNP->vpid == ORTE_VPID_INVALID) {
        rc = show_help(filename, topic, output, ORTE_PROC_MY_NAME);
    }
    
    /* otherwise, we relay the output message to
     * the HNP for processing
     */
    else {
        opal_buffer_t buf;
        uint8_t flag = ORTE_OUTPUT_SHOW_HELP;
        static bool am_inside = false;

        /* JMS Note that we *may* have a similar recursion situation
           as with output_vverbose(), above.  Need to think about this
           properly, but put a safeguard in here for sure for the time
           being. */
        if (am_inside) {
            rc = show_help(filename, topic, output, ORTE_PROC_MY_NAME);
        } else {
            am_inside = true;
        
            /* build the message to the HNP */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            /* pack the flag indicating this is from show_help */
            opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
            /* pack the filename of the show_help text file */
            opal_dss.pack(&buf, &filename, 1, OPAL_STRING);
            /* pack the topic tag */
            opal_dss.pack(&buf, &topic, 1, OPAL_STRING);
            /* pack the resulting string */
            opal_dss.pack(&buf, &output, 1, OPAL_STRING);
            /* send it to the HNP */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_OUTPUT, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            OBJ_DESTRUCT(&buf);
            am_inside = false;
        }
    }
    
CLEANUP:
    free(output);
    return rc;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */

