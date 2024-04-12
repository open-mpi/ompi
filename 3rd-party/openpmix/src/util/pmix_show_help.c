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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>

#include "pmix.h"
#include "pmix_common.h"
#include "src/include/pmix_globals.h"
#include "src/mca/pinstalldirs/pinstalldirs.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

bool pmix_show_help_enabled = false;
static time_t show_help_time_last_displayed = 0;
static bool show_help_timer_set = false;
static pmix_event_t show_help_timer_event;
static int output_stream = -1;

static pmix_status_t load_array(char ***array,
                                const char *filename,
                                const char *topic);

/* How long to wait between displaying duplicate show_help notices */
static struct timeval show_help_interval = {5, 0};

static void show_help_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status);

    if (NULL != cd->directives) {
        PMIX_INFO_FREE(cd->directives, cd->ndirs);
    }
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}

static void local_delivery(const char *file,
                           const char *topic,
                           const char *msg)
{
    pmix_shift_caddy_t *cd;

    if (!pmix_show_help_enabled) {
        /* the show help subsystem has not yet been enabled,
         * likely because we haven't gotten far enough thru
         * client/server/tool "init". In this case, we can
         * only output the help locally as we don't have
         * access to anything else */
        fprintf(stderr, "%s", msg);
        return;
    }

    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->ninfo = 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    PMIX_INFO_LOAD(&cd->info[0], PMIX_LOG_STDERR, msg, PMIX_STRING);
    cd->ndirs = 2;
    PMIX_INFO_CREATE(cd->directives, cd->ndirs);
    PMIX_INFO_LOAD(&cd->directives[0], PMIX_LOG_KEY, file, PMIX_STRING);
    PMIX_INFO_LOAD(&cd->directives[1], PMIX_LOG_VAL, topic, PMIX_STRING);
    cd->cbfunc.opcbfn = show_help_cbfunc;
    cd->cbdata = cd;
    cd->proc = NULL;
    PMIX_THREADSHIFT(cd, pmix_log_local_op);
}


/* List items for holding (filename, topic) tuples */
typedef struct {
    pmix_list_item_t super;
    /* The filename */
    char *tli_filename;
    /* The topic */
    char *tli_topic;
    /* List of process names that have displayed this (filename, topic) */
    pmix_list_t tli_processes;
    /* Time this message was displayed */
    time_t tli_time_displayed;
    /* Count of processes since last display (i.e., "new" processes
       that have showed this message that have not yet been output) */
    int tli_count_since_last_display;
    /* Do we want to display these? */
    bool tli_display;
} tuple_list_item_t;

static void tuple_list_item_constructor(tuple_list_item_t *obj)
{
    obj->tli_filename = NULL;
    obj->tli_topic = NULL;
    PMIX_CONSTRUCT(&(obj->tli_processes), pmix_list_t);
    obj->tli_time_displayed = time(NULL);
    obj->tli_count_since_last_display = 0;
    obj->tli_display = true;
}

static void tuple_list_item_destructor(tuple_list_item_t *obj)
{
    if (NULL != obj->tli_filename) {
        free(obj->tli_filename);
    }
    if (NULL != obj->tli_topic) {
        free(obj->tli_topic);
    }
    PMIX_LIST_DESTRUCT(&(obj->tli_processes));
}
static PMIX_CLASS_INSTANCE(tuple_list_item_t, pmix_list_item_t, tuple_list_item_constructor,
                           tuple_list_item_destructor);

/* List of (filename, topic) tuples that have already been displayed */
static pmix_list_t abd_tuples;


/*
 * Private variables
 */
static const char *default_filename = "help-messages";
static const char *dash_line
    = "--------------------------------------------------------------------------\n";
static char **search_dirs = NULL;

static pmix_status_t match(const char *a, const char *b)
{
    int rc = PMIX_ERROR; 
    char *p1, *p2, *tmp1 = NULL, *tmp2 = NULL;
    size_t min;

    /* Check straight string match first */
    if (0 == strcmp(a, b))
        return PMIX_SUCCESS;

    if (NULL != strchr(a, '*') || NULL != strchr(b, '*')) {
        tmp1 = strdup(a); 
        if (NULL == tmp1) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        tmp2 = strdup(b); 
        if (NULL == tmp2) {
            free(tmp1);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        p1 = strchr(tmp1, '*');
        p2 = strchr(tmp2, '*');

        if (NULL != p1) {
            *p1 = '\0';
        }
        if (NULL != p2) {
            *p2 = '\0';
        }
        min = strlen(tmp1);
        if (strlen(tmp2) < min) {
            min = strlen(tmp2);
        }
        if (0 == min || 0 == strncmp(tmp1, tmp2, min)) {
            rc = PMIX_SUCCESS;
        }
        free(tmp1);
        free(tmp2);
        return rc;
    }

    /* No match */
    return PMIX_ERROR;
}


static pmix_status_t pmix_get_tli(const char *filename,
                                  const char *topic,
                                  tuple_list_item_t **tli_)
{
    tuple_list_item_t *tli = *tli_;

    /* Search the list for a duplicate. */
    PMIX_LIST_FOREACH(tli, &abd_tuples, tuple_list_item_t)
    {
        if (PMIX_SUCCESS == match(tli->tli_filename, filename) &&
            PMIX_SUCCESS == match(tli->tli_topic, topic)) {
            *tli_ = tli;
            return PMIX_SUCCESS;
        }
    }

    /* Nope, we didn't find it -- make a new one */
    tli = PMIX_NEW(tuple_list_item_t);
    if (NULL == tli) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    tli->tli_filename = strdup(filename);
    tli->tli_topic = strdup(topic);
    pmix_list_append(&abd_tuples, &(tli->super));
    *tli_ = tli;

    return PMIX_ERR_NOT_FOUND;
}

static void pmix_show_accumulated_duplicates(int fd, short event, void *context)
{
    time_t now = time(NULL);
    tuple_list_item_t *tli;
    char *tmp;
    PMIX_HIDE_UNUSED_PARAMS(fd, event, context);

    /* Loop through all the messages we've displayed and see if any
       processes have sent duplicates that have not yet been displayed
       yet */
    PMIX_LIST_FOREACH(tli, &abd_tuples, tuple_list_item_t)
    {
        if (tli->tli_display && tli->tli_count_since_last_display > 0) {
            static bool first = true;
            pmix_asprintf(&tmp, "%d more process%s sent help message %s / %s\n",
                          tli->tli_count_since_last_display,
                          (tli->tli_count_since_last_display > 1) ? "es have" : " has",
                          tli->tli_filename, tli->tli_topic);
            tli->tli_time_displayed = time(NULL);
            char stamp[50] = {0};
            strftime(stamp, 50, "%Y-%m-%d %H:%M:%S", localtime(&tli->tli_time_displayed));
            char *buf;
            pmix_asprintf(&buf, "%s-%s", tli->tli_filename, stamp);
            local_delivery(buf, tli->tli_topic, tmp);
            free(buf);
            tli->tli_count_since_last_display = 0;

            if (first) {
                pmix_asprintf(&tmp, "%s", "Set MCA parameter \"base_help_aggregate\" to 0 to see all help / error messages\n");
                local_delivery(tli->tli_filename, tli->tli_topic, tmp);
                first = false;
            }
        }
    }

    show_help_time_last_displayed = now;
    show_help_timer_set = false;
}


pmix_status_t pmix_help_check_dups(const char *filename, const char *topic)
{

    tuple_list_item_t *tli;
    time_t now = time(NULL);
    int rc;

    rc = pmix_get_tli(filename, topic, &tli);
    if (PMIX_SUCCESS == rc) {
        /* Already  displayed!
           But do we want to print anything?  That's complicated.
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
            pmix_show_accumulated_duplicates(0, 0, NULL);
        }
        if (!show_help_timer_set) {
            pmix_event_evtimer_set(pmix_globals.evbase, &show_help_timer_event,
                                   pmix_show_accumulated_duplicates, NULL);
            pmix_event_evtimer_add(&show_help_timer_event, &show_help_interval);
            show_help_timer_set = true;
        }
    }
    /* Not already displayed */
    else if (PMIX_ERR_NOT_FOUND == rc) {
        if (!show_help_timer_set) {
            show_help_time_last_displayed = now;
        }
    }
    else {
        /* Some other error occurred */
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    return rc;
}


/*
 * Local functions
 */
pmix_status_t pmix_show_help_init(char *helpdir)
{
    pmix_output_stream_t lds;

    PMIX_CONSTRUCT(&lds, pmix_output_stream_t);
    lds.lds_want_stderr = true;
    output_stream = pmix_output_open(&lds);
    PMIX_CONSTRUCT(&abd_tuples, pmix_list_t);

    PMIx_Argv_append_nosize(&search_dirs, pmix_pinstall_dirs.pmixdatadir);
    if(NULL != helpdir) {
        PMIx_Argv_append_nosize(&search_dirs, helpdir);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_show_help_finalize(void)
{
    pmix_output_close(output_stream);
    output_stream = -1;

    /* destruct the search list */
    if (NULL != search_dirs) {
        PMIx_Argv_free(search_dirs);
        search_dirs = NULL;
    };

    PMIX_LIST_DESTRUCT(&abd_tuples);
    return PMIX_SUCCESS;
}

/*
 * Make one big string with all the lines.  This isn't the most
 * efficient method in the world, but we're going for clarity here --
 * not optimization.  :-)
 */
static pmix_status_t array2string(char **outstring, int want_error_header, char **lines)
{
    int i, count;
    size_t len;

    /* See how much space we need */

    len = want_error_header ? 2 * strlen(dash_line) : 0;
    count = PMIx_Argv_count(lines);
    for (i = 0; i < count; ++i) {
        if (NULL == lines[i]) {
            break;
        }
        len += strlen(lines[i]) + 1;
    }

    /* Malloc it out */

    (*outstring) = (char *) malloc(len + 1);
    if (NULL == *outstring) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    /* Fill the big string */

    *(*outstring) = '\0';
    if (want_error_header) {
        strcat(*outstring, dash_line);
    }
    for (i = 0; i < count; ++i) {
        if (NULL == lines[i]) {
            break;
        }
        strcat(*outstring, lines[i]);
        strcat(*outstring, "\n");
    }
    if (want_error_header) {
        strcat(*outstring, dash_line);
    }

    return PMIX_SUCCESS;
}

/*
 * Find the right file to open
 */
static pmix_status_t open_file(const char *base,
                               const char *topic,
                               FILE **fptr)
{
    char *filename;
    char *err_msg = NULL;
    size_t base_len;
    int i;
    FILE *fp = NULL;

    /* If no filename was supplied, use the default */

    if (NULL == base) {
        base = default_filename;
    }

    /* if this is called prior to someone initializing the system,
     * then don't try to look
     */
    if (NULL != search_dirs) {
        /* Try to open the file.  If we can't find it, try it with a .txt
         * extension.
         */
        for (i = 0; NULL != search_dirs[i]; i++) {
            filename = pmix_os_path(false, search_dirs[i], base, NULL);
            fp = fopen(filename, "r");
            if (NULL == fp) {
                if (NULL != err_msg) {
                    free(err_msg);
                }
                if (0 > asprintf(&err_msg, "%s: %s", filename, strerror(errno))) {
                    free(filename);
                    return PMIX_ERR_OUT_OF_RESOURCE;
                }
                base_len = strlen(base);
                if (4 > base_len || 0 != strcmp(base + base_len - 4, ".txt")) {
                    free(filename);
                    if (0 > asprintf(&filename, "%s%s%s.txt", search_dirs[i], PMIX_PATH_SEP, base)) {
                        free(err_msg);
                        return PMIX_ERR_OUT_OF_RESOURCE;
                    }
                    fp = fopen(filename, "r");
                }
            }
            free(filename);
            if (NULL != fp) {
                break;
            }
        }
    }

    /* If we still couldn't open it, then something is wrong */
    if (NULL == fp) {
        char *msg;
        pmix_asprintf(&msg, "%sSorry!  You were supposed to get help about:\n    %s\nBut I couldn't open "
                    "the help file:\n    %s.  Sorry!\n%s", dash_line, topic, err_msg, dash_line);
        local_delivery(err_msg, topic, msg);
        free(err_msg);
        return PMIX_ERR_NOT_FOUND;
    }

    if (NULL != err_msg) {
        free(err_msg);
    }

    *fptr = fp;
    /* Happiness */
    return PMIX_SUCCESS;
}

#define PMIX_MAX_LINE_LENGTH 1024

static char *localgetline(FILE *fp)
{
    char *ret, *buff;
    char input[PMIX_MAX_LINE_LENGTH];
    int i = 0;

    ret = fgets(input, PMIX_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        if ('\0' != input[0]) {
            input[strlen(input) - 1] = '\0'; /* remove newline */
        }
        buff = strdup(&input[i]);
        return buff;
    }

    return NULL;
}

/*
 * In the file that has already been opened, find the topic that we're
 * supposed to output
 */
static pmix_status_t find_topic(FILE *fp,
                                const char *base,
                                const char *topic)
{
    char *line, *cptr;
    PMIX_HIDE_UNUSED_PARAMS(base);

    /* Examine every topic */

    while (NULL != (line = localgetline(fp))) {
        /* topics start with a '[' in the first position */
        if ('[' != line[0]) {
            free(line);
            continue;
        }
        /* find the end of the topic name */
        cptr = strchr(line, ']');
        if (NULL == cptr) {
            /* this is not a valid topic */
            free(line);
            continue;
        }
        *cptr = '\0';
        if (0 == strcmp(&line[1], topic)) {
            /* this is the topic */
            free(line);
            return PMIX_SUCCESS;
        }
        /* not the topic we want */
        free(line);
    }

    return PMIX_ERR_NOT_FOUND;
}

/*
 * We have an open file, and we're pointed at the right topic.  So
 * read in all the lines in the topic and make a list of them.
 */
static pmix_status_t read_topic(FILE *fp, char ***array)
{
    int rc;
    char *line, *file, *tp;
    char **tmparray = NULL;

    while (NULL != (line = localgetline(fp))) {
        /* the topic ends when we see either the end of
         * the file (indicated by a NULL return) or the
         * beginning of the next topic */
        if (0 == strncmp(line, "#include#", strlen("#include#"))) {
            /* keyword "include" found - check for file/topic */
            file = &line[strlen("#include#")];
            if (0 == strlen(file)) {
                /* missing filename */
                free(line);
                return PMIX_ERR_BAD_PARAM;
            }
            /* see if they provided a topic */
            tp = strchr(file, '#');
            if (NULL != tp) {
                *tp = '\0';  // NULL-terminate the filename
                ++tp;
            }
            rc = load_array(&tmparray, file, tp);
            if (PMIX_SUCCESS != rc) {
                free(line);
                return rc;
            }
        }
        if ('#' == line[0]) {
            /* skip comments */
            free(line);
            continue;
        }
        if ('[' == line[0]) {
            /* start of the next topic */
            free(line);

            /* Fall through to strip out leading / trailing blank
               lines */
            break;
        }
        /* save the line */
        rc = PMIx_Argv_append_nosize(&tmparray, line);
        free(line);
        if (rc != PMIX_SUCCESS) {
            return rc;
        }
    }

    /* Strip off empty lines at the beginning and end of the resulting
       array, because RST/Sphinx requires us to have blank lines to
       separate paragraphs.

       This algorithm is neither clever nor efficient, but it's
       simple.  First, find the first and last non-blank lines. */
    int first_nonblank = -1;
    int last_nonblank = -1;
    for (int i = 0; NULL != tmparray[i]; ++i) {
        if (tmparray[i][0] != '\0') {
            if (-1 == first_nonblank) {
                first_nonblank = i;
            }
            last_nonblank = i;
        }
    }

    /* If there were no non-blank lines, that's an error */
    if (-1 == first_nonblank) {
        PMIx_Argv_free(tmparray);
        return PMIX_ERR_NOT_FOUND;
    }

    /* Copy the range of [first_nonblank, last_nonblank] to the output
       array */
    for (int i = first_nonblank; i <= last_nonblank; ++i) {
        PMIx_Argv_append_nosize(array, tmparray[i]);
    }
    PMIx_Argv_free(tmparray);

    return PMIX_SUCCESS;
}

static pmix_status_t load_array(char ***array,
                                const char *filename,
                                const char *topic)
{
    int ret;
    FILE *fp;

    ret = open_file(filename, topic, &fp);
    if (PMIX_SUCCESS != ret) {
        return ret;
    }

    ret = find_topic(fp, filename, topic);
    if (PMIX_SUCCESS == ret) {
        ret = read_topic(fp, array);
    }

    fclose(fp);

    if (PMIX_SUCCESS != ret) {
        PMIx_Argv_free(*array);
    }

    return ret;
}

char *pmix_show_help_vstring(const char *filename,
                             const char *topic,
                             int want_error_header,
                             va_list arglist)
{
    int rc;
    char *single_string, *output, **array = NULL;

    /* Load the message */
    if (PMIX_SUCCESS != (rc = load_array(&array, filename, topic))) {
        return NULL;
    }

    /* Convert it to a single raw string */
    rc = array2string(&single_string, want_error_header, array);

    if (PMIX_SUCCESS == rc) {
        /* Apply the formatting to make the final output string */
        if (0 > vasprintf(&output, single_string, arglist)) {
            output = NULL;
        }
        free(single_string);
    }

    PMIx_Argv_free(array);
    return (PMIX_SUCCESS == rc) ? output : NULL;
}

char *pmix_show_help_string(const char *filename, const char *topic, int want_error_handler, ...)
{
    char *output;
    va_list arglist;

    va_start(arglist, want_error_handler);
    output = pmix_show_help_vstring(filename, topic, want_error_handler, arglist);
    va_end(arglist);

    return output;
}

pmix_status_t pmix_show_vhelp(const char *filename, const char *topic,
                              int want_error_header, va_list arglist)
{
    char *output;

    /* Convert it to a single string */
    output = pmix_show_help_vstring(filename, topic, want_error_header, arglist);

    /* If we got a single string, output it with formatting */
    if (NULL != output) {
        local_delivery(filename, topic, output);
    }

    return (NULL == output) ? PMIX_ERROR : PMIX_SUCCESS;
}

pmix_status_t pmix_show_help(const char *filename, const char *topic,
                             int want_error_header, ...)
{
    va_list arglist;
    char *output;

    va_start(arglist, want_error_header);
    output = pmix_show_help_vstring(filename, topic, want_error_header, arglist);
    va_end(arglist);

    /* If nothing came back, there's nothing to do */
    if (NULL == output) {
        return PMIX_SUCCESS;
    }

    local_delivery(filename, topic, output);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_show_help_add_dir(const char *directory)
{
    PMIx_Argv_append_nosize(&search_dirs, directory);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_show_help_norender(const char *filename,
                                      const char *topic,
                                      const char *output)
{
    local_delivery(filename, topic, output);
    return PMIX_SUCCESS;
}
