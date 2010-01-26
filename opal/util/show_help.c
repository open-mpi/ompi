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

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <locale.h>
#include <errno.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/show_help.h"
#include "opal/util/show_help_lex.h"
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/constants.h"


/*
 * Private variables
 */
#if 0
/* not attempting i18n-like support right now */
static const char *default_language = "C";
#endif
static const char *default_filename = "help-messages";
static const char *dash_line = "--------------------------------------------------------------------------\n";
static int output_stream = -1;


int opal_show_help_init(void)
{
    opal_output_stream_t lds;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr = true;
    output_stream = opal_output_open(&lds);

    return OPAL_SUCCESS;
}

int opal_show_help_finalize(void)
{
    opal_output_close(output_stream);
    output_stream = -1;
    return OPAL_SUCCESS;
}

/*
 * Make one big string with all the lines.  This isn't the most
 * efficient method in the world, but we're going for clarity here --
 * not optimization.  :-)
 */
static int array2string(char **outstring,
                        bool want_error_header, char **lines)
{
    int i, count;
    size_t len;

    /* See how much space we need */

    len = want_error_header ? 2 * strlen(dash_line) : 0;
    count = opal_argv_count(lines);
    for (i = 0; i < count; ++i) {
        if (NULL == lines[i]) {
            break;
        }
        len += strlen(lines[i]) + 1;
    }

    /* Malloc it out */

    (*outstring) = (char*) malloc(len + 1);
    if (NULL == *outstring) {
        return OPAL_ERR_OUT_OF_RESOURCE;
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

    return OPAL_SUCCESS;
}


/*
 * Find the right file to open
 */
static int open_file(const char *base, const char *topic)
{
#if 0
    /* not attempting i18n-like support right now */
    const char *lang;
#endif
    char *filename;
    char *err_msg = 0;
    size_t base_len;

    /* If no filename was supplied, use the default */

    if (NULL == base) {
        base = default_filename;
    }

    /* Don't try any i18n-like support right now */
#if 1
    /* Try to open the file.  If we can't find it, try it with a .txt
       extension. */

    filename = opal_os_path( false, opal_install_dirs.pkgdatadir, base, NULL );
    opal_show_help_yyin = fopen(filename, "r");
    if (NULL == opal_show_help_yyin) {
        asprintf(&err_msg, "%s: %s", filename, strerror(errno));
        base_len = strlen(base);
        if (4 > base_len || 0 != strcmp(base + base_len - 4, ".txt")) {
            free(filename);
            asprintf(&filename, "%s%s%s.txt", opal_install_dirs.pkgdatadir,
                     OPAL_PATH_SEP, base);
            opal_show_help_yyin = fopen(filename, "r");
        }
    }
    free(filename);
#else
    /* What's our locale? */

    lang = setlocale(LC_MESSAGES, "");
    if (NULL == lang) {
        lang = default_language;
    }

    /* Do we have a file matching that locale?  If not, open the
       default language (because we know that we have that one) */

    asprintf(&filename, "%s%s%s.%s", opal_install_dirs.pkgdatadir,
             OPAL_PATH_SEP, base, lang);
    opal_show_help_yyin = fopen(filename, "r");
    if (NULL == opal_show_help_yyin) {
        asprintf(&err_msg, "%s: %s", filename, strerror(errno));
        free(filename);
        asprintf(&filename, "%s%s%s.%s", opal_install_dirs.pkgdatadir, 
                 OPAL_PATH_SEP, base, default_language);
        opal_show_help_yyin = fopen(filename, "r");
    }
    free(filename);

    /* If we still couldn't find it, try with no extension */

    if (NULL == opal_show_help_yyin) {
        filename = opal_os_path( false, opal_install_dirs.pkgdatadir, base, NULL );
        opal_show_help_yyin = fopen(filename, "r");
        free(filename);
    }
#endif

    /* If we still couldn't open it, then something is wrong */

    if (NULL == opal_show_help_yyin) {
        opal_output(output_stream, "%sSorry!  You were supposed to get help about:\n    %s\nBut I couldn't open the help file:\n    %s.  Sorry!\n%s", dash_line, topic, err_msg, dash_line);
        free(err_msg);
        return OPAL_ERR_NOT_FOUND;
    }

    if (NULL != err_msg) {
        free(err_msg);
    }

    /* Set the buffer */

    opal_show_help_init_buffer(opal_show_help_yyin);

    /* Happiness */

    return OPAL_SUCCESS;
}


/*
 * In the file that has already been opened, find the topic that we're
 * supposed to output
 */
static int find_topic(const char *base, const char *topic)
{
    int token, ret;
    char *tmp;

    /* Examine every topic */

    while (1) {
        token = opal_show_help_yylex();
        switch (token) {
        case OPAL_SHOW_HELP_PARSE_TOPIC:
            tmp = strdup(opal_show_help_yytext);
            if (NULL == tmp) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            tmp[strlen(tmp) - 1] = '\0';
            ret = strcmp(tmp + 1, topic);
            free(tmp);
            if (0 == ret) {
                return OPAL_SUCCESS;
            }
            break;

        case OPAL_SHOW_HELP_PARSE_MESSAGE:
            break;

        case OPAL_SHOW_HELP_PARSE_DONE:
            opal_output(output_stream, "%sSorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\nBut I couldn't find that topic in the file.  Sorry!\n%s", dash_line, topic, base, dash_line);
            return OPAL_ERR_NOT_FOUND;
            break;

        default:
            break;
        }
    }

    /* Never get here */
}


/*
 * We have an open file, and we're pointed at the right topic.  So
 * read in all the lines in the topic and make a list of them.
 */
static int read_topic(char ***array)
{
    char *tmp;
    int token;

    while (1) {
        token = opal_show_help_yylex();
        switch (token) {
        case OPAL_SHOW_HELP_PARSE_MESSAGE:
            tmp = strdup(opal_show_help_yytext);
            if (NULL == tmp) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            opal_argv_append_nosize(array, tmp);
            break;

        default:
            return OPAL_SUCCESS;
            break;
        }
    }

    /* Never get here */
}


static int load_array(char ***array, const char *filename, const char *topic)
{
    int ret;

    if (OPAL_SUCCESS != (ret = open_file(filename, topic))) {
        return ret;
    }
    if (OPAL_SUCCESS != (ret = find_topic(filename, topic))) {
        fclose(opal_show_help_yyin);
        return ret;
    }

    ret = read_topic(array);
    opal_show_help_finish_parsing();
    fclose(opal_show_help_yyin);
    if (OPAL_SUCCESS != ret) {
        opal_argv_free(*array);
        return ret;
    }

    return OPAL_SUCCESS;
}

char *opal_show_help_vstring(const char *filename, const char *topic, 
                             bool want_error_header, va_list arglist)
{
    int rc;
    char *single_string, *output, **array = NULL;

    /* Load the message */
    if (OPAL_SUCCESS != (rc = load_array(&array, filename, topic))) {
        return NULL;
    }

    /* Convert it to a single raw string */
    rc = array2string(&single_string, want_error_header, array);

    if (OPAL_SUCCESS == rc) {
        /* Apply the formatting to make the final output string */
        vasprintf(&output, single_string, arglist);
        free(single_string);
    }

    opal_argv_free(array);
    return (OPAL_SUCCESS == rc) ? output : NULL;
}

char *opal_show_help_string(const char *filename, const char *topic, 
                            bool want_error_handler, ...)
{
    char *output;
    va_list arglist;

    va_start(arglist, want_error_handler);
    output = opal_show_help_vstring(filename, topic, want_error_handler, 
                                    arglist);
    va_end(arglist);

    return output;
}

int opal_show_vhelp(const char *filename, const char *topic, 
                    bool want_error_header, va_list arglist)
{
    char *output;

    /* Convert it to a single string */
    output = opal_show_help_vstring(filename, topic, want_error_header,
                                    arglist);

    /* If we got a single string, output it with formatting */
    if (NULL != output) {
        opal_output(output_stream, "%s", output);
        free(output);
    }

    return (NULL == output) ? OPAL_ERROR : OPAL_SUCCESS;
}

int opal_show_help(const char *filename, const char *topic, 
                   bool want_error_header, ...)
{
    va_list arglist;
    int rc;

    /* Convert it to a single string */
    va_start(arglist, want_error_header);
    rc = opal_show_vhelp(filename, topic, want_error_header, arglist);
    va_end(arglist);

    return rc;
}
