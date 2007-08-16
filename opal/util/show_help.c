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

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/show_help.h"
#include "opal/util/show_help_lex.h"
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/util/os_path.h"
#include "opal/constants.h"

static int open_file(const char *base, const char *topic);
static int find_topic(const char *base, const char *topic);
static int read_message(char ***lines);
static int output(bool want_error_header, char **lines,
                  const char *base, const char *topic,
                  va_list arglist);
static int destroy_message(char **lines);


/*
 * Private variables
 */
#if 0
/* not attempting i18n-like support right now */
static const char *default_language = "C";
#endif
static const char *default_filename = "help-messages";
static const char *dash_line = "--------------------------------------------------------------------------\n";


int opal_show_help(const char *filename, const char *topic, 
                   bool want_error_header, ...)
{
    int ret;
    va_list arglist;
    char **array = NULL;

    if (OPAL_SUCCESS != (ret = open_file(filename, topic))) {
        return ret;
    }
    if (OPAL_SUCCESS != (ret = find_topic(filename, topic))) {
        fclose(opal_show_help_yyin);
        return ret;
    }

    ret = read_message(&array);
    opal_show_help_finish_parsing();
    fclose(opal_show_help_yyin);
    if (OPAL_SUCCESS != ret) {
        destroy_message(array);
        return ret;
    }

    va_start(arglist, want_error_header);
    output(want_error_header, array, filename, topic, arglist);
    va_end(arglist);

    destroy_message(array);
    return ret;
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
    free(filename);
    if (NULL == opal_show_help_yyin) {
        asprintf(&filename, "%s/%s.txt", opal_install_dirs.pkgdatadir, base);
        opal_show_help_yyin = fopen(filename, "r");
        free(filename);
    }
#else
    /* What's our locale? */

    lang = setlocale(LC_MESSAGES, "");
    if (NULL == lang) {
        lang = default_language;
    }

    /* Do we have a file matching that locale?  If not, open the
       default language (because we know that we have that one) */

    asprintf(&filename, "%s/%s.%s", opal_install_dirs.pkgdatadir, base, lang);
    opal_show_help_yyin = fopen(filename, "r");
    free(filename);
    if (NULL == opal_show_help_yyin) {
        asprintf(&filename, "%s/%s.%s", opal_install_dirs.pkgdatadir, 
                 base, default_language);
        opal_show_help_yyin = fopen(filename, "r");
        free(filename);
    }

    /* If we still couldn't find it, try with no extension */

    if (NULL == opal_show_help_yyin) {
        filename = opal_os_path( false, opal_install_dirs.pkgdatadir, base, NULL );
        opal_show_help_yyin = fopen(filename, "r");
        free(filename);
    }
#endif

    /* If we still couldn't open it, then something is wrong */

    if (NULL == opal_show_help_yyin) {
        fprintf(stderr, dash_line);
        fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
        fprintf(stderr, "But I couldn't find any file matching that name.  Sorry!\n");
        fprintf(stderr, dash_line);
        return OPAL_ERR_NOT_FOUND;
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
            fprintf(stderr, dash_line);
            fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
            fprintf(stderr, "But I couldn't find that topic in the file.  Sorry!\n");
            fprintf(stderr, dash_line);
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
static int read_message(char ***array)
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


/*
 * Make one big string with all the lines.  This isn't the most
 * efficient method in the world, but we're going for clarity here --
 * not optimization.  :-)
 */
static int output(bool want_error_header, char **lines,
                  const char *base, const char *topic,
                  va_list arglist)
{
    int i, count;
    size_t len;
    char *concat;

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

    concat = (char*) malloc(len + 1);
    if (NULL == concat) {
        fprintf(stderr, dash_line);
        fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
        fprintf(stderr, "But memory seems to be exhausted.  Sorry!\n");
        fprintf(stderr, dash_line);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Fill the big string */

    *concat = '\0';
    if (want_error_header) {
        strcat(concat, dash_line);
    }
    for (i = 0; i < count; ++i) {
        if (NULL == lines[i]) {
            break;
        }
        strcat(concat, lines[i]);
        strcat(concat, "\n");
    }
    if (want_error_header) {
        strcat(concat, dash_line);
    }

    /* Apply formatting */
    vfprintf( stderr, concat, arglist );

    /* All done */

    free(concat);
    return OPAL_SUCCESS;
}


/*
 * Free all the strings in the array and destruct the array 
 */
static int destroy_message(char **lines)
{
    int i, count;

    count = opal_argv_count(lines);
    for (i = 0; i < count; ++i) {
        if (NULL == lines[i]) {
            break;
        }
        free(lines[i]);
    }

    return OPAL_SUCCESS;
}
