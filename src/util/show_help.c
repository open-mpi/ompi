/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <locale.h>

#include "include/constants.h"
#include "util/show_help.h"
#include "util/show_help_lex.h"
#include "util/printf.h"
#include "class/ompi_pointer_array.h"

static int open_file(const char *base, const char *topic);
static int find_topic(const char *base, const char *topic);
static int read_message(ompi_pointer_array_t *lines);
static int output(bool want_error_header, ompi_pointer_array_t *lines,
                  const char *base, const char *topic,
                  va_list arglist);
static int destroy_message(ompi_pointer_array_t *lines);


/*
 * Private variables
 */
#if 0
/* not attempting i18n-like support right now */
static const char *default_language = "C";
#endif
static const char *default_filename = "help-messages";
static const char *star_line = "**************************************************************************\n";


int ompi_show_help(const char *filename, const char *topic, 
                   bool want_error_header, ...)
{
    int ret;
    va_list arglist;
    ompi_pointer_array_t array;

    if (OMPI_SUCCESS != (ret = open_file(filename, topic))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = find_topic(filename, topic))) {
        fclose(ompi_show_help_yyin);
        return ret;
    }

    OBJ_CONSTRUCT(&array, ompi_pointer_array_t);
    ret = read_message(&array);
    ompi_show_help_finish_parsing();
    fclose(ompi_show_help_yyin);
    if (OMPI_SUCCESS != ret) {
        destroy_message(&array);
        return ret;
    }

    va_start(arglist, want_error_header);
    output(want_error_header, &array, filename, topic, arglist);
    va_end(arglist);

    destroy_message(&array);
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

    asprintf(&filename, "%s/%s", OMPI_PKGDATADIR, base);
    ompi_show_help_yyin = fopen(filename, "r");
    free(filename);
    if (NULL == ompi_show_help_yyin) {
        asprintf(&filename, "%s/%s.txt", OMPI_PKGDATADIR, base);
        ompi_show_help_yyin = fopen(filename, "r");
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

    asprintf(&filename, "%s/%s.%s", OMPI_PKGDATADIR, base, lang);
    ompi_show_help_yyin = fopen(filename, "r");
    free(filename);
    if (NULL == ompi_show_help_yyin) {
        asprintf(&filename, "%s/%s.%s", OMPI_PKGDATADIR, 
                 base, default_language);
        ompi_show_help_yyin = fopen(filename, "r");
        free(filename);
    }

    /* If we still couldn't find it, try with no extension */

    if (NULL == ompi_show_help_yyin) {
        asprintf(&filename, "%s/%s", OMPI_PKGDATADIR, base);
        ompi_show_help_yyin = fopen(filename, "r");
        free(filename);
    }
#endif

    /* If we still couldn't open it, then something is wrong */

    if (NULL == ompi_show_help_yyin) {
        fprintf(stderr, star_line);
        fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
        fprintf(stderr, "But I couldn't find any file matching that name.  Sorry!\n");
        fprintf(stderr, star_line);
        return OMPI_ERR_NOT_FOUND;
    }

    /* Set the buffer */

    ompi_show_help_init_buffer(ompi_show_help_yyin);

    /* Happiness */

    return OMPI_SUCCESS;
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
        token = ompi_show_help_yylex();
        switch (token) {
        case OMPI_SHOW_HELP_PARSE_TOPIC:
            tmp = strdup(ompi_show_help_yytext);
            if (NULL == tmp) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            tmp[strlen(tmp) - 1] = '\0';
            ret = strcmp(tmp + 1, topic);
            free(tmp);
            if (0 == ret) {
                return OMPI_SUCCESS;
            }
            break;

        case OMPI_SHOW_HELP_PARSE_MESSAGE:
            break;

        case OMPI_SHOW_HELP_PARSE_DONE:
            fprintf(stderr, star_line);
            fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
            fprintf(stderr, "But I couldn't find that topic in the file.  Sorry!\n");
            fprintf(stderr, star_line);
            return OMPI_ERR_NOT_FOUND;
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
static int read_message(ompi_pointer_array_t *array)
{
    char *tmp;
    int token;

    while (1) {
        token = ompi_show_help_yylex();
        switch (token) {
        case OMPI_SHOW_HELP_PARSE_MESSAGE:
            tmp = strdup(ompi_show_help_yytext);
            if (NULL == tmp) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            ompi_pointer_array_add(array, tmp);
            break;

        default:
            return OMPI_SUCCESS;
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
static int output(bool want_error_header, ompi_pointer_array_t *lines,
                  const char *base, const char *topic,
                  va_list arglist)
{
    size_t i, len;
    char *tmp, *concat, *formatted;

    /* See how much space we need */

    len = want_error_header ? 2 * strlen(star_line) : 0;
    for (i = 0; i < ompi_pointer_array_get_size(lines); ++i) {
        tmp = ompi_pointer_array_get_item(lines, i);
        if (NULL == tmp) {
            break;
        }
        len += strlen(tmp) + 1;
    }

    /* Malloc it out */

    concat = malloc(len + 1);
    if (NULL == concat) {
        fprintf(stderr, star_line);
        fprintf(stderr, "Sorry!  You were supposed to get help about:\n    %s\nfrom the file:\n    %s\n", topic, base);
        fprintf(stderr, "But memory seems to be exhausted.  Sorry!\n");
        fprintf(stderr, star_line);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Fill the big string */

    *concat = '\0';
    if (want_error_header) {
        strcat(concat, star_line);
    }
    for (i = 0; i < ompi_pointer_array_get_size(lines); ++i) {
        tmp = ompi_pointer_array_get_item(lines, i);
        if (NULL == tmp) {
            break;
        }
        strcat(concat, tmp);
        strcat(concat, "\n");
    }
    if (want_error_header) {
        strcat(concat, star_line);
    }

    /* Apply formatting */

    vasprintf(&formatted, concat, arglist);

    /* Print it out */

    fprintf(stderr, formatted);

    /* All done */

    free(formatted);
    free(concat);
    return OMPI_SUCCESS;
}


/*
 * Free all the strings in the array and destruct the array 
 */
static int destroy_message(ompi_pointer_array_t *lines)
{
    size_t i;
    char *tmp;

    for (i = 0; i < ompi_pointer_array_get_size(lines); ++i) {
        tmp = ompi_pointer_array_get_item(lines, i);
        if (NULL == tmp) {
            break;
        }
        free(tmp);
    }
    OBJ_DESTRUCT(lines);

    return OMPI_SUCCESS;
}
