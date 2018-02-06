/*
 * Copyright © 2013-2014 University of Wisconsin-La Crosse.
 *                         All rights reserved.
 * Copyright © 2016-2017 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#include <private/netloc.h>
#include <netloc.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

char *netloc_line_get_next_token(char **string, char c)
{
    char *field;
    char *string_end;

    if (!*string)
        return NULL;

    string_end = strchr(*string, c);

    if (string_end) {
        string_end[0] = '\0';
        field = *string;
        *string = string_end+1;
    } else {
        field = *string;
        *string = NULL;
    }

    return field;
}

ssize_t netloc_line_get(char **lineptr, size_t *n, FILE *stream)
{
    ssize_t read = getline(lineptr, n, stream);
    if (read == -1)
        return -1;

    /* Remove last \n character */
    char *line = *lineptr;
    size_t lastpos = strlen(line)-1;
    if (line[lastpos] == '\n') {
        line[lastpos] = '\0';
        read--;
    }
    return read;
}

