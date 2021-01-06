/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

/* This is taken from src/pm/util/pmiport.c */
int MPL_env2range(const char *envName, int *lowPtr, int *highPtr)
{
    const char *range_ptr;
    int low = 0, high = 0;

    /* Get the low and high range.  */
    range_ptr = getenv(envName);
    if (range_ptr) {
        const char *p;
        /* Look for n:m format */
        p = range_ptr;
        while (*p && isspace(*p))
            p++;
        while (*p && isdigit(*p))
            low = 10 * low + (*p++ - '0');
        if (*p == ':') {
            p++;
            while (*p && isdigit(*p))
                high = 10 * high + (*p++ - '0');
        }
        if (*p) {
            MPL_error_printf("Invalid character %c in %s\n", *p, envName);
            return -1;
        }
        *lowPtr = low;
        *highPtr = high;
    }
    return 0;
}

/*
 * Returns the value for a given envName in val if present, returns -1 if
 * there is an error, 0 for no value, and 1 for value found (note that this
 * isn't the same as GetEnvRange).  val is changed only if envName is
 * found and is a valid integer.
 */
int MPL_env2int(const char *envName, int *val)
{
    const char *val_ptr;

    val_ptr = getenv(envName);
    if (val_ptr) {
        const char *p;
        int sign = 1, value = 0;
        p = val_ptr;
        while (*p && isspace(*p))
            p++;
        if (*p == '-') {
            p++;
            sign = -1;
        }
        if (*p == '+')
            p++;
        while (*p && isdigit(*p))
            value = 10 * value + (*p++ - '0');
        if (*p) {
            MPL_error_printf("Invalid character %c in %s\n", *p, envName);
            return -1;
        }
        *val = sign * value;
        return 1;
    }
    return 0;
}

/* Get a boolean value for an environment variable.  To be user-friendly,
   accept a range of choices:

   TRUE: yes, YES, true, TRUE, on, ON, 1
   FALSE: no, NO, false, FALSE, off, OFF, 0

   returns 1 if a value was found, 0 if no value and -1 if an unrecognized
   value was found.  The boolean value is stored in *val if found.
*/
int MPL_env2bool(const char *envName, int *val)
{
    const char *val_ptr;

    val_ptr = getenv(envName);
    if (val_ptr) {
        if (strcmp(val_ptr, "YES") == 0 ||
            strcmp(val_ptr, "yes") == 0 ||
            strcmp(val_ptr, "TRUE") == 0 ||
            strcmp(val_ptr, "true") == 0 ||
            strcmp(val_ptr, "ON") == 0 || strcmp(val_ptr, "on") == 0 || strcmp(val_ptr, "1") == 0) {
            *val = 1;
            return 1;
        }
        if (strcmp(val_ptr, "NO") == 0 ||
            strcmp(val_ptr, "no") == 0 ||
            strcmp(val_ptr, "FALSE") == 0 ||
            strcmp(val_ptr, "false") == 0 ||
            strcmp(val_ptr, "OFF") == 0 ||
            strcmp(val_ptr, "off") == 0 || strcmp(val_ptr, "0") == 0) {
            *val = 0;
            return 1;
        }
        /* Else an invalid value */
        /* FIXME: We need to provide a way to signal this error */
        return -1;
    }
    return 0;
}

int MPL_env2str(const char *envName, const char **val)
{
    const char *val_ptr;

    val_ptr = getenv(envName);

    if (val_ptr) {
        *val = val_ptr;
        return 1;
    }

    return 0;
}

int MPL_putenv(char *name_val)
{
    return putenv(name_val);
}

/*
 * Returns the value for a given envName in val if present, returns -1 if
 * there is an error, 0 for no value, and 1 for value found (note that this
 * isn't the same as GetEnvRange).  val is changed only if envName is
 * found and is a valid double.
 */
int MPL_env2double(const char *envName, double *val)
{
    const char *val_ptr;
    char *end_ptr = NULL;
    double tmp;

    val_ptr = getenv(envName);
    if (val_ptr) {
        /* just use strtod for now, we can patch it up with configury if we
         * encounter any platforms that don't actually have it */
        tmp = strtod(val_ptr, &end_ptr);
        if (tmp == 0.0 && val_ptr == end_ptr) {
            /* no conversion was performed by strtod */
            return -1;
        } else {
            *val = tmp;
            return 1;
        }
    }
    return 0;
}
