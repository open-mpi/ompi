/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

#ifdef MPL_HAVE_MATH_H
#include <math.h>
#endif
/* ctype is needed for isspace and isascii (isspace is only defined for
   values on which isascii returns true). */
#include <ctype.h>

static int encode_buffer(char *dest, int dest_length, const char *src,
                         int src_length, int *num_encoded)
{
    int num_used;
    int n = 0;
    if (src_length == 0) {
        if (dest_length > 2) {
            *dest = MPL_STR_QUOTE_CHAR;
            dest++;
            *dest = MPL_STR_QUOTE_CHAR;
            dest++;
            *dest = '\0';
            *num_encoded = 0;
            return MPL_SUCCESS;
        } else {
            return MPL_ERR_STR_TRUNCATED;
        }
    }
    while (src_length && dest_length) {
        num_used = MPL_snprintf(dest, dest_length, "%02X", (unsigned char) *src);
        if (num_used < 0) {
            *num_encoded = n;
            return MPL_ERR_STR_TRUNCATED;
        }
        /*MPL_DBG_MSG_FMT(STRING,VERBOSE,(MPL_DBG_FDEST," %c = %c%c",
         * ch, dest[0], dest[1])); */
        dest += num_used;
        dest_length -= num_used;
        src++;
        n++;
        src_length--;
    }
    *num_encoded = n;
    return src_length ? MPL_ERR_STR_TRUNCATED : MPL_SUCCESS;
}

static int decode_buffer(const char *str, char *dest, int length, int *num_decoded)
{
    char hex[3];
    int value;
    int n = 0;

    if (str == NULL || dest == NULL || num_decoded == NULL)
        return MPL_ERR_STR_FAIL;
    if (length < 1) {
        *num_decoded = 0;
        if (*str == '\0')
            return MPL_SUCCESS;
        return MPL_ERR_STR_TRUNCATED;
    }
    if (*str == MPL_STR_QUOTE_CHAR)
        str++;
    hex[2] = '\0';
    while (*str != '\0' && *str != MPL_STR_SEPAR_CHAR && *str != MPL_STR_QUOTE_CHAR && length) {
        hex[0] = *str;
        str++;
        hex[1] = *str;
        str++;
        if (0 == sscanf(hex, "%X", &value))
            return MPL_ERR_STR_TRUNCATED;
        *dest = (char) value;
        /*MPL_DBG_MSG_FMT(STRING,VERBOSE,(MPL_DBG_FDEST," %s = %c",
         * hex, *dest)); */
        dest++;
        n++;
        length--;
    }
    *num_decoded = n;
    if (length == 0) {
        if (*str != '\0' && *str != MPL_STR_SEPAR_CHAR && *str != MPL_STR_QUOTE_CHAR)
            return MPL_ERR_STR_TRUNCATED;
    }
    return MPL_SUCCESS;
}

static const char *first_token(const char *str)
{
    if (str == NULL)
        return NULL;
    /* isspace is defined only if isascii is true */
    while (/*isascii(*str) && isspace(*str) */ *str == MPL_STR_SEPAR_CHAR)
        str++;
    if (*str == '\0')
        return NULL;
    return str;
}

static const char *next_token(const char *str)
{
    if (str == NULL)
        return NULL;
    str = first_token(str);
    if (str == NULL)
        return NULL;
    if (*str == MPL_STR_QUOTE_CHAR) {
        /* move over string */
        str++;  /* move over the first quote */
        if (*str == '\0')
            return NULL;
        while (*str != MPL_STR_QUOTE_CHAR) {
            /* move until the last quote, ignoring escaped quotes */
            if (*str == MPL_STR_ESCAPE_CHAR) {
                str++;
                if (*str == MPL_STR_QUOTE_CHAR)
                    str++;
            } else {
                str++;
            }
            if (*str == '\0')
                return NULL;
        }
        str++;  /* move over the last quote */
    } else {
        if (*str == MPL_STR_DELIM_CHAR) {
            /* move over the DELIM token */
            str++;
        } else {
            /* move over literal */
            while (/*(isascii(*str) &&
                         * !isspace(*str)) && */
                      *str != MPL_STR_SEPAR_CHAR && *str != MPL_STR_DELIM_CHAR && *str != '\0')
                str++;
        }
    }
    return first_token(str);
}

static int compare_token(const char *token, const char *str)
{
    if (token == NULL || str == NULL)
        return -1;

    if (*token == MPL_STR_QUOTE_CHAR) {
        /* compare quoted strings */
        token++;        /* move over the first quote */
        /* compare characters until reaching the end of the string or the
         * end quote character */
        for (;;) {
            if (*token == MPL_STR_ESCAPE_CHAR) {
                if (*(token + 1) == MPL_STR_QUOTE_CHAR) {
                    /* move over the escape character if the next character
                     * is a quote character */
                    token++;
                }
                if (*token != *str)
                    break;
            } else {
                if (*token != *str || *token == MPL_STR_QUOTE_CHAR)
                    break;
            }
            if (*str == '\0')
                break;
            token++;
            str++;
        }
        if (*str == '\0' && *token == MPL_STR_QUOTE_CHAR)
            return 0;
        if (*token == MPL_STR_QUOTE_CHAR)
            return 1;
        if (*str < *token)
            return -1;
        return 1;
    }

    /* compare DELIM token */
    if (*token == MPL_STR_DELIM_CHAR) {
        if (*str == MPL_STR_DELIM_CHAR) {
            str++;
            if (*str == '\0')
                return 0;
            return 1;
        }
        if (*token < *str)
            return -1;
        return 1;
    }

    /* compare literals */
    while (*token == *str &&
           *str != '\0' && *token != MPL_STR_DELIM_CHAR && (*token != MPL_STR_SEPAR_CHAR)) {
        token++;
        str++;
    }
    if ((*str == '\0') &&
        (*token == MPL_STR_DELIM_CHAR || (*token == MPL_STR_SEPAR_CHAR) || *token == '\0'))
        return 0;
    if (*token == MPL_STR_DELIM_CHAR || (*token == MPL_STR_SEPAR_CHAR) || *token < *str)
        return -1;
    return 1;
}


static int token_copy(const char *token, char *str, int maxlen)
{
    /* check parameters */
    if (token == NULL || str == NULL)
        return MPL_ERR_STR_FAIL;

    /* check special buffer lengths */
    if (maxlen < 1)
        return MPL_ERR_STR_FAIL;
    if (maxlen == 1) {
        *str = '\0';
        return (str[0] == '\0') ? MPL_SUCCESS : MPL_ERR_STR_TRUNCATED;
    }

    /* cosy up to the token */
    token = first_token(token);
    if (token == NULL) {
        *str = '\0';
        return MPL_SUCCESS;
    }

    if (*token == MPL_STR_DELIM_CHAR) {
        /* copy the special deliminator token */
        str[0] = MPL_STR_DELIM_CHAR;
        str[1] = '\0';
        return MPL_SUCCESS;
    }

    if (*token == MPL_STR_QUOTE_CHAR) {
        /* quoted copy */
        token++;        /* move over the first quote */
        do {
            if (*token == MPL_STR_ESCAPE_CHAR) {
                if (*(token + 1) == MPL_STR_QUOTE_CHAR)
                    token++;
                *str = *token;
            } else {
                if (*token == MPL_STR_QUOTE_CHAR) {
                    *str = '\0';
                    return MPL_SUCCESS;
                }
                *str = *token;
            }
            str++;
            token++;
            maxlen--;
        } while (maxlen);
        /* we've run out of destination characters so back up and null
         * terminate the string */
        str--;
        *str = '\0';
        return MPL_ERR_STR_TRUNCATED;
    }

    /* literal copy */
    while (*token != MPL_STR_DELIM_CHAR &&
           (*token != MPL_STR_SEPAR_CHAR) && *token != '\0' && maxlen) {
        *str = *token;
        str++;
        token++;
        maxlen--;
    }
    if (maxlen) {
        *str = '\0';
        return MPL_SUCCESS;
    }
    str--;
    *str = '\0';
    return MPL_ERR_STR_TRUNCATED;
}

/*@ MPL_str_get_string_arg - Extract an option from a string with a
  maximum length

Input Parameters:
+   str - Source string
.   key - key
-   maxlen - Maximum total length of 'val'

Output Parameters:
.   val - output string

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine searches for a "key = value" entry in a string

  Module:
  Utility
  @*/
int MPL_str_get_string_arg(const char *str, const char *flag, char *val, int maxlen)
{
    if (maxlen < 1)
        return MPL_ERR_STR_FAIL;

    /* line up with the first token */
    str = first_token(str);
    if (str == NULL)
        return MPL_ERR_STR_FAIL;

    /* This loop will match the first instance of "flag = value" in the string. */
    do {
        if (compare_token(str, flag) == 0) {
            str = next_token(str);
            if (compare_token(str, MPL_STR_DELIM_STR) == 0) {
                str = next_token(str);
                if (str == NULL)
                    return MPL_ERR_STR_FAIL;
                return token_copy(str, val, maxlen);
            }
        } else {
            str = next_token(str);
        }
    } while (str);
    return MPL_ERR_STR_FAIL;
}

/*@ MPL_str_get_binary_arg - Extract an option from a string with a maximum
  length

Input Parameters:
+   str - Source string
.   key - key
-   maxlen - Maximum total length of 'buffer'

Output Parameters:
+   buffer - output buffer
-   out_length - output length

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine searches for a "key = value" entry in a string and decodes
    the value
    back to binary data.  The data must have been encoded with
    MPL_str_add_binary_arg.

  Module:
  Utility
  @*/
int MPL_str_get_binary_arg(const char *str, const char *flag, char *buffer,
                           int maxlen, int *out_length)
{
    if (maxlen < 1)
        return MPL_ERR_STR_FAIL;

    /* line up with the first token */
    str = first_token(str);
    if (str == NULL)
        return MPL_ERR_STR_FAIL;

    /* This loop will match the first instance of "flag = value" in the string. */
    do {
        if (compare_token(str, flag) == 0) {
            str = next_token(str);
            if (compare_token(str, MPL_STR_DELIM_STR) == 0) {
                str = next_token(str);
                if (str == NULL)
                    return MPL_ERR_STR_FAIL;
                return decode_buffer(str, buffer, maxlen, out_length);
            }
        } else {
            str = next_token(str);
        }
    } while (str);
    return MPL_ERR_STR_FAIL;
}

/*@ MPL_str_get_int_arg - Extract an option from a string

Input Parameters:
+   str - Source string
-   key - key

Output Parameters:
.   val_ptr - pointer to the output integer

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine searches for a "key = value" entry in a string and decodes the value
    back to an int.

  Module:
  Utility
  @*/
int MPL_str_get_int_arg(const char *str, const char *flag, int *val_ptr)
{
    int result;
    char int_str[12];

    result = MPL_str_get_string_arg(str, flag, int_str, 12);
    if (result == MPL_SUCCESS) {
        *val_ptr = atoi(int_str);
        return MPL_SUCCESS;
    }
    return result;
}

/* quoted_printf does not NULL terminate the string if maxlen is reached */
static int quoted_printf(char *str, int maxlen, const char *val)
{
    int count = 0;
    if (maxlen < 1)
        return 0;
    *str = MPL_STR_QUOTE_CHAR;
    str++;
    maxlen--;
    count++;
    while (maxlen) {
        if (*val == '\0')
            break;
        if (*val == MPL_STR_QUOTE_CHAR) {
            *str = MPL_STR_ESCAPE_CHAR;
            str++;
            maxlen--;
            count++;
            if (maxlen == 0)
                return count;
        }
        *str = *val;
        str++;
        maxlen--;
        count++;
        val++;
    }
    if (maxlen) {
        *str = MPL_STR_QUOTE_CHAR;
        str++;
        maxlen--;
        count++;
        if (maxlen == 0)
            return count;
        *str = '\0';
    }
    return count;
}

/*@ MPL_str_add_string - Add a string to a string

Input Parameters:
+   str_ptr - pointer to the destination string
.   maxlen_ptr - pointer to the maximum length of '*str_ptr'
-   val - string to add

Output Parameters:
+   str_ptr - The string pointer is updated to the next available location in
    the string
-   maxlen_ptr - maxlen is decremented by the amount str_ptr is incremented

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine adds a string to a string in such a way that
    MPL_str_get_string can
    retreive the same string back.  It takes into account spaces and quote
    characters.
    The string pointer is updated to the start of the next string in the
    string and maxlen is updated accordingly.

  Module:
  Utility
  @*/
int MPL_str_add_string(char **str_ptr, int *maxlen_ptr, const char *val)
{
    int num_chars;
    char *str;
    int maxlen;

    str = *str_ptr;
    maxlen = *maxlen_ptr;

    if (strchr(val, MPL_STR_SEPAR_CHAR) ||
        strchr(val, MPL_STR_QUOTE_CHAR) || strchr(val, MPL_STR_DELIM_CHAR)) {
        num_chars = quoted_printf(str, maxlen, val);
        if (num_chars == maxlen) {
            /* truncation, cleanup string */
            *str = '\0';
            return -1;
        }
        if (num_chars < maxlen - 1) {
            str[num_chars] = MPL_STR_SEPAR_CHAR;
            str[num_chars + 1] = '\0';
            num_chars++;
        } else {
            str[num_chars] = '\0';
        }
    } else {
        if (*val == '\0') {
            num_chars = MPL_snprintf(str, maxlen, MPL_STR_QUOTE_STR MPL_STR_QUOTE_STR /*"\"\"" */);
        } else {
            num_chars = MPL_snprintf(str, maxlen, "%s%c", val, MPL_STR_SEPAR_CHAR);
        }
        if (num_chars == maxlen) {
            *str = '\0';
            return -1;
        }
    }
    *str_ptr += num_chars;
    *maxlen_ptr -= num_chars;
    return 0;
}

/*@ MPL_str_get_string - Get the next string from a string

Input Parameters:
+   str_ptr - pointer to the destination string
-   maxlen_ptr - pointer to the maximum length of '*str_ptr'

Output Parameters:
+   str_ptr - location of the next string
-   val - location to store the string

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Return Value:
    The return value is 0 for success, -1 for insufficient buffer space, and
    1 for failure.

    Notes:
    This routine gets a string that was previously added by
    MPL_str_add_string.
    It takes into account spaces and quote characters. The string pointer is
    updated to the start of the next string in the string.

  Module:
  Utility
  @*/
int MPL_str_get_string(char **str_ptr, char *val, int maxlen)
{
    int result;
    char *str;

    if (str_ptr == NULL) {
        return -2;
    }

    str = *str_ptr;

    if (maxlen < 1) {
        return 0;
    }

    /* line up with the first token */
    str = (char *) first_token(str);
    if (str == NULL) {
        return 0;
    }

    /* copy the token */
    result = token_copy(str, val, maxlen);
    if (result == MPL_SUCCESS) {
        str = (char *) next_token(str);
        *str_ptr = str;
        return 0;
    } else if (result == MPL_ERR_STR_TRUNCATED) {
        return -1;
    }

    /* failure */
    return -2;
}

/*@ MPL_str_add_string_arg - Add an option to a string with a maximum length

Input Parameters:
+   str_ptr - Pointer to the destination string
.   maxlen_ptr - Pointer to the maximum total length of '*str_ptr'
.   key - key
-   val - input string

Output Parameters:
+   str_ptr - The string pointer is updated to the next available location in
    the string
-   maxlen_ptr - maxlen is reduced by the number of characters written

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine adds a string option to a string in the form "key = value".

  Module:
  Utility
  @*/
int MPL_str_add_string_arg(char **str_ptr, int *maxlen_ptr, const char *flag, const char *val)
{
    int num_chars;
    char **orig_str_ptr;

    if (maxlen_ptr == NULL)
        return MPL_ERR_STR_FAIL;

    orig_str_ptr = str_ptr;

    if (*maxlen_ptr < 1)
        return MPL_ERR_STR_FAIL;

    /* add the flag */
    if (strstr(flag, MPL_STR_SEPAR_STR) || strstr(flag, MPL_STR_DELIM_STR) ||
        flag[0] == MPL_STR_QUOTE_CHAR) {
        num_chars = quoted_printf(*str_ptr, *maxlen_ptr, flag);
    } else {
        num_chars = MPL_snprintf(*str_ptr, *maxlen_ptr, "%s", flag);
    }
    *maxlen_ptr = *maxlen_ptr - num_chars;
    if (*maxlen_ptr < 1) {
        MPL_DBG_MSG_S(MPIR_DBG_STRING, VERBOSE, "partial argument added to string: '%s'", *str_ptr);
        **str_ptr = '\0';
        return MPL_ERR_STR_NOMEM;
    }
    *str_ptr = *str_ptr + num_chars;

    /* add the deliminator character */
    **str_ptr = MPL_STR_DELIM_CHAR;
    *str_ptr = *str_ptr + 1;
    *maxlen_ptr = *maxlen_ptr - 1;

    /* add the value string */
    if (strstr(val, MPL_STR_SEPAR_STR) || strstr(val, MPL_STR_DELIM_STR) ||
        val[0] == MPL_STR_QUOTE_CHAR) {
        num_chars = quoted_printf(*str_ptr, *maxlen_ptr, val);
    } else {
        if (*val == '\0') {
            num_chars = MPL_snprintf(*str_ptr, *maxlen_ptr,
                                     MPL_STR_QUOTE_STR MPL_STR_QUOTE_STR /*"\"\"" */);
        } else {
            num_chars = MPL_snprintf(*str_ptr, *maxlen_ptr, "%s", val);
        }
    }
    *str_ptr = *str_ptr + num_chars;
    *maxlen_ptr = *maxlen_ptr - num_chars;
    if (*maxlen_ptr < 2) {
        MPL_DBG_MSG_S(MPIR_DBG_STRING, VERBOSE, "partial argument added to string: '%s'", *str_ptr);
        **orig_str_ptr = '\0';
        return MPL_ERR_STR_NOMEM;
    }

    /* add the trailing space */
    **str_ptr = MPL_STR_SEPAR_CHAR;
    *str_ptr = *str_ptr + 1;
    **str_ptr = '\0';
    *maxlen_ptr = *maxlen_ptr - 1;

    return MPL_SUCCESS;
}

/*@ MPL_str_add_int_arg - Add an option to a string with a maximum length

Input Parameters:
+   str_ptr - Pointer to the destination string
.   maxlen_ptr - Pointer to the maximum total length of '*str_ptr'
.   key - key
-   val - input integer

Output Parameters:
+   str_ptr - The string pointer is updated to the next available location in
    the string
-   maxlen_ptr - maxlen is reduced by the number of characters written

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine adds an integer option to a string in the form "key = value".

  Module:
  Utility
  @*/
int MPL_str_add_int_arg(char **str_ptr, int *maxlen_ptr, const char *flag, int val)
{
    char val_str[12];
    MPL_snprintf(val_str, 12, "%d", val);
    return MPL_str_add_string_arg(str_ptr, maxlen_ptr, flag, val_str);
}

/*@ MPL_str_add_binary_arg - Add an option to a string with a maximum length

Input Parameters:
+   str_ptr - Pointer to the destination string
.   maxlen_ptr - Pointer to the maximum total length of '*str_ptr'
.   key - key
.   val - input data
-   length - length of the input data

Output Parameters:
+   str_ptr - The string pointer is updated to the next available location in
    the string
-   maxlen_ptr - maxlen is reduced by the number of characters written

    Return value:
    MPL_SUCCESS, MPL_ERR_STR_NOMEM, MPL_ERR_STR

    Notes:
    This routine encodes binary data into a string option in the form
    "key = encoded_value".

  Module:
  Utility
  @*/
int MPL_str_add_binary_arg(char **str_ptr, int *maxlen_ptr, const char *flag,
                           const char *buffer, int length)
{
    int result;
    int num_chars;
    char **orig_str_ptr;

    if (maxlen_ptr == NULL)
        return MPL_ERR_STR_FAIL;

    orig_str_ptr = str_ptr;

    if (*maxlen_ptr < 1)
        return MPL_ERR_STR_FAIL;

    /* add the flag */
    if (strstr(flag, MPL_STR_SEPAR_STR) || strstr(flag, MPL_STR_DELIM_STR) ||
        flag[0] == MPL_STR_QUOTE_CHAR) {
        num_chars = quoted_printf(*str_ptr, *maxlen_ptr, flag);
    } else {
        num_chars = MPL_snprintf(*str_ptr, *maxlen_ptr, "%s", flag);
    }
    *maxlen_ptr = *maxlen_ptr - num_chars;
    if (*maxlen_ptr < 1) {
        MPL_DBG_MSG_S(MPIR_DBG_STRING, VERBOSE, "partial argument added to string: '%s'", *str_ptr);
        **str_ptr = '\0';
        return MPL_ERR_STR_NOMEM;
    }
    *str_ptr = *str_ptr + num_chars;

    /* add the deliminator character */
    **str_ptr = MPL_STR_DELIM_CHAR;
    *str_ptr = *str_ptr + 1;
    *maxlen_ptr = *maxlen_ptr - 1;

    /* add the value string */
    result = encode_buffer(*str_ptr, *maxlen_ptr, buffer, length, &num_chars);
    if (result != MPL_SUCCESS) {
        **orig_str_ptr = '\0';
        return result;
    }
    num_chars = num_chars * 2;  /* the encoding function turns one source
                                 * character into two destination characters */
    *str_ptr = *str_ptr + num_chars;
    *maxlen_ptr = *maxlen_ptr - num_chars;
    if (*maxlen_ptr < 2) {
        MPL_DBG_MSG_S(MPIR_DBG_STRING, VERBOSE, "partial argument added to string: '%s'", *str_ptr);
        **orig_str_ptr = '\0';
        return MPL_ERR_STR_NOMEM;
    }

    /* add the trailing space */
    **str_ptr = MPL_STR_SEPAR_CHAR;
    *str_ptr = *str_ptr + 1;
    **str_ptr = '\0';
    *maxlen_ptr = *maxlen_ptr - 1;

    return MPL_SUCCESS;
}
