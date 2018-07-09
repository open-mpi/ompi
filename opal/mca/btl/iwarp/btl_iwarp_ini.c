/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/show_help.h"

#include "btl_iwarp.h"
#include "btl_iwarp_lex.h"
#include "btl_iwarp_ini.h"

static const char *ini_filename = NULL;
static bool initialized = false;
static opal_list_t devices;
static char *key_buffer = NULL;
static size_t key_buffer_len = 0;


/*
 * Struct to hold the section name, vendor ID, and list of vendor part
 * ID's and a corresponding set of values (parsed from an INI file).
 */
typedef struct parsed_section_values_t {
    char *name;

    uint32_t *vendor_ids;
    int vendor_ids_len;

    uint32_t *vendor_part_ids;
    int vendor_part_ids_len;

    opal_btl_iwarp_ini_values_t values;
} parsed_section_values_t;

/*
 * Struct to hold the final values.   Different from above in a few ways:
 *
 * - The vendor and part IDs will always be set properly
 * - There will only be one part ID (i.e., the above struct is
 *   exploded into multiple of these for each of searching)
 * - There is a super of opal_list_item_t so that we can have a list
 *   of these
 */
typedef struct device_values_t {
    opal_list_item_t super;

    char *section_name;
    uint32_t vendor_id;
    uint32_t vendor_part_id;

    opal_btl_iwarp_ini_values_t values;
} device_values_t;

static void device_values_constructor(device_values_t *s);
static void device_values_destructor(device_values_t *s);

OBJ_CLASS_INSTANCE(device_values_t,
                   opal_list_item_t,
                   device_values_constructor,
                   device_values_destructor);


/*
 * Local functions
 */
static int parse_file(char *filename);
static int parse_line(parsed_section_values_t *item);
static void reset_section(bool had_previous_value, parsed_section_values_t *s);
static void reset_values(opal_btl_iwarp_ini_values_t *v);
static int save_section(parsed_section_values_t *s);


/*
 * Read the INI files for device-specific values and save them in
 * internal data structures for later lookup.
 */
int opal_btl_iwarp_ini_init(void)
{
    int ret = OPAL_ERR_NOT_FOUND;
    char *colon;
    char separator = ':';

    OBJ_CONSTRUCT(&devices, opal_list_t);

    colon = strchr(mca_btl_iwarp_component.device_params_file_names, separator);
    if (NULL == colon) {
        /* If we've only got 1 file (i.e., no colons found), parse it
           and be done */
        ret = parse_file(mca_btl_iwarp_component.device_params_file_names);
    } else {
        /* Otherwise, loop over all the files and parse them */
        char *orig = strdup(mca_btl_iwarp_component.device_params_file_names);
        char *str = orig;

        while (NULL != (colon = strchr(str, ':'))) {
            *colon = '\0';
            ret = parse_file(str);
            /* Note that NOT_FOUND and SUCCESS are not fatal errors
               and we keep going.  Other errors are treated as
               fatal */
            if (OPAL_ERR_NOT_FOUND != ret && OPAL_SUCCESS != ret) {
                break;
            }
            str = colon + 1;
        }
        /* Parse the last file if we didn't have a fatal error above */
        if (OPAL_ERR_NOT_FOUND != ret && OPAL_SUCCESS != ret) {
            ret = parse_file(str);
        }

        /* All done */
        free(orig);
    }

    /* Return SUCCESS unless we got a fatal error */

    initialized = true;
    return (OPAL_SUCCESS == ret || OPAL_ERR_NOT_FOUND == ret) ?
        OPAL_SUCCESS : ret;
}


/*
 * The component found a device and is querying to see if an INI file
 * specified any parameters for it.
 */
int opal_btl_iwarp_ini_query(uint32_t vendor_id, uint32_t vendor_part_id,
                              opal_btl_iwarp_ini_values_t *values)
{
    int ret;
    device_values_t *h;

    if (!initialized) {
        if (OPAL_SUCCESS != (ret = opal_btl_iwarp_ini_init())) {
            return ret;
        }
    }

    if (mca_btl_iwarp_component.verbose) {
        BTL_OUTPUT(("Querying INI files for vendor 0x%04x, part ID %d",
                    vendor_id, vendor_part_id));
    }

    reset_values(values);

    /* Iterate over all the saved devices */
    OPAL_LIST_FOREACH(h, &devices, device_values_t) {
        if (vendor_id == h->vendor_id &&
            vendor_part_id == h->vendor_part_id) {
            /* Found it! */
            /* NOTE: There is a bug in the PGI 6.2 series that causes
               the compiler to choke when copying structs containing
               bool members by value.  So do a memcpy here instead. */
            memcpy(values, &h->values, sizeof(h->values));
            if (mca_btl_iwarp_component.verbose) {
                BTL_OUTPUT(("Found corresponding INI values: %s",
                            h->section_name));
            }
            return OPAL_SUCCESS;
        }
    }

    /* If we fall through to here, we didn't find it */
    if (mca_btl_iwarp_component.verbose) {
        BTL_OUTPUT(("Did not find corresponding INI values"));
    }
    return OPAL_ERR_NOT_FOUND;
}


/*
 * The component is shutting down; release all internal state
 */
int opal_btl_iwarp_ini_finalize(void)
{
    if (initialized) {
        OPAL_LIST_DESTRUCT(&devices);
        initialized = true;
    }

    return OPAL_SUCCESS;
}

/**************************************************************************/

/*
 * Parse a single file
 */
static int parse_file(char *filename)
{
    int val;
    int ret = OPAL_SUCCESS;
    bool showed_no_section_warning = false;
    bool showed_unexpected_tokens_warning = false;
    parsed_section_values_t section;

    reset_section(false, &section);

    /* Open the file */
    ini_filename = filename;
    btl_iwarp_ini_yyin = fopen(filename, "r");
    if (NULL == btl_iwarp_ini_yyin) {
        opal_show_help("help-mpi-btl-iwarp.txt", "ini file:file not found",
                       true, filename);
        ret = OPAL_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Do the parsing */
    btl_iwarp_ini_parse_done = false;
    btl_iwarp_ini_yynewlines = 1;
    btl_iwarp_ini_init_buffer(btl_iwarp_ini_yyin);
    while (!btl_iwarp_ini_parse_done) {
        val = btl_iwarp_ini_yylex();
        switch (val) {
        case BTL_IWARP_INI_PARSE_DONE:
            /* This will also set btl_iwarp_ini_parse_done to true, so just
               break here */
            break;

        case BTL_IWARP_INI_PARSE_NEWLINE:
            /* blank line!  ignore it */
            break;

        case BTL_IWARP_INI_PARSE_SECTION:
            /* We're starting a new section; if we have previously
               parsed a section, go see if we can use its values. */
            save_section(&section);

            reset_section(true, &section);
            section.name = strdup(btl_iwarp_ini_yytext);
            break;

        case BTL_IWARP_INI_PARSE_SINGLE_WORD:
            if (NULL == section.name) {
                /* Warn that there is no current section, and ignore
                   this parameter */
                if (!showed_no_section_warning) {
                    opal_show_help("help-mpi-btl-iwarp.txt", "ini file:not in a section", true);
                    showed_no_section_warning = true;
                }
                /* Parse it and then dump it */
                parse_line(&section);
                reset_section(true, &section);
            } else {
                parse_line(&section);
            }
            break;

        default:
            /* anything else is an error */
            if (!showed_unexpected_tokens_warning) {
                opal_show_help("help-mpi-btl-iwarp.txt", "ini file:unexpected token", true);
                showed_unexpected_tokens_warning = true;
            }
            break;
        }
    }
    save_section(&section);
    fclose(btl_iwarp_ini_yyin);
    btl_iwarp_ini_yylex_destroy ();

cleanup:
    reset_section(true, &section);
    if (NULL != key_buffer) {
        free(key_buffer);
        key_buffer = NULL;
        key_buffer_len = 0;
    }
    return ret;
}


/*
 * Parse a single line in the INI file
 */
static int parse_line(parsed_section_values_t *sv)
{
    int val, ret = OPAL_SUCCESS;
    char *value = NULL;
    bool showed_unknown_field_warning = false;

    /* Save the name name */
    if (key_buffer_len < strlen(btl_iwarp_ini_yytext) + 1) {
        char *tmp;
        key_buffer_len = strlen(btl_iwarp_ini_yytext) + 1;
        tmp = (char *) realloc(key_buffer, key_buffer_len);
        if (NULL == tmp) {
            free(key_buffer);
            key_buffer_len = 0;
            key_buffer = NULL;
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
        key_buffer = tmp;
    }
    strncpy(key_buffer, btl_iwarp_ini_yytext, key_buffer_len);

    /* The first thing we have to see is an "=" */
    val = btl_iwarp_ini_yylex();
    if (btl_iwarp_ini_parse_done || BTL_IWARP_INI_PARSE_EQUAL != val) {
        opal_show_help("help-mpi-btl-iwarp.txt", "ini file:expected equals", true);
        return OPAL_ERROR;
    }

    /* Next we get the value */
    val = btl_iwarp_ini_yylex();
    if (BTL_IWARP_INI_PARSE_SINGLE_WORD != val && BTL_IWARP_INI_PARSE_VALUE != val) {
        return OPAL_ERROR;
    }

    value = strdup(btl_iwarp_ini_yytext);

    /* Now we need to see the newline */
    val = btl_iwarp_ini_yylex();

    /* If we did not get EOL or EOF, something is wrong */
    if (BTL_IWARP_INI_PARSE_NEWLINE != val && BTL_IWARP_INI_PARSE_DONE != val) {
      opal_show_help("help-mpi-btl-iwarp.txt", "ini file:expected newline", true);
      free(value);
      return OPAL_ERROR;
    }

    /* Ok, we got a good parse.  Now figure out what it is and save
       the value.  Note that the flex already took care of trimming
       all whitespace at the beginning and ending of the value. */

    if (0 == strcasecmp(key_buffer, "vendor_id")) {
        if (OPAL_SUCCESS != (ret = opal_btl_iwarp_ini_intify_list(value, &sv->vendor_ids,
                                               &sv->vendor_ids_len))) {
            return ret;
        }
    }

    else if (0 == strcasecmp(key_buffer, "vendor_part_id")) {
        if (OPAL_SUCCESS != (ret = opal_btl_iwarp_ini_intify_list(value, &sv->vendor_part_ids,
                                               &sv->vendor_part_ids_len))) {
            return ret;
        }
    }

    else if (0 == strcasecmp(key_buffer, "mtu")) {
        /* Single value */
        sv->values.mtu = (uint32_t) opal_btl_iwarp_ini_intify(value);
        sv->values.mtu_set = true;
    }

    else if (0 == strcasecmp(key_buffer, "use_eager_rdma")) {
        /* Single value */
        sv->values.use_eager_rdma = (uint32_t) opal_btl_iwarp_ini_intify(value);
        sv->values.use_eager_rdma_set = true;
    }

    else if (0 == strcasecmp(key_buffer, "receive_queues")) {
        /* Single value (already strdup'ed) */
        sv->values.receive_queues = value;
        value = NULL;
    }

    else if (0 == strcasecmp(key_buffer, "max_inline_data")) {
        /* Single value */
        sv->values.max_inline_data = (int32_t) opal_btl_iwarp_ini_intify(value);
        sv->values.max_inline_data_set = true;
    }

    else if (0 == strcasecmp(key_buffer, "rdmacm_reject_causes_connect_error")) {
        /* Single value */
        sv->values.rdmacm_reject_causes_connect_error =
            (bool) opal_btl_iwarp_ini_intify(value);
        sv->values.rdmacm_reject_causes_connect_error_set = true;
    }

    else if (0 == strcasecmp(key_buffer, "ignore_device")) {
        /* Single value */
        sv->values.ignore_device = (bool) opal_btl_iwarp_ini_intify(value);
        sv->values.ignore_device_set = true;
    }

    else {
        /* Have no idea what this parameter is.  Not an error -- just
           ignore it */
        if (!showed_unknown_field_warning) {
            opal_show_help("help-mpi-btl-iwarp.txt",
                           "ini file:unknown field", true,
                           ini_filename, btl_iwarp_ini_yynewlines,
                           key_buffer);
            showed_unknown_field_warning = true;
        }
    }

    /* All done */

    if (NULL != value) {
        free(value);
    }
    return ret;
}


/*
 * Construct an device_values_t and set all of its values to known states
 */
static void device_values_constructor(device_values_t *s)
{
    s->section_name = NULL;
    s->vendor_id = 0;
    s->vendor_part_id = 0;
    reset_values(&s->values);
}


/*
 * Destruct an device_values_t and free any memory that it has
 */
static void device_values_destructor(device_values_t *s)
{
    if (NULL != s->section_name) {
        free(s->section_name);
    }
    if (NULL != s->values.receive_queues) {
        free(s->values.receive_queues);
    }
}


/*
 * Reset a parsed section; free any memory that it may have had
 */
static void reset_section(bool had_previous_value, parsed_section_values_t *s)
{
    if (had_previous_value) {
        if (NULL != s->name) {
            free(s->name);
        }
        if (NULL != s->vendor_ids) {
            free(s->vendor_ids);
        }
        if (NULL != s->vendor_part_ids) {
            free(s->vendor_part_ids);
        }
    }

    s->name = NULL;
    s->vendor_ids = NULL;
    s->vendor_ids_len = 0;
    s->vendor_part_ids = NULL;
    s->vendor_part_ids_len = 0;

    reset_values(&s->values);
}


/*
 * Reset the values to known states
 */
static void reset_values(opal_btl_iwarp_ini_values_t *v)
{
    v->mtu = 0;
    v->mtu_set = false;

    v->use_eager_rdma = 0;
    v->use_eager_rdma_set = false;

    v->receive_queues = NULL;

    v->max_inline_data = 0;
    v->max_inline_data_set = false;

    v->rdmacm_reject_causes_connect_error = false;
    v->rdmacm_reject_causes_connect_error_set = false;

    v->ignore_device        = false;
    v->ignore_device_set    = false;
}


/*
 * If we have a valid section, see if we have a matching section
 * somewhere (i.e., same vendor ID and vendor part ID).  If we do,
 * update the values.  If not, save the values in a new instance and
 * add it to the list.
 */
static int save_section(parsed_section_values_t *s)
{
    int i, j;
    device_values_t *h;
    bool found;

    /* Is the parsed section valid? */
    if (NULL == s->name || 0 == s->vendor_ids_len ||
        0 == s->vendor_part_ids_len) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Iterate over each of the vendor/part IDs in the parsed
       values */
    for (i = 0; i < s->vendor_ids_len; ++i) {
        for (j = 0; j < s->vendor_part_ids_len; ++j) {
            found = false;

            /* Iterate over all the saved devices */
            OPAL_LIST_FOREACH(h, &devices, device_values_t) {
                if (s->vendor_ids[i] == h->vendor_id &&
                    s->vendor_part_ids[j] == h->vendor_part_id) {
                    /* Found a match.  Update any newly-set values. */
                    if (s->values.mtu_set) {
                        h->values.mtu = s->values.mtu;
                        h->values.mtu_set = true;
                    }

                    if (s->values.use_eager_rdma_set) {
                        h->values.use_eager_rdma = s->values.use_eager_rdma;
                        h->values.use_eager_rdma_set = true;
                    }

                    if (NULL != s->values.receive_queues) {
                        h->values.receive_queues =
                            strdup(s->values.receive_queues);
                    }

                    if (s->values.max_inline_data_set) {
                        h->values.max_inline_data = s->values.max_inline_data;
                        h->values.max_inline_data_set = true;
                    }

                    if (s->values.rdmacm_reject_causes_connect_error_set) {
                        h->values.rdmacm_reject_causes_connect_error =
                            s->values.rdmacm_reject_causes_connect_error;
                        h->values.rdmacm_reject_causes_connect_error_set =
                            true;
                    }

                    if (s->values.ignore_device_set) {
                        h->values.ignore_device = s->values.ignore_device;
                        h->values.ignore_device_set = true;
                    }

                    found = true;
                    break;
                }
            }

            /* Did we find/update it in the exising list?  If not,
               create a new one. */
            if (!found) {
                h = OBJ_NEW(device_values_t);
                h->section_name = strdup(s->name);
                h->vendor_id = s->vendor_ids[i];
                h->vendor_part_id = s->vendor_part_ids[j];
                /* NOTE: There is a bug in the PGI 6.2 series that
                   causes the compiler to choke when copying structs
                   containing bool members by value.  So do a memcpy
                   here instead. */
                memcpy(&h->values, &s->values, sizeof(s->values));
                /* Need to strdup the string, though */
                if (NULL != s->values.receive_queues) {
                    h->values.receive_queues = strdup(s->values.receive_queues);
                }
                opal_list_append(&devices, &h->super);
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Do string-to-integer conversion, for both hex and decimal numbers
 */
int opal_btl_iwarp_ini_intify(char *str)
{
    return strtol (str, NULL, 0);
}


/*
 * Take a comma-delimited list and infity them all
 */
int opal_btl_iwarp_ini_intify_list(char *value, uint32_t **values, int *len)
{
    char *comma;
    char *str = value;

    *len = 0;

    /* Comma-delimited list of values */
    comma = strchr(str, ',');
    if (NULL == comma) {
        /* If we only got one value (i.e., no comma found), then
           just make an array of one value and save it */
        *values = (uint32_t *) malloc(sizeof(uint32_t));
        if (NULL == *values) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        *values[0] = (uint32_t) opal_btl_iwarp_ini_intify(str);
        *len = 1;
    } else {
        int newsize = 1;

        /* Count how many values there are and allocate enough space
           for them */
        while (NULL != comma) {
            ++newsize;
            str = comma + 1;
            comma = strchr(str, ',');
        }
        *values = (uint32_t *) malloc(sizeof(uint32_t) * newsize);
        if (NULL == *values) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* Iterate over the values and save them */
        str = value;
        comma = strchr(str, ',');
        while (NULL != comma) {
            *comma = '\0';
            (*values)[*len] = (uint32_t) opal_btl_iwarp_ini_intify(str);
            ++(*len);
            str = comma + 1;
            comma = strchr(str, ',');
        }
        /* Get the last value (i.e., the value after the last
           comma, because it won't have been snarfed in the
           loop) */
        (*values)[*len] = (uint32_t) opal_btl_iwarp_ini_intify(str);
        ++(*len);
    }

    return OPAL_SUCCESS;
}
