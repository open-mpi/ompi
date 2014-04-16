/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "coll_ml.h"
#include "coll_ml_inlines.h"
#include "coll_ml_config.h"
#include "coll_ml_lex.h"

static char *key_buffer = NULL;
static size_t key_buffer_len = 0;

typedef struct section_config_t {
    char *section_name;
    int section_id;
    per_collective_configuration_t config;
} section_config_t;

typedef struct coll_config_t {
    char *coll_name;
    int coll_id;
    section_config_t section;
} coll_config_t;

static int algorithm_name_to_id(char *name)
{
    assert (NULL != name);
    if (!strcasecmp(name,"ML_BCAST_SMALL_DATA_KNOWN"))
        return ML_BCAST_SMALL_DATA_KNOWN;
    if (!strcasecmp(name,"ML_BCAST_SMALL_DATA_UNKNOWN"))
        return ML_BCAST_SMALL_DATA_UNKNOWN;
    if (!strcasecmp(name,"ML_BCAST_SMALL_DATA_SEQUENTIAL"))
        return ML_BCAST_SMALL_DATA_SEQUENTIAL;
    if (!strcasecmp(name,"ML_BCAST_LARGE_DATA_KNOWN"))
        return ML_BCAST_LARGE_DATA_KNOWN;
    if (!strcasecmp(name,"ML_BCAST_LARGE_DATA_UNKNOWN"))
        return ML_BCAST_LARGE_DATA_UNKNOWN;
    if (!strcasecmp(name,"ML_BCAST_LARGE_DATA_SEQUENTIAL"))
        return ML_BCAST_LARGE_DATA_SEQUENTIAL;
    if (!strcasecmp(name,"ML_N_DATASIZE_BINS"))
        return ML_N_DATASIZE_BINS;
    if (!strcasecmp(name,"ML_NUM_BCAST_FUNCTIONS"))
        return ML_NUM_BCAST_FUNCTIONS;
    if (!strcasecmp(name,"ML_SCATTER_SMALL_DATA_KNOWN"))
        return ML_SCATTER_SMALL_DATA_KNOWN;
    if (!strcasecmp(name,"ML_SCATTER_N_DATASIZE_BINS"))
        return ML_SCATTER_N_DATASIZE_BINS;
    if (!strcasecmp(name,"ML_SCATTER_SMALL_DATA_UNKNOWN"))
        return ML_SCATTER_SMALL_DATA_UNKNOWN;
    if (!strcasecmp(name,"ML_SCATTER_SMALL_DATA_SEQUENTIAL"))
        return ML_SCATTER_SMALL_DATA_SEQUENTIAL;
    if (!strcasecmp(name,"ML_NUM_SCATTER_FUNCTIONS"))
        return ML_NUM_SCATTER_FUNCTIONS;
    if (!strcasecmp(name,"ML_SMALL_DATA_ALLREDUCE"))
        return ML_SMALL_DATA_ALLREDUCE;
    if (!strcasecmp(name,"ML_LARGE_DATA_ALLREDUCE"))
        return ML_LARGE_DATA_ALLREDUCE;
    if (!strcasecmp(name,"ML_SMALL_DATA_REDUCE"))
        return ML_SMALL_DATA_ALLREDUCE;
    if (!strcasecmp(name,"ML_LARGE_DATA_REDUCE"))
        return ML_LARGE_DATA_ALLREDUCE;
    if (!strcasecmp(name,"ML_SMALL_DATA_REDUCE"))
        return ML_SMALL_DATA_REDUCE;
    if (!strcasecmp(name,"ML_LARGE_DATA_REDUCE"))
        return ML_LARGE_DATA_REDUCE;
    if (!strcasecmp(name,"ML_NUM_ALLREDUCE_FUNCTIONS"))
        return ML_NUM_ALLREDUCE_FUNCTIONS;
    if (!strcasecmp(name,"ML_SMALL_DATA_ALLTOALL"))
        return ML_SMALL_DATA_ALLTOALL;
    if (!strcasecmp(name,"ML_LARGE_DATA_ALLTOALL"))
        return ML_LARGE_DATA_ALLTOALL;
    if (!strcasecmp(name,"ML_NUM_ALLTOALL_FUNCTIONS"))
        return ML_NUM_ALLTOALL_FUNCTIONS;
    if (!strcasecmp(name,"ML_SMALL_DATA_ALLGATHER"))
        return ML_SMALL_DATA_ALLGATHER;
    if (!strcasecmp(name,"ML_LARGE_DATA_ALLGATHER"))
        return ML_LARGE_DATA_ALLGATHER;
    if (!strcasecmp(name,"ML_NUM_ALLGATHER_FUNCTIONS"))
        return ML_NUM_ALLGATHER_FUNCTIONS;
    if (!strcasecmp(name,"ML_SMALL_DATA_GATHER"))
        return ML_SMALL_DATA_GATHER;
    if (!strcasecmp(name,"ML_LARGE_DATA_GATHER"))
        return ML_LARGE_DATA_GATHER;
    if (!strcasecmp(name,"ML_NUM_GATHER_FUNCTIONS"))
        return ML_NUM_GATHER_FUNCTIONS;
    if (!strcasecmp(name,"ML_BARRIER_DEFAULT"))
        return ML_BARRIER_DEFAULT;

    /* ERROR */
    return ML_UNDEFINED;
}

static int hierarchy_name_to_id(char *name)
{
    assert (NULL != name);
    if (!strcasecmp(name, "FULL_HR")) {
        return COLL_ML_HR_FULL;
    }
    if (!strcasecmp(name, "FULL_HR_NO_BASESOCKET")) {
        return COLL_ML_HR_NBS;
    }
    if (!strcasecmp(name, "PTP_ONLY")) {
        return COLL_ML_HR_SINGLE_PTP;
    }
    if (!strcasecmp(name, "IBOFFLOAD_ONLY")) {
        return COLL_ML_HR_SINGLE_IBOFFLOAD;
    }
    /* Error */
    return ML_UNDEFINED;
}

static int section_name_to_id(char *name)
{
    assert (NULL != name);
    if (!strcasecmp(name, "SMALL")) {
        return ML_SMALL_MSG;
    }

    if (!strcasecmp(name, "LARGE")) {
        return ML_LARGE_MSG;
    }
    /* Error */
    return ML_UNDEFINED;
}

static int coll_name_to_id(char *name)
{
    assert (NULL != name);
    if (!strcasecmp(name, "ALLGATHER")) {
        return ML_ALLGATHER;
    }
    if (!strcasecmp(name, "ALLGATHERV")) {
        return ML_ALLGATHERV;
    }
    if (!strcasecmp(name, "ALLREDUCE")) {
        return ML_ALLREDUCE;
    }
    if (!strcasecmp(name, "ALLTOALL")) {
        return ML_ALLTOALL;
    }
    if (!strcasecmp(name, "ALLTOALLV")) {
        return ML_ALLTOALLV;
    }
    if (!strcasecmp(name, "ALLTOALLW")) {
        return ML_ALLTOALLW;
    }
    if (!strcasecmp(name, "ALLTOALLW")) {
        return ML_ALLTOALLW;
    }
    if (!strcasecmp(name, "BARRIER")) {
        return ML_BARRIER;
    }
    if (!strcasecmp(name, "BCAST")) {
        return ML_BCAST;
    }
    if (!strcasecmp(name, "EXSCAN")) {
        return ML_EXSCAN;
    }
    if (!strcasecmp(name, "GATHER")) {
        return ML_GATHER;
    }
    if (!strcasecmp(name, "GATHERV")) {
        return ML_GATHERV;
    }
    if (!strcasecmp(name, "REDUCE")) {
        return ML_REDUCE;
    }
    if (!strcasecmp(name, "REDUCE_SCATTER")) {
        return ML_REDUCE_SCATTER;
    }
    if (!strcasecmp(name, "SCAN")) {
        return ML_SCAN;
    }
    if (!strcasecmp(name, "SCATTER")) {
        return ML_SCATTER;
    }
    if (!strcasecmp(name, "SCATTERV")) {
        return ML_SCATTERV;
    }

    /* nonblocking functions */

    if (!strcasecmp(name, "IALLGATHER")) {
        return ML_IALLGATHER;
    }
    if (!strcasecmp(name, "IALLGATHERV")) {
        return ML_IALLGATHERV;
    }
    if (!strcasecmp(name, "IALLREDUCE")) {
        return ML_IALLREDUCE;
    }
    if (!strcasecmp(name, "IALLTOALL")) {
        return ML_IALLTOALL;
    }
    if (!strcasecmp(name, "IALLTOALLV")) {
        return ML_IALLTOALLV;
    }
    if (!strcasecmp(name, "IALLTOALLW")) {
        return ML_IALLTOALLW;
    }
    if (!strcasecmp(name, "IALLTOALLW")) {
        return ML_IALLTOALLW;
    }
    if (!strcasecmp(name, "IBARRIER")) {
        return ML_IBARRIER;
    }
    if (!strcasecmp(name, "IBCAST")) {
        return ML_IBCAST;
    }
    if (!strcasecmp(name, "IEXSCAN")) {
        return ML_IEXSCAN;
    }
    if (!strcasecmp(name, "IGATHER")) {
        return ML_IGATHER;
    }
    if (!strcasecmp(name, "IGATHERV")) {
        return ML_IGATHERV;
    }
    if (!strcasecmp(name, "IREDUCE")) {
        return ML_IREDUCE;
    }
    if (!strcasecmp(name, "IREDUCE_SCATTER")) {
        return ML_IREDUCE_SCATTER;
    }
    if (!strcasecmp(name, "ISCAN")) {
        return ML_ISCAN;
    }
    if (!strcasecmp(name, "ISCATTER")) {
        return ML_ISCATTER;
    }
    if (!strcasecmp(name, "ISCATTERV")) {
        return ML_ISCATTERV;
    }

    /* Error - collecives name was not matched */
    return ML_UNDEFINED;
}
static int set_collective_name(coll_config_t *coll_config) 
{
    int coll_id = 
        coll_name_to_id(coll_ml_config_yytext);

    if (ML_UNDEFINED == coll_id) {
        return OMPI_ERROR;
    }

    coll_config->coll_id = coll_id;
    coll_config->coll_name = strdup(coll_ml_config_yytext);

    return OMPI_SUCCESS;
}

static int set_section_name(section_config_t *section_config) 
{
    int section_id;

    section_id = section_name_to_id(coll_ml_config_yytext);

    if (ML_UNDEFINED == section_id) {
        return OMPI_ERROR;
    }

    section_config->section_id = section_id;
    section_config->section_name = strdup(coll_ml_config_yytext);

    return OMPI_SUCCESS;
}

void mca_coll_ml_reset_config(per_collective_configuration_t *config) 
{
    config->topology_id = ML_UNDEFINED;
    config->threshold = ML_UNDEFINED;
    config->algorithm_id = ML_UNDEFINED;
    config->fragmentation_enabled = ML_UNDEFINED;
}

static void reset_section(section_config_t *section_cf)
{
    if (section_cf->section_name) {
        free (section_cf->section_name);
        section_cf->section_name = NULL;
    }

    section_cf->section_id = ML_UNDEFINED;
    mca_coll_ml_reset_config(&section_cf->config);
}

static void reset_collective(coll_config_t *coll_cf)
{
    if (coll_cf->coll_name) {
        free (coll_cf->coll_name);
        coll_cf->coll_name = NULL;
    }

    coll_cf->coll_id = ML_UNDEFINED;
    reset_section(&coll_cf->section);
}

/*
 * String to integer;
 */
static int string_to_int(char *str)
{
    while (isspace(*str)) {
        ++str;
    }

    /* Nope -- just decimal, so use atoi() */
    return atoi(str);
}

static int parse_algorithm_key(section_config_t *section, char *value)
{
    int ret;
    ret = algorithm_name_to_id(value);
    if (ML_UNDEFINED == ret) {
        return OMPI_ERROR;
    } else {
        section->config.algorithm_id = ret;
    }

    return OMPI_SUCCESS;
}

static int parse_threshold_key(section_config_t *section, char *value)
{
    assert (NULL != value);

    if(!strcasecmp(value, "unlimited")) {
        section->config.threshold = -1;
    } else {
        section->config.threshold = string_to_int(value);
    }

    return OMPI_SUCCESS;
}

static int parse_hierarchy_key(section_config_t *section, char *value)
{
    int ret;

    ret = hierarchy_name_to_id(value);
    if (ML_UNDEFINED == ret) {
        return OMPI_ERROR;
    }

    section->config.topology_id = ret;

    return OMPI_SUCCESS;
}

static int parse_fragmentation_key(section_config_t *section, char *value)
{
    assert (NULL != value);

    if(!strcasecmp(value, "enable")) {
        section->config.fragmentation_enabled = 1;
    } else if (!strcasecmp(value, "disable")) {
        section->config.fragmentation_enabled = 0;
    } else {
        ML_ERROR(("Line %d, unexpected fragmentation value %s. Legal values are: enable/disable",
                    coll_ml_config_yynewlines, value));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/* Save configuration that have been collected so far */
static int save_settings(coll_config_t *coll_config)
{
    per_collective_configuration_t *cf;

    if (ML_UNDEFINED == coll_config->coll_id || ML_UNDEFINED == coll_config->section.section_id) {
        return OMPI_ERROR;
    }

    cf = &mca_coll_ml_component.coll_config[coll_config->coll_id][coll_config->section.section_id];

    cf->topology_id = coll_config->section.config.topology_id;
    cf->threshold = coll_config->section.config.threshold; 
    cf->algorithm_id = coll_config->section.config.algorithm_id;
    cf->fragmentation_enabled = coll_config->section.config.fragmentation_enabled;

    return OMPI_SUCCESS;
}

/*
 * Parse a single line
 */
static int parse_line(section_config_t *section)
{
    int val, ret = OMPI_SUCCESS;
    char *value = NULL;

    /* Save the name name */
    if (key_buffer_len < strlen(coll_ml_config_yytext) + 1) {
        char *tmp;
        key_buffer_len = strlen(coll_ml_config_yytext) + 1;
        tmp = (char *) realloc(key_buffer, key_buffer_len);
        if (NULL == tmp) {
            free(key_buffer);
            key_buffer_len = 0;
            key_buffer = NULL;
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
        key_buffer = tmp;
    }
    strncpy(key_buffer, coll_ml_config_yytext, key_buffer_len);

    /* The first thing we have to see is an "=" */
    val = coll_ml_config_yylex();
    if (coll_ml_config_parse_done || COLL_ML_CONFIG_PARSE_EQUAL != val) {
        ML_ERROR(("Line %d, expected = before key: %s", 
                    coll_ml_config_yynewlines, 
                    key_buffer));
        return OMPI_ERROR;
    }

    /* Next we get the value */
    val = coll_ml_config_yylex();
    if (COLL_ML_CONFIG_PARSE_SINGLE_WORD == val ||
        COLL_ML_CONFIG_PARSE_VALUE == val) {
        value = strdup(coll_ml_config_yytext);

        /* Now we need to see the newline */
        val = coll_ml_config_yylex();
        if (COLL_ML_CONFIG_PARSE_NEWLINE != val &&
            COLL_ML_CONFIG_PARSE_DONE != val) {
            ML_ERROR(("Line %d, expected new line after %s",
                    coll_ml_config_yynewlines,
                    key_buffer));
            free(value);
            return OMPI_ERROR;
        }
    }

    /* If we did not get EOL or EOF, something is wrong */
    else if (COLL_ML_CONFIG_PARSE_DONE != val &&
             COLL_ML_CONFIG_PARSE_NEWLINE != val) {
        ML_ERROR(("Line %d, expected new line or end of line",
                    coll_ml_config_yynewlines));
        ret = OMPI_ERROR;
        goto Error;
    }

    /* Line parsing is done, read the values */
    if (!strcasecmp(key_buffer, "algorithm")) {
        ret = parse_algorithm_key(section, value);
        if (OMPI_SUCCESS != ret) {
            goto Error;
        }
    }

    else if (!strcasecmp(key_buffer, "threshold")) {
        ret = parse_threshold_key(section, value);
        if (OMPI_SUCCESS != ret) {
            goto Error;
        }
    }

    else if (!strcasecmp(key_buffer, "hierarchy")) {
        ret = parse_hierarchy_key(section, value);
        if (OMPI_SUCCESS != ret) {
            goto Error;
        }
    }

    else if (!strcasecmp(key_buffer, "fragmentation")) {
        ret = parse_fragmentation_key(section, value);
        if (OMPI_SUCCESS != ret) {
            goto Error;
        }
    /* Failed to parse the key */
    } else {
        ML_ERROR(("Line %d, unknown key %s",
                    coll_ml_config_yynewlines, key_buffer));
    }
    
    /* All done */
Error:
    if (NULL != value) {
        free(value);
    }

    return ret;
}

/**************************************************************************/

/*
 * Parse a single file
 */
static int parse_file(char *filename)
{
    int val;
    int ret = OMPI_SUCCESS;
    bool first_section = true, first_coll = true;
    coll_config_t coll_config;

    memset (&coll_config, 0, sizeof (coll_config));
    reset_collective(&coll_config);

    /* Open the file */
    coll_ml_config_yyin = fopen(filename, "r");
    if (NULL == coll_ml_config_yyin) {
        ML_ERROR(("Failed to open config file %s", filename));
        ret = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Do the parsing */
    coll_ml_config_parse_done = false;
    coll_ml_config_yynewlines = 1;
    coll_ml_config_init_buffer(coll_ml_config_yyin);
    while (!coll_ml_config_parse_done) {
        val = coll_ml_config_yylex();
        switch (val) {
        case COLL_ML_CONFIG_PARSE_DONE:
        case COLL_ML_CONFIG_PARSE_NEWLINE:
            break;
        case COLL_ML_CONFIG_PARSE_COLLECTIVE:
            /* dump all the information to last section that was defined */
            if (!first_coll) {
                ret = save_settings(&coll_config);

                if (OMPI_SUCCESS != ret) {
                    ML_ERROR(("Error in syntax for collective %s", coll_config.coll_name));
                    goto cleanup;
                }
            }
            
            /* reset collective config */
            reset_collective(&coll_config);

            first_coll    = false;
            first_section = true;

            ret = set_collective_name(&coll_config);
            if (OMPI_SUCCESS != ret) {
                goto cleanup;
            }
            break;
        case COLL_ML_CONFIG_PARSE_SECTION:
            if (ML_UNDEFINED == coll_config.coll_id) {
                ML_ERROR(("Collective section wasn't defined !"));
                ret = OMPI_ERROR;
                goto cleanup;
            }

            if (!first_section) {
                /* dump all the information to last section that was defined */
                ret = save_settings(&coll_config);
                if (OMPI_SUCCESS != ret) {
                    ML_ERROR(("Error in syntax for collective %s section %s", coll_config.coll_name,
                              coll_config.section.section_name));
                    goto cleanup;
                }
            }

            first_section = false;

            /* reset all section values */
            reset_section(&coll_config.section);

            /* set new section name */
            ret = set_section_name(&coll_config.section);
            if (OMPI_SUCCESS != ret) {
                goto cleanup;
            }
            break;
        case COLL_ML_CONFIG_PARSE_SINGLE_WORD:
            if (ML_UNDEFINED == coll_config.coll_id ||
                ML_UNDEFINED == coll_config.section.section_id) {
                ML_ERROR(("Collective section or sub-section was not defined !"));
                ret = OMPI_ERROR;
                goto cleanup;
            } else {
                parse_line(&coll_config.section);
            }
            break;

        default:
            /* anything else is an error */
            ML_ERROR(("Unexpected token!"));
            ret = OMPI_ERROR;
            goto cleanup;
            break;
        }
    }

    save_settings(&coll_config);
    fclose(coll_ml_config_yyin);
    coll_ml_config_yylex_destroy ();
    ret = OMPI_SUCCESS;

cleanup:
    reset_collective(&coll_config);
    if (NULL != key_buffer) {
        free(key_buffer);
        key_buffer = NULL;
        key_buffer_len = 0;
    }
    return ret;
}

int mca_coll_ml_config_file_init(void)
{
    return parse_file(mca_coll_ml_component.config_file_name);
}

