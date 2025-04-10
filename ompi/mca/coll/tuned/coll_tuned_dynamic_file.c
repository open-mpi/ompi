/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Amazon.com, Inc. or its affiliates.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdio.h>

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "coll_tuned.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "opal/util/json/opal_json.h"


/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "ompi/mca/coll/base/coll_base_topo.h"

/* need file reading function */
#include "ompi/mca/coll/base/coll_base_util.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

/* and our own prototypes */
#include "coll_tuned_dynamic_file.h"

static int fileline=0; /* used for verbose error messages */

#define getnext(fptr, pval)        ompi_coll_base_file_getnext_long(fptr, &fileline, pval)
#define isnext_digit(fptr)         ompi_coll_base_file_peek_next_char_isdigit(fptr)


static int coll_tuned_read_alg(const opal_json_t *msg_rule, ompi_coll_msg_rule_t *msg_p, int coll_id) {
    const char *RESULT_ALG_FIELD = "alg";
    const opal_json_t *alg_prop = NULL;
    int rc, rc_as_str, rc_as_int, rc_validation;
    const char* string_buf;
    size_t string_len;
    char int_as_str[24];
    int64_t int_val;

    rc = opal_json_get_key( msg_rule, RESULT_ALG_FIELD, &alg_prop);
    if (rc != OPAL_SUCCESS) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
                "The \"alg\" field is required, but was not found." );
        return OPAL_ERROR;
    }
    rc_as_str = opal_json_read_string(alg_prop, &string_buf, &string_len);
    rc_as_int = opal_json_read_integer(alg_prop, &int_val);
    opal_json_free(&alg_prop);
    if (rc_as_str == OPAL_SUCCESS) {
        rc_validation = coll_tuned_alg_from_str( coll_id, string_buf, &msg_p->result_alg );
    } else if (rc_as_int == OPAL_SUCCESS) {
        rc_validation = coll_tuned_alg_to_str( coll_id, int_val, NULL );
        if (rc_validation != OPAL_SUCCESS) {
            snprintf(int_as_str, 23, "%ld", int_val);
            int_as_str[23] = '\0';
            string_buf = int_as_str;
        } else {
            msg_p->result_alg = int_val;
        }
    } else {
        opal_output_verbose(1, ompi_coll_tuned_stream,
                "The \"alg\" field must be either a string or an integer, but it is something else." );
        return OPAL_ERROR;
    }
    if (rc_validation != OPAL_SUCCESS) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
        "Algorithm (%s) provided for collective \"%s\" is not valid.  "
        "Check documentation for valid configurations of coll_tuned_%s_algorithm.  Ignoring this rule.",
        string_buf, mca_coll_base_colltype_to_str(coll_id), mca_coll_base_colltype_to_str(coll_id) );
        /* the rationale for disabling the rule is to allow two ompi versions to use the same file. */
        /* disable the rule by making an impossible condition: */
        msg_p->msg_size_min = COLL_RULES_MESSAGE_SIZE_INF;
        msg_p->msg_size_max = 0;
        rc = OPAL_SUCCESS;
    }
    return OPAL_SUCCESS;
}

/* returns the value in the field.
   *rc is set as follows:
    OPAL_SUCCESS (value found and parsed)
    OPAL_ERROR   (value found, but not an integer)
    OPAL_ERR_DATA_VALUE_NOT_FOUND (value not found)

   When optional==0, and the field is not found, or when the field is not
   an integer, then a warning message is printed.
 */
static int coll_tuned_get_json_integer_field(const opal_json_t *parent,
                                                 const char* fieldname,
                                                 int optional,
                                                 int64_t *val ) {
    const opal_json_t *json_val;
    int rc;
    const char *string_buf;
    size_t str_len;
    int rc_as_int, rc_as_str;
    rc = opal_json_get_key( parent, fieldname, &json_val);
    if (rc == OPAL_SUCCESS) {
        rc_as_int = opal_json_read_integer(json_val, val);
        if ( rc_as_int != OPAL_SUCCESS ) {
            rc_as_str = opal_json_read_string(json_val, &string_buf, &str_len);
        }
        opal_json_free(&json_val);
        if (rc_as_int == OPAL_SUCCESS) {
            return rc_as_int;
        } else if ( rc_as_str == OPAL_SUCCESS) {
            if (strncmp("inf",string_buf, 3)==0 ||
                strncmp("Inf",string_buf, 3)==0 ||
                strncmp("INF",string_buf, 3)==0 ) {
                    *val = COLL_RULES_MESSAGE_SIZE_INF;
                    return rc_as_str;
            }
        }
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Found field \"%s\" as expected, but could not interpret it as an integer or \"inf\".",
            fieldname);
        return OPAL_ERROR;
    }
    if (optional == 0) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Missing the required field \"%s\".",
            fieldname);
    }
    return OPAL_ERR_DATA_VALUE_NOT_FOUND;
}

static int coll_tuned_read_message_size_rule(   ompi_coll_msg_rule_t *msg_p,
                                                const opal_json_t *msg_rule, int coll_id ) {
    int rc;
    const char *MSG_SIZE_FIELD = "msg_size_min";
    const char *MAX_MSG_SIZE_FIELD = "msg_size_max";
    const char *TOPO_FANINOUT_FIELD = "faninout";
    const char *SEGSIZE_FIELD = "seg_size";
    const char *MAX_REQUESTS_FIELD = "reqs";
    const int OPTIONAL = 1;
    // const int REQUIRED = 0;
    int64_t int_val;


#define OPTIONAL_READ(field, target) \
    rc = coll_tuned_get_json_integer_field(msg_rule, field, OPTIONAL, &int_val); \
    if (rc == OPAL_ERROR) {return rc;} \
    if (rc == OPAL_SUCCESS) {target = int_val;}
#define REQUIRED_READ(field, target) \
    rc = coll_tuned_get_json_integer_field(msg_rule, field, REQUIRED, &int_val); \
    if (rc != OPAL_SUCCESS) {return rc;} \
    if (rc == OPAL_SUCCESS) {target = int_val;}

    msg_p->msg_size_min = 0;
    OPTIONAL_READ( MSG_SIZE_FIELD, msg_p->msg_size_min )

    msg_p->msg_size_max = COLL_RULES_MESSAGE_SIZE_INF;
    OPTIONAL_READ( MAX_MSG_SIZE_FIELD, msg_p->msg_size_max )

    msg_p->result_topo_faninout = 0;
    OPTIONAL_READ( TOPO_FANINOUT_FIELD, msg_p->result_topo_faninout )

    msg_p->result_segsize = 0;
    OPTIONAL_READ( SEGSIZE_FIELD, msg_p->result_segsize )

    msg_p->result_max_requests = 0;
    OPTIONAL_READ( MAX_REQUESTS_FIELD, msg_p->result_max_requests )

    rc = coll_tuned_read_alg( msg_rule, msg_p, coll_id );

    return OPAL_SUCCESS;

#undef REQUIRED_READ
#undef OPTIONAL_READ
}

static int coll_tuned_get_comm_distribution(const opal_json_t *parent, enum comm_rank_distro_t *distro) {
    int rc;
    const opal_json_t *json_val;
    const char *string_buf;
    size_t string_len;
    const char *comm_rank_distro_name = "comm_rank_distribution";

    rc = opal_json_get_key( parent, comm_rank_distro_name, &json_val);
    if (rc == OPAL_ERROR) {
        *distro = COLL_RULES_DISTRO_ANY;
        return OPAL_SUCCESS;
    }
    rc = opal_json_read_string(json_val, &string_buf, &string_len);
    opal_json_free(&json_val);
    if ( rc == OPAL_SUCCESS) {
        rc = coll_rules_comm_rank_distro_from_str(string_buf, distro);

        if (rc == OPAL_ERROR) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                    "Unrecognized value for field \"%s\".  Got \"%s\", assuming \"%s\".",
                        comm_rank_distro_name, string_buf,
                        coll_rules_comm_rank_distro_to_str(COLL_RULES_DISTRO_ANY) );
            *distro = COLL_RULES_DISTRO_ANY;
            rc = OPAL_SUCCESS;
        }
    }
    return rc;
}


static int ompi_coll_tuned_read_rules_json (const opal_json_t *json_root, ompi_coll_alg_rule_t** rules) {

    int rc = OPAL_ERROR;

    /* complete table of rules */
    ompi_coll_alg_rule_t *alg_rules = (ompi_coll_alg_rule_t*) NULL;

    /* individual pointers to sections of rules */
    ompi_coll_alg_rule_t *alg_p = (ompi_coll_alg_rule_t*) NULL;
    ompi_coll_com_rule_t *com_p = (ompi_coll_com_rule_t*) NULL;
    ompi_coll_msg_rule_t *msg_p = (ompi_coll_msg_rule_t*) NULL;

    size_t jmsg_rule = 0;
    size_t jcomm_rule = 0;
    size_t jcol = 0;
    int64_t int_val;
    const char* coll_name = "";

    alg_rules = ompi_coll_tuned_mk_alg_rules(COLLCOUNT);

    const opal_json_t *collectives_obj = NULL;
    const opal_json_t *comm_rule_array = NULL;
    const opal_json_t *comm_rule = NULL;
    const opal_json_t *msg_size_array = NULL;

    size_t num_collectives = 0;
    size_t num_comm_rules = 0;
    rc = opal_json_get_key(json_root, "collectives", &collectives_obj);
    if (rc != OPAL_SUCCESS) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Required top-level field \"collectives\" was not found.");
        return OPAL_ERROR;
    }

    rc = opal_json_get_container_size(collectives_obj, &num_collectives);
    if (rc != OPAL_SUCCESS || OPAL_JSON_OBJECT != collectives_obj->type) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "The \"collectives\" field must be a dictionary of collectives.");
        opal_json_free(&collectives_obj);
        return OPAL_ERROR;
    }

    for(jcol = 0; jcol < num_collectives; jcol++ ) {
        int coll_id;
        rc = opal_json_get_key_by_index( collectives_obj, jcol, &coll_name, &comm_rule_array);
        if (rc) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Internal json error when attempting to parse the collective at index %ld\n",
                jcol);
            goto error_cleanup;
        }
        coll_id = mca_coll_base_name_to_colltype(coll_name);
        if (coll_id < 0) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Unrecognized collective: \"%s\". Use all lowercase such as \"allgather\"",
                coll_name);
            goto error_cleanup;
        }

        alg_p = &alg_rules[coll_id];
        alg_p->coll_id = coll_id;
        rc = opal_json_get_container_size(comm_rule_array, &num_comm_rules);
        if (rc != OPAL_SUCCESS || OPAL_JSON_ARRAY != comm_rule_array->type) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Problem parsing the collective at index %ld (for %s).  Expected an array of comm-related rules.",
                jcol, mca_coll_base_colltype_to_str(coll_id) );
            goto error_cleanup;
        }
        alg_p->n_com_sizes = (int)num_comm_rules;
        alg_p->com_rules = ompi_coll_tuned_mk_com_rules (num_comm_rules, coll_id);

        for (jcomm_rule=0; jcomm_rule < num_comm_rules; jcomm_rule++) {
            size_t num_msg_rules;
            rc = opal_json_get_index(comm_rule_array, jcomm_rule, &comm_rule);
            com_p = &(alg_p->com_rules[jcomm_rule]);

            com_p->mpi_comsize_min = 0;
            rc = coll_tuned_get_json_integer_field(comm_rule, "comm_size_min", 1, &int_val);
            if (rc == OPAL_ERROR) { goto error_bad_comm_rule; }
            if (rc == OPAL_SUCCESS) { com_p->mpi_comsize_min = int_val; }


            com_p->mpi_comsize_max = INT_MAX;
            rc = coll_tuned_get_json_integer_field(comm_rule, "comm_size_max", 1, &int_val);
            if (rc == OPAL_ERROR) { goto error_bad_comm_rule; }
            if (rc == OPAL_SUCCESS) { com_p->mpi_comsize_max = int_val; }

            rc = coll_tuned_get_comm_distribution(comm_rule, &com_p->comm_rank_distribution);
            if (rc == OPAL_ERROR) { goto error_bad_comm_rule; }

            rc = opal_json_get_key( comm_rule, "rules", &msg_size_array);
            if (rc != OPAL_SUCCESS) {
                opal_output_verbose(1, ompi_coll_tuned_stream,
                    "Expected a set of message rules in this communicator rule");
                goto error_bad_comm_rule;
            }
            rc = opal_json_get_container_size(msg_size_array, &num_msg_rules);
            if (rc != OPAL_SUCCESS) { goto error_bad_comm_rule; }

            com_p->n_rules = num_msg_rules;
            com_p->msg_rules = ompi_coll_tuned_mk_msg_rules (num_msg_rules, coll_id, com_p->mpi_comsize_min);


            for (jmsg_rule=0; jmsg_rule < num_msg_rules; jmsg_rule++) {
                const opal_json_t *msg_rule;
                msg_p = &(com_p->msg_rules[jmsg_rule]);
                rc = opal_json_get_index(msg_size_array, jmsg_rule, &msg_rule);

                rc = coll_tuned_read_message_size_rule( msg_p, msg_rule, coll_id);
                opal_json_free(&msg_rule);
                if (rc != OPAL_SUCCESS) {
                    goto error_bad_message_rule;
                }
            }

            opal_json_free(&msg_size_array);
            opal_json_free(&comm_rule);
        }

        opal_json_free(&comm_rule_array);
    }
    *rules = alg_rules;

    opal_json_free(&collectives_obj);
    return rc;
error_bad_message_rule:
    opal_output_verbose(1, ompi_coll_tuned_stream,
        "Problem occurred within collective %s, "
        "comm_size_min=%d, comm_size_max=%d (entry number %ld in the comm_size array), "
        "message size entry number %ld.", coll_name, com_p->mpi_comsize_min, com_p->mpi_comsize_max, 1+jcomm_rule, 1+jmsg_rule);
    goto error_cleanup;
error_bad_comm_rule:
    opal_output_verbose(1, ompi_coll_tuned_stream,
        "Problem occurred within collective %s, "
        "in entry number %ld of the comm_size array", coll_name, 1+jcomm_rule);
    goto error_cleanup;
error_cleanup:
    opal_json_free(&msg_size_array);
    opal_json_free(&comm_rule);
    opal_json_free(&comm_rule_array);
    opal_json_free(&collectives_obj);
    return OMPI_ERROR;
}

/*
 * Reads a rule file called fname
 * The rule file defines a set of sets of rules. The outer set is keyed on
 * communicator size while the inner set is keyed on message size.  When a
 * communicator is constructed its size is used to look up the nested set of
 * message size keyed rules.  When a collective is called the message size
 * determined from its call arguments are used to lookup a specific rule in the
 * inner set.
 *
 * Rules for communicator and message sizes 0 and N (where N is the larger than
 * largest key you provide) can be specified to fall back to the fixed decision
 * framework above and below the communicator and message size ranges of
 * interest.
 *
 * If an error occurs it removes rule table and then exits with a very verbose
 * error message. this stops the user using a half baked rule table.
 *
 * Returns OPAL_ERROR or OPAL_SUCCESS
 *
 */

static int ompi_coll_tuned_read_rules_config_file_classic (char *fname, ompi_coll_alg_rule_t** rules)
{
    long NCOL = 0,      /* number of collectives for which rules are provided  */
         COLID = 0,     /* identifies the collective type to associate the rules with */
         NCOMSIZES = 0, /* number of sets of message size rules. the key is communicator size */
         COMSIZE = 0,   /* communicator size, the key identifying a specific set of message size rules. */
         NMSGSIZES = 0, /* number of message size rules in the set. */
         MSGSIZE = 0,   /* message size, the key identifying a specific rule in the set. */
         ALG = 0,       /* the collective specific algorithm to use */
         FANINOUT = 0,  /* algorithm specific tuning parameter */
         SEGSIZE = 0,   /* algorithm specific tuning parameter */
         MAXREQ = 0;    /* algorithm specific tuning parameter */
    FILE *fptr = (FILE*) NULL;
    int x, ncs, nms, version;

    ompi_coll_alg_rule_t *alg_rules = (ompi_coll_alg_rule_t*) NULL;   /* complete table of rules */

    /* individual pointers to sections of rules */
    ompi_coll_alg_rule_t *alg_p = (ompi_coll_alg_rule_t*) NULL;
    ompi_coll_com_rule_t *com_p = (ompi_coll_com_rule_t*) NULL;
    ompi_coll_msg_rule_t *msg_p = (ompi_coll_msg_rule_t*) NULL;

    /* stats info */
    int total_alg_count = 0;
    int total_com_count = 0;
    int total_msg_count = 0;

    if (!fname) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Gave NULL as rule table configuration file for tuned collectives... ignoring!\n");
        return OPAL_ERROR;
    }

    if (!rules) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Gave NULL as rule table result ptr!... ignoring!\n");
        return OPAL_ERROR;
    }

    fptr = fopen (fname, "r");
    if (!fptr) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Cannot read rules file [%s]\n", fname);
        goto on_file_error;
    }

    /* make space and init the algorithm rules for each of the n_collectives MPI collectives */
    alg_rules = ompi_coll_tuned_mk_alg_rules(COLLCOUNT);
    if (NULL == alg_rules) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Cannot allocate rules for file [%s]\n", fname);
        goto on_file_error;
    }

    /* consume the optional version identifier */
    if (0 == fscanf(fptr, "rule-file-version-%d", &version)) {
        version = 1;
    }

    /* get the number of collectives for which rules are provided in the file */
    if( (getnext(fptr, &NCOL) < 0) || (NCOL < 0) ) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Could not read number of collectives in configuration file around line %d\n", fileline);
        goto on_file_error;
    }
    if (NCOL>COLLCOUNT) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Number of collectives in configuration file %ld is greater than number of MPI collectives possible %d ??? error around line %d\n",
            NCOL, COLLCOUNT, fileline);
        goto on_file_error;
    }

    for (x=0;x<NCOL;x++) { /* for each collective */

        /* get the collective for which rules are being provided */
        if( (getnext(fptr, &COLID) < 0) || (COLID < 0) ) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Could not read next Collective id in configuration file around line %d\n", fileline);
            goto on_file_error;
        }
        if (COLID>=COLLCOUNT) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Collective id in configuration file %ld is greater than MPI collectives possible %d. Error around line %d\n", COLID, COLLCOUNT, fileline);
            goto on_file_error;
        }

        if (alg_rules[COLID].coll_id != COLID) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Internal error in handling collective ID %ld\n", COLID);
            goto on_file_error;
        }
        opal_output_verbose(25, ompi_coll_tuned_stream,
            "Reading dynamic rule for collective ID %ld\n", COLID);
        alg_p = &alg_rules[COLID];

        alg_p->coll_id = COLID;
        alg_p->n_com_sizes = 0;
        alg_p->com_rules = (ompi_coll_com_rule_t *) NULL;

        /* get the number of communicator sizes for which a set of rules are to be provided */
        if( (getnext (fptr, &NCOMSIZES) < 0) || (NCOMSIZES < 0) ) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Could not read count of communicators for collective ID %ld at around line %d\n", COLID, fileline);
            goto on_file_error;
        }
        opal_output_verbose(25, ompi_coll_tuned_stream,
            "Read communicator count %ld for dynamic rule for collective ID %ld\n", NCOMSIZES, COLID);
        if( NCOMSIZES > INT_MAX) {
            opal_output_verbose(25, ompi_coll_tuned_stream,
                "Refusing to proceed: suspiciously large number found for the number of communicator-based rules: %ld\n", NCOMSIZES);
            goto on_file_error;
        }
        alg_p->n_com_sizes = NCOMSIZES;
        alg_p->com_rules = ompi_coll_tuned_mk_com_rules (NCOMSIZES, COLID);
        if (NULL == alg_p->com_rules) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "Cannot allocate com rules for file [%s]\n", fname);
            goto on_file_error;
        }

        for (ncs=0;ncs<NCOMSIZES;ncs++) {	/* for each comm size */

            com_p = &(alg_p->com_rules[ncs]);

            /* get the communicator size to associate the set of rules with */
            if( (getnext (fptr, &COMSIZE) < 0) || (COMSIZE < 0) ) {
                opal_output_verbose(1, ompi_coll_tuned_stream,
                    "Could not read communicator size for collective ID %ld com rule %d at around line %d\n", COLID, ncs, fileline);
                goto on_file_error;
            }

            if (ncs > 0) {
                com_p->mpi_comsize_min = alg_p->com_rules[ncs-1].mpi_comsize_max + 1;
            } else {
                com_p->mpi_comsize_min = 0;
            }
            if (ncs == NCOMSIZES-1) {
                com_p->mpi_comsize_max = INT_MAX;
            } else {
                com_p->mpi_comsize_max = COMSIZE - 1;
            }

            /* get the number of message sizes to specify rules for. inner set size */
            if( (getnext (fptr, &NMSGSIZES) < 0) || (NMSGSIZES < 0) ) {
                opal_output_verbose(1, ompi_coll_tuned_stream,
                    "Could not read number of message sizes for collective ID %ld com rule %d at around line %d\n", COLID, ncs, fileline);
                goto on_file_error;
            }
            opal_output_verbose(25, ompi_coll_tuned_stream,
                "Read message count %ld for dynamic rule for collective ID %ld and comm size %ld\n",
                         NMSGSIZES, COLID, COMSIZE);
            com_p->n_rules = NMSGSIZES;
            com_p->msg_rules = ompi_coll_tuned_mk_msg_rules (NMSGSIZES, COLID, COMSIZE);
            if (NULL == com_p->msg_rules) {
                opal_output_verbose(1, ompi_coll_tuned_stream,
                    "Cannot allocate msg rules for file [%s]\n", fname);
                goto on_file_error;
            }

            msg_p = com_p->msg_rules;

            for (nms=0;nms<NMSGSIZES;nms++) {	/* for each msg size */

                msg_p = &(com_p->msg_rules[nms]);

                /* read the message size to associate the rule with */
                if( (getnext (fptr, &MSGSIZE) < 0) || (MSGSIZE < 0) ) {
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "Could not read message size for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline);
                    goto on_file_error;
                }
                msg_p->msg_size_min = (size_t)MSGSIZE;
                msg_p->msg_size_max = COLL_RULES_MESSAGE_SIZE_INF;
                if (nms > 0) {
                    com_p->msg_rules[nms-1].msg_size_max = msg_p->msg_size_min - 1;
                }

                /* read the collective specific algorithm identifier */
                if( (getnext (fptr, &ALG) < 0) || (ALG < 0) ) {
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "Could not read target algorithm method for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline);
                    goto on_file_error;
                }
                msg_p->result_alg = ALG;

                /* read faninout tuning parameter. required */
                if( (getnext (fptr, &FANINOUT) < 0) || (FANINOUT < 0) ) {
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "Could not read fan in/out topo for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline);
                    goto on_file_error;
                }
                msg_p->result_topo_faninout = FANINOUT;

                /* read segsize tuning parameter. required */
                if( (getnext (fptr, &SEGSIZE) < 0) || (SEGSIZE < 0) ) {
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "Could not read target segment size for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline);
                    goto on_file_error;
                }
                msg_p->result_segsize = SEGSIZE;

                /* read the max requests tuning parameter. optional */
                msg_p->result_max_requests = ompi_coll_tuned_alltoall_max_requests;
                if( (version > 1) && isnext_digit(fptr) ) {
                    if( (getnext (fptr, &MAXREQ) < 0) || (MAXREQ < 0) ) {
                        opal_output_verbose(1, ompi_coll_tuned_stream,
                            "Could not read max requests for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline);
                        goto on_file_error;
                    }
                    msg_p->result_max_requests = MAXREQ;
                }

                /* check the first rule is for 0 size. look-up depends on this */
                if (!nms && MSGSIZE) {
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "All algorithms must specify a rule for message size of zero upwards always first!\n");
                    opal_output_verbose(1, ompi_coll_tuned_stream,
                        "Message size was %lu for collective ID %ld com rule %d msg rule %d at around line %d\n", MSGSIZE, COLID, ncs, nms, fileline);
                    goto on_file_error;
                }
                total_msg_count++;
            } /* msg size */
            total_com_count++;
        } /* comm size */

        total_alg_count++;
        opal_output_verbose(25, ompi_coll_tuned_stream,
            "Done reading dynamic rule for collective ID %ld\n", COLID);

    } /* per collective */

    fclose (fptr);

    opal_output_verbose(25, ompi_coll_tuned_stream,
        "\nConfigure file Stats\n");
    opal_output_verbose(25, ompi_coll_tuned_stream,
        "Version\t\t\t\t\t: %5u\n", version);
    opal_output_verbose(25, ompi_coll_tuned_stream,
        "Collectives with rules\t\t\t: %5d\n", total_alg_count);
    opal_output_verbose(25, ompi_coll_tuned_stream,
        "Communicator sizes with rules\t\t: %5d\n", total_com_count);
    opal_output_verbose(25, ompi_coll_tuned_stream,
        "Message sizes with rules\t\t: %5d\n", total_msg_count);
    opal_output_verbose(25, ompi_coll_tuned_stream,
        "Lines in configuration file read\t\t: %5d\n", fileline);

    /* return the rules to the caller */
    *rules = alg_rules;

    return OPAL_SUCCESS;


 on_file_error:

    /* here we close out the file and delete any memory allocated nicely */
    /* we return back a verbose message and a count of -1 algorithms read */
    /* draconian but its better than having a bad collective decision table */

    opal_output_verbose(1, ompi_coll_tuned_stream,
        "read_rules_config_file: bad configure file [%s]. Read afar as line %d\n", fname, fileline);
    opal_output_verbose(1, ompi_coll_tuned_stream,
        "Ignoring user supplied tuned collectives configuration decision file.\n");
    opal_output_verbose(1, ompi_coll_tuned_stream,
        "Switching back to [compiled in] fixed decision table.\n");
    opal_output_verbose(1, ompi_coll_tuned_stream,
        "Fix errors as listed above and try again.\n");

    /* deallocate memory if allocated */
    if (alg_rules) ompi_coll_tuned_free_all_rules (alg_rules);

    /* close file */
    if (fptr) fclose (fptr);

    *rules = (ompi_coll_alg_rule_t*) NULL;
    return OPAL_ERROR;
}

/**
 * Reads a rule file called fname
 *
 * In Open MPI 6.0 we introduced json-based rule file, but we continue to read
 * the original format for now.
 *
 * This funtion attempts a json read, and if it fails, falls back to classic
 * read.  If both fail, we fall back to fixed rules.
 *
 * Errors will be entirely hidden from the user unless coll_base_verbose is set
 * to at least 1.
 *
 * With coll_base_verbose set to 25, rules file is dumped to output stream.
 *
 * Returns OPAL_ERROR or OPAL_SUCCESS
 */

int ompi_coll_tuned_read_rules_config_file (char *fname, ompi_coll_alg_rule_t** rules) {
    if (!fname) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Gave NULL as rule table configuration file for tuned collectives... ignoring!\n");
        return OPAL_ERROR;
    }

    if (!rules) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "Gave NULL as rule table result ptr!... ignoring!\n");
        return OPAL_ERROR;
    }

    const opal_json_t *json;
    opal_output_verbose(1, ompi_coll_tuned_stream, "Attempting to read tuned rules as JSON...\n");
    int ret = opal_json_load_file(fname, &json, 0);
    if (ret == OPAL_SUCCESS) {
        ret = ompi_coll_tuned_read_rules_json(json, rules);
        opal_json_free(&json);
        if (ret != OPAL_SUCCESS) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "ERROR: %s is valid json, but there were errors reading the rules from the file. "
                "Falling back to default tuning and ignoring the file.\n", fname);
        }
    } else {
        opal_output_verbose(1, ompi_coll_tuned_stream, "Failed to parse %s as valid json.  Assuming classic format.\n",fname);
        ret = ompi_coll_tuned_read_rules_config_file_classic(fname, rules);
        if (ret != OPAL_SUCCESS) {
            opal_output_verbose(1, ompi_coll_tuned_stream, "Failed to load %s in either json or classic readers.  Check format.\n",fname);
        }
    }

    if (ret == OPAL_SUCCESS && opal_output_check_verbosity( 2, ompi_coll_tuned_stream)) {
        opal_output_verbose( 2, ompi_coll_tuned_stream, "Dumping rules:\n");
        ompi_coll_tuned_dump_all_rules(*rules);
    }
    return ret;

}