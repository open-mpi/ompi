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

static int ompi_coll_tuned_name_to_coll_id( const char *coll_name, int *coll_id ) {
    // TODO: implement me.
    *coll_id = 1;
    return 0;
}


static int ompi_coll_tuned_read_rules_json (const opal_json_t *json_root, ompi_coll_alg_rule_t** rules) {

    int rc = OPAL_ERROR;

    /* complete table of rules */
    ompi_coll_alg_rule_t *alg_rules = (ompi_coll_alg_rule_t*) NULL;

    /* individual pointers to sections of rules */
    ompi_coll_alg_rule_t *alg_p = (ompi_coll_alg_rule_t*) NULL;
    ompi_coll_com_rule_t *com_p = (ompi_coll_com_rule_t*) NULL;
    ompi_coll_msg_rule_t *msg_p = (ompi_coll_msg_rule_t*) NULL;


    alg_rules = ompi_coll_tuned_mk_alg_rules(COLLCOUNT);

    const opal_json_t *collectives_obj;
    size_t num_collectives = 0;
    size_t num_comm_rules;
    rc = opal_json_get_key(json_root, "collectives", &collectives_obj);
    // TODO err

    rc = opal_json_get_container_size(collectives_obj, &num_collectives);
    // TODO err

    for( size_t jcol = 0; jcol < num_collectives; jcol++ ) {
        const opal_json_t *comm_rule_array;
        const char* coll_name;
        int coll_id;
        rc = opal_json_get_key_by_index( collectives_obj, jcol, &coll_name, &comm_rule_array);
        // TODO: err
        rc = ompi_coll_tuned_name_to_coll_id( coll_name, &coll_id );
        // TODO: err

        alg_p = &alg_rules[coll_id];
        alg_p->alg_rule_id = coll_id;
        rc = opal_json_get_container_size(comm_rule_array, &num_comm_rules);
        // TODO: err
        alg_p->n_com_sizes = (int)num_comm_rules;
        alg_p->com_rules = ompi_coll_tuned_mk_com_rules (num_comm_rules, coll_id);

        for (size_t jcomm_rule=0; jcomm_rule < num_comm_rules; jcomm_rule++) {
            const opal_json_t *comm_rule;
            const opal_json_t *msg_size_array;
            size_t num_msg_rules;
            int64_t comm_size;
            rc = opal_json_get_index(comm_rule_array, jcomm_rule, &comm_rule);
            com_p = &(alg_p->com_rules[jcomm_rule]);
            rc = opal_json_read_integer("comm_size", &comm_size);
            rc = opal_json_get_key( comm_rule, "rules", &msg_size_array);
            rc = opal_json_get_container_size(msg_size_array, &num_msg_rules);

            for (size_t jmsg_rule=0; jmsg_rule < num_msg_rules; jmsg_rule++) {
                /* { "bytes" : 0, "alg" : 0, "reqs" : 20 } */
                const opal_json_t *msg_rule;
                rc = opal_json_get_index(msg_size_array, jmsg_rule, &msg_rule);
                msg_p = &(com_p->msg_rules[jmsg_rule]);
                int64_t msg_size, faninout;
                opal_json_read_integer(msg_rule, &msg_size);
                /* allow arg by string? */
                msg_p->msg_size = (size_t)msg_size;

                opal_json_free(&msg_rule);
            }

            opal_json_free(&msg_size_array);
            opal_json_free(&comm_rule);
        }

        opal_json_free(&comm_rule_array);
    }

    // TODO free collectives_obj
    return rc;
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
 * Returns the number of actual collectives that a rule exists for
 * (note 0 is NOT an error)
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
#if OPAL_ENABLE_DEBUG
    int total_com_count = 0;
    int total_msg_count = 0;
#endif

    if (!fname) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Gave NULL as rule table configuration file for tuned collectives... ignoring!\n"));
        return (-1);
    }

    if (!rules) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Gave NULL as rule table result ptr!... ignoring!\n"));
        return (-2);
    }

    fptr = fopen (fname, "r");
    if (!fptr) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Cannot read rules file [%s]\n", fname));
        goto on_file_error;
    }

    /* make space and init the algorithm rules for each of the n_collectives MPI collectives */
    alg_rules = ompi_coll_tuned_mk_alg_rules(COLLCOUNT);
    if (NULL == alg_rules) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Cannot allocate rules for file [%s]\n", fname));
        goto on_file_error;
    }

    /* consume the optional version identifier */
    if (0 == fscanf(fptr, "rule-file-version-%d", &version)) {
        version = 1;
    }

    /* get the number of collectives for which rules are provided in the file */
    if( (getnext(fptr, &NCOL) < 0) || (NCOL < 0) ) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read number of collectives in configuration file around line %d\n", fileline));
        goto on_file_error;
    }
    if (NCOL>COLLCOUNT) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Number of collectives in configuration file %ld is greater than number of MPI collectives possible %d ??? error around line %d\n", NCOL, COLLCOUNT, fileline));
        goto on_file_error;
    }

    for (x=0;x<NCOL;x++) { /* for each collective */

        /* get the collective for which rules are being provided */
        if( (getnext(fptr, &COLID) < 0) || (COLID < 0) ) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read next Collective id in configuration file around line %d\n", fileline));
            goto on_file_error;
        }
        if (COLID>=COLLCOUNT) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"Collective id in configuration file %ld is greater than MPI collectives possible %d. Error around line %d\n", COLID, COLLCOUNT, fileline));
            goto on_file_error;
        }

        if (alg_rules[COLID].alg_rule_id != COLID) {
            OPAL_OUTPUT((ompi_coll_tuned_stream, "Internal error in handling collective ID %ld\n", COLID));
            goto on_file_error;
        }
        OPAL_OUTPUT((ompi_coll_tuned_stream, "Reading dynamic rule for collective ID %ld\n", COLID));
        alg_p = &alg_rules[COLID];

        alg_p->alg_rule_id = COLID;
        alg_p->n_com_sizes = 0;
        alg_p->com_rules = (ompi_coll_com_rule_t *) NULL;

        /* get the number of communicator sizes for which a set of rules are to be provided */
        if( (getnext (fptr, &NCOMSIZES) < 0) || (NCOMSIZES < 0) ) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read count of communicators for collective ID %ld at around line %d\n", COLID, fileline));
            goto on_file_error;
        }
        OPAL_OUTPUT((ompi_coll_tuned_stream, "Read communicator count %ld for dynamic rule for collective ID %ld\n", NCOMSIZES, COLID));
        alg_p->n_com_sizes = NCOMSIZES;
        alg_p->com_rules = ompi_coll_tuned_mk_com_rules (NCOMSIZES, COLID);
        if (NULL == alg_p->com_rules) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"Cannot allocate com rules for file [%s]\n", fname));
            goto on_file_error;
        }

        for (ncs=0;ncs<NCOMSIZES;ncs++) {	/* for each comm size */

            com_p = &(alg_p->com_rules[ncs]);

            /* get the communicator size to associate the set of rules with */
            if( (getnext (fptr, &COMSIZE) < 0) || (COMSIZE < 0) ) {
                OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read communicator size for collective ID %ld com rule %d at around line %d\n", COLID, ncs, fileline));
                goto on_file_error;
            }

            com_p->mpi_comsize = COMSIZE;

            /* get the number of message sizes to specify rules for. inner set size */
            if( (getnext (fptr, &NMSGSIZES) < 0) || (NMSGSIZES < 0) ) {
                OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read number of message sizes for collective ID %ld com rule %d at around line %d\n", COLID, ncs, fileline));
                goto on_file_error;
            }
            OPAL_OUTPUT((ompi_coll_tuned_stream, "Read message count %ld for dynamic rule for collective ID %ld and comm size %ld\n",
                         NMSGSIZES, COLID, COMSIZE));
            com_p->n_msg_sizes = NMSGSIZES;
            com_p->msg_rules = ompi_coll_tuned_mk_msg_rules (NMSGSIZES, COLID, ncs, COMSIZE);
            if (NULL == com_p->msg_rules) {
                OPAL_OUTPUT((ompi_coll_tuned_stream,"Cannot allocate msg rules for file [%s]\n", fname));
                goto on_file_error;
            }

            msg_p = com_p->msg_rules;

            for (nms=0;nms<NMSGSIZES;nms++) {	/* for each msg size */

                msg_p = &(com_p->msg_rules[nms]);

                /* read the message size to associate the rule with */
                if( (getnext (fptr, &MSGSIZE) < 0) || (MSGSIZE < 0) ) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read message size for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline));
                    goto on_file_error;
                }
                msg_p->msg_size = (size_t)MSGSIZE;

                /* read the collective specific algorithm identifier */
                if( (getnext (fptr, &ALG) < 0) || (ALG < 0) ) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read target algorithm method for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline));
                    goto on_file_error;
                }
                msg_p->result_alg = ALG;

                /* read faninout tuning parameter. required */
                if( (getnext (fptr, &FANINOUT) < 0) || (FANINOUT < 0) ) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read fan in/out topo for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline));
                    goto on_file_error;
                }
                msg_p->result_topo_faninout = FANINOUT;

                /* read segsize tuning parameter. required */
                if( (getnext (fptr, &SEGSIZE) < 0) || (SEGSIZE < 0) ) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read target segment size for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline));
                    goto on_file_error;
                }
                msg_p->result_segsize = SEGSIZE;

                /* read the max requests tuning parameter. optional */
                msg_p->result_max_requests = ompi_coll_tuned_alltoall_max_requests;
                if( (version > 1) && isnext_digit(fptr) ) {
                    if( (getnext (fptr, &MAXREQ) < 0) || (MAXREQ < 0) ) {
                        OPAL_OUTPUT((ompi_coll_tuned_stream,"Could not read max requests for collective ID %ld com rule %d msg rule %d at around line %d\n", COLID, ncs, nms, fileline));
                        goto on_file_error;
                    }
                    msg_p->result_max_requests = MAXREQ;
                }

                /* check the first rule is for 0 size. look-up depends on this */
                if (!nms && MSGSIZE) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"All algorithms must specify a rule for message size of zero upwards always first!\n"));
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"Message size was %lu for collective ID %ld com rule %d msg rule %d at around line %d\n", MSGSIZE, COLID, ncs, nms, fileline));
                    goto on_file_error;
                }

#if OPAL_ENABLE_DEBUG
                total_msg_count++;
#endif

            } /* msg size */

#if OPAL_ENABLE_DEBUG
            total_com_count++;
#endif

        } /* comm size */

        total_alg_count++;
        OPAL_OUTPUT((ompi_coll_tuned_stream, "Done reading dynamic rule for collective ID %ld\n", COLID));

    } /* per collective */

    fclose (fptr);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"\nConfigure file Stats\n"));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Version\t\t\t\t\t: %5u\n", version));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Collectives with rules\t\t\t: %5d\n", total_alg_count));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Communicator sizes with rules\t\t: %5d\n", total_com_count));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Message sizes with rules\t\t: %5d\n", total_msg_count));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Lines in configuration file read\t\t: %5d\n", fileline));

    /* return the rules to the caller */
    *rules = alg_rules;

    return (total_alg_count);


 on_file_error:

    /* here we close out the file and delete any memory allocated nicely */
    /* we return back a verbose message and a count of -1 algorithms read */
    /* draconian but its better than having a bad collective decision table */

    OPAL_OUTPUT((ompi_coll_tuned_stream,"read_rules_config_file: bad configure file [%s]. Read afar as line %d\n", fname, fileline));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Ignoring user supplied tuned collectives configuration decision file.\n"));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Switching back to [compiled in] fixed decision table.\n"));
    OPAL_OUTPUT((ompi_coll_tuned_stream,"Fix errors as listed above and try again.\n"));

    /* deallocate memory if allocated */
    if (alg_rules) ompi_coll_tuned_free_all_rules (alg_rules);

    /* close file */
    if (fptr) fclose (fptr);

    *rules = (ompi_coll_alg_rule_t*) NULL;
    return (-1);
}

int ompi_coll_tuned_read_rules_config_file (char *fname, ompi_coll_alg_rule_t** rules) {
    if (!fname) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Gave NULL as rule table configuration file for tuned collectives... ignoring!\n"));
        return (-1);
    }

    if (!rules) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"Gave NULL as rule table result ptr!... ignoring!\n"));
        return (-2);
    }

    const opal_json_t *json;
    int ret = opal_json_load_file(fname, &json);
    if (ret == OPAL_SUCCESS) {
        ret = ompi_coll_tuned_read_rules_json(json, rules);
        opal_json_free(&json);
        return ret;
    } else {
        opal_output_verbose(20, ompi_coll_tuned_stream, "Failed to parse %s as valid json.  Assuming classic format.\n",fname);
        ret = ompi_coll_tuned_read_rules_config_file_classic(fname, rules);
        if (ret != OPAL_SUCCESS) {
            opal_output_verbose(1, ompi_coll_tuned_stream, "Failed to load %s in either json or classic readers.  Check format.\n",fname);
        }
        return ret;
    }
}