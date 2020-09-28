/*
 * Copyright (c) 2020      Bull SAS. All rights reserved.
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
#include "base.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_base_topo.h"

/* need file reading function */
#include "coll_base_util.h"

/* also need the dynamic rule structures */
#include "coll_base_dynamic_rules.h"

/* and our own prototypes */
#include "coll_base_dynamic_file.h"

static int fileline=0; /* used for verbose error messages */

#define getnext(fptr, pval)        ompi_coll_base_file_getnext_long(fptr, &fileline, pval)

/*
 * Reads a rule file called fname
 * Builds the algorithm rule table for a max of n_collectives
 *
 * If an error occurs it removes rule table and then exits with a very verbose
 * error message (this stops the user using a half baked rule table
 *
 * Returns the number of actual collectives that a rule exists for
 * (note 0 is NOT an error)
 *
 */

int ompi_coll_base_read_rules_config_file (char *fname, int format_version, ompi_coll_base_alg_rule_t** rules, int n_collectives)
{
    FILE *fptr = (FILE*) NULL;
    long X, CI, NCS, CS, ALG, NMS, FANINOUT, nodes_rules_number, nodes_number;
    long MS, SS;
    int x, ncs, nms, node_rule;
    int ret = OMPI_SUCCESS;

    ompi_coll_base_alg_rule_t *alg_rules = (ompi_coll_base_alg_rule_t*) NULL;   /* complete table of rules */

    /* individual pointers to sections of rules */
    ompi_coll_base_alg_rule_t *alg_p     = (ompi_coll_base_alg_rule_t*) NULL;
    ompi_coll_base_nodes_rule_t *nodes_p = (ompi_coll_base_nodes_rule_t*) NULL;
    ompi_coll_base_com_rule_t *com_p     = (ompi_coll_base_com_rule_t*) NULL;
    ompi_coll_base_msg_rule_t *msg_p     = (ompi_coll_base_msg_rule_t*) NULL;

    /* stats info */
    int total_alg_count   = 0;
    int total_nodes_count = 0;
    int total_com_count   = 0;
    int total_msg_count   = 0;

    if (!fname) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Gave NULL as rule table configuration file for base collectives... ignoring!\n");
        ret = -1;
        goto on_file_error;
    }

    if (!rules) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Gave NULL as rule table result ptr!... ignoring!\n");
        ret = -2;
        goto on_file_error;
    }

    if (n_collectives<1) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Gave %d as max number of collectives in the rule table configuration file for base collectives!... ignoring!\n", n_collectives);
        ret = -3;
        goto on_file_error;
    }

    fptr = fopen (fname, "r");
    if (!fptr) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"cannot read rules file [%s]\n", fname);
        ret = -4;
        goto on_file_error;
    }

    switch (format_version) {
        case COMM_MSG_FORMAT:
            opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Reading dynamic rule format %d : <comm_size, msg_size>\n", COMM_MSG_FORMAT);
            break;
        case NODES_COMM_MSG_FORMAT:
            opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Reading dynamic rule format %d : <nodes_nb, comm_size, msg_size>\n", NODES_COMM_MSG_FORMAT);
            break;
        default:
            opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Format value %d is unknown\n", format_version);
            ret = -5;
            goto on_file_error;
            break;
    }

    /* make space and init the algorithm rules for each of the n_collectives MPI collectives */
    alg_rules = ompi_coll_base_mk_alg_rules (n_collectives);
    if (NULL == alg_rules) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"cannot cannot allocate rules (n=%d) for file [%s]\n", n_collectives, fname);
        ret = -6;
        goto on_file_error;
    }

    if( (getnext(fptr, &X) < 0) || (X < 0) ) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read number of collectives in configuration file around line %d\n", fileline);
        ret = -7;
        goto on_file_error;
    }
    if (X>n_collectives) {
        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Number of collectives in configuration file %ld is greater than number of MPI collectives possible %d ??? error around line %d\n", X, n_collectives, fileline);
        ret = -8;
        goto on_file_error;
    }

    for (x=0;x<X;x++) { /* for each collective */

        if( (getnext(fptr, &CI) < 0) || (CI < 0) ) {
            opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read next Collective id in configuration file around line %d\n", fileline);
            ret = -7;
            goto on_file_error;
        }
        if (CI>=n_collectives) {
            opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Collective id in configuration file %ld is greater than MPI collectives possible %d. Error around line %d\n", CI, n_collectives, fileline);
            ret = -8;
            goto on_file_error;
        }

        if (alg_rules[CI].alg_rule_id != CI) {
            opal_output_verbose(1,ompi_coll_base_framework.framework_output, "Internal error in handling collective ID %ld\n", CI);
            ret = -9;
            goto on_file_error;
        }
        opal_output_verbose(100,ompi_coll_base_framework.framework_output, "Reading dynamic rule for collective ID %ld\n", CI);
        alg_p = &alg_rules[CI];

        alg_p->alg_rule_id = CI;
        /* If format specify a nodes number */
        if (NODES_COMM_MSG_FORMAT == format_version) {
            if (getnext (fptr, &nodes_rules_number) < 0 || nodes_rules_number < 0) { ;
                opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read count of nodes number for collective ID %ld at around line %d\n", CI, fileline);
                ret = -7;
                goto on_file_error;
            }
            alg_p->n_nodes_sizes = nodes_rules_number;
        }
        else {
            nodes_rules_number = 1;
            alg_p->n_nodes_sizes = 1;
        }
        alg_p->nodes_rules = ompi_coll_base_mk_nodes_rules (nodes_rules_number, CI);

        for (node_rule = 0 ; node_rule < nodes_rules_number ; node_rule++) { /* for each nodes number */

            nodes_p = &(alg_p->nodes_rules[node_rule]);

            if (NODES_COMM_MSG_FORMAT == format_version) {
                if ( (getnext (fptr, &nodes_number) < 0) || (nodes_number < 0)){
                    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read nodes number for collective ID %ld node rule %d at around line %d\n", CI, node_rule, fileline);
                    ret = -7;
                    goto on_file_error;
                }
                nodes_p->nodes_number = nodes_number;
            }
            else {
                nodes_p->nodes_number = 1; /* Only one configuration - all cases */
            }

            if( (getnext (fptr, &NCS) < 0) || (NCS < 0) ) {
                opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read count of communicators for collective ID %ld node rule %d at around line %d\n", CI, node_rule, fileline);
                ret = -7;
                goto on_file_error;
            }
            opal_output_verbose(100,ompi_coll_base_framework.framework_output, "Read communicator count %ld for dynamic rule for collective ID %ld with node count %d\n", NCS, CI, nodes_p->nodes_number);
            nodes_p->n_com_sizes = NCS;
            nodes_p->com_rules = ompi_coll_base_mk_com_rules (NCS, CI, node_rule);

            for (ncs=0;ncs<NCS;ncs++) {	/* for each comm size */

                com_p = &(nodes_p->com_rules[ncs]);

                if( (getnext (fptr, &CS) < 0) || (CS < 0) ) {
                    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read communicator size for collective ID %ld com rule %d at around line %d\n", CI, ncs, fileline);
                    ret = -7;
                    goto on_file_error;
                }

                com_p->mpi_comsize = CS;

                if( (getnext (fptr, &NMS) < 0) || (NMS < 0) ) {
                    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read number of message sizes for collective ID %ld com rule %d at around line %d\n", CI, ncs, fileline);
                    ret = -7;
                    goto on_file_error;
                }
                opal_output_verbose(100,ompi_coll_base_framework.framework_output, "Read message count %ld for dynamic rule for collective ID %ld and comm size %ld\n",
                             NMS, CI, CS);
                com_p->n_msg_sizes = NMS;
                com_p->msg_rules = ompi_coll_base_mk_msg_rules (NMS, CI, node_rule, ncs, CS);

                for (nms=0;nms<NMS;nms++) {	/* for each msg size */

                    msg_p = &(com_p->msg_rules[nms]);

                    if( (getnext (fptr, &MS) < 0) || (MS < 0) ) {
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read message size for collective ID %ld com rule %d msg rule %d at around line %d\n", CI, ncs, nms, fileline);
                        ret = -7;
                        goto on_file_error;
                    }
                    msg_p->msg_size = (size_t)MS;

                    if( (getnext (fptr, &ALG) < 0) || (ALG < 0) ) {
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read target algorithm method for collective ID %ld com rule %d msg rule %d at around line %d\n", CI, ncs, nms, fileline);
                        ret = -7;
                        goto on_file_error;
                    }
                    msg_p->result_alg = ALG;

                    if( (getnext (fptr, &FANINOUT) < 0) || (FANINOUT < 0) ) {
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read fan in/out topo for collective ID %ld com rule %d msg rule %d at around line %d\n", CI, ncs, nms, fileline);
                        ret = -7;
                        goto on_file_error;
                    }
                    msg_p->result_topo_faninout = FANINOUT;

                    if( (getnext (fptr, &SS) < 0) || (SS < 0) ) {
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Could not read target segment size for collective ID %ld com rule %d msg rule %d at around line %d\n", CI, ncs, nms, fileline);
                        ret = -7;
                        goto on_file_error;
                    }
                    msg_p->result_segsize = SS;

                    if (!nms && MS) {
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"All algorithms must specify a rule for message size of zero upwards always first!\n");
                        opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Message size was %lu for collective ID %ld com rule %ld msg rule %d at around line %d\n", MS, CI, ncs, nms, fileline);
                        ret = -7;
                        goto on_file_error;
                    }

                    total_msg_count++;

                } /* msg size */

                total_com_count++;

            } /* comm size */

            total_nodes_count++;

        } /* nodes number */
        total_alg_count++;
        opal_output_verbose(100,ompi_coll_base_framework.framework_output, "Done reading dynamic rule for collective ID %ld\n", CI);

    } /* per collective */

    ret = total_alg_count;
    fclose (fptr);

    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"\nConfigure file Stats\n");
    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Collectives with rules\t\t\t: %5d\n", total_alg_count);
    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Nodes count with rules\t\t\t: %5d\n", total_nodes_count);
    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Communicator sizes with rules\t\t: %5d\n", total_com_count);
    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Message sizes with rules\t\t\t: %5d\n", total_msg_count);
    opal_output_verbose(10,ompi_coll_base_framework.framework_output,"Lines in configuration file read\t\t: %5d\n", fileline);

    /* return the rules to the caller */
    *rules = alg_rules;

    return ret;


 on_file_error:

    /* here we close out the file and delete any memory allocated nicely */
    /* we return back a verbose message and a count of -1 algorithms read */
    /* draconian but its better than having a bad collective decision table */

    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"read_rules_config_file: bad configure file [%s] which considered format is %d. Read afar as line %d\n"
                                                , fname, format_version, fileline);
    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Ignoring user supplied base collectives configuration decision file.\n");
    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Switching back to [compiled in] fixed decision table.\n");
    opal_output_verbose(1,ompi_coll_base_framework.framework_output,"Fix errors as listed above and try again.\n");

    /* deallocate memory if allocated */
    if (alg_rules) ompi_coll_base_free_all_rules (alg_rules, n_collectives);

    /* close file */
    if (fptr) fclose (fptr);

    *rules = (ompi_coll_base_alg_rule_t*) NULL;
    return ret;
}

