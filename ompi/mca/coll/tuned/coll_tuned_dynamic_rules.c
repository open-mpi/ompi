/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2025      Amazon.com, Inc. or its affiliates.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "coll_tuned.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "ompi/mca/coll/base/coll_base_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

#include <stdlib.h>
#include <stdio.h>

#include "ompi/mca/coll/base/coll_base_util.h"


ompi_coll_alg_rule_t* ompi_coll_tuned_mk_alg_rules (int n_alg)
{
    int i;
    ompi_coll_alg_rule_t* alg_rules;

    alg_rules = (ompi_coll_alg_rule_t *) calloc (n_alg, sizeof (ompi_coll_alg_rule_t));
    if (!alg_rules) return (alg_rules);

    /* set all we can at this point */
    for (i=0;i<n_alg;i++) {
        alg_rules[i].coll_id = i;
    }
    return (alg_rules);
}


ompi_coll_com_rule_t* ompi_coll_tuned_mk_com_rules (int n_com_rules, int coll_id)
{
    int i;
    ompi_coll_com_rule_t * com_rules;

    com_rules = (ompi_coll_com_rule_t *) calloc (n_com_rules, sizeof (ompi_coll_com_rule_t));
    if (!com_rules) return (com_rules);

    for (i=0;i<n_com_rules;i++) {
        com_rules[i].mpi_comsize_min = 0;   /* unknown */
        com_rules[i].mpi_comsize_max = INT_MAX;
        com_rules[i].comm_rank_distribution = COLL_RULES_DISTRO_ANY;
        com_rules[i].coll_id = coll_id;
        com_rules[i].n_rules = 0;   /* unknown */
        com_rules[i].msg_rules = (ompi_coll_msg_rule_t *) NULL;
    }
    return (com_rules);
}


ompi_coll_msg_rule_t* ompi_coll_tuned_mk_msg_rules (int n_msg_rules, int coll_id, int mpi_comsize)
{
    int i;
    ompi_coll_msg_rule_t *msg_rules;

    msg_rules = (ompi_coll_msg_rule_t *) calloc (n_msg_rules, sizeof (ompi_coll_msg_rule_t));
    if (!msg_rules) return (msg_rules);

    for( i = 0; i < n_msg_rules; i++ ) {
        msg_rules[i].coll_id = coll_id;
        msg_rules[i].msg_size_min = 0;           /* unknown */
        msg_rules[i].msg_size_max = COLL_RULES_MESSAGE_SIZE_INF;
        msg_rules[i].result_alg = 0;             /* unknown */
        msg_rules[i].result_topo_faninout = 0;   /* unknown */
        msg_rules[i].result_segsize = 0;         /* unknown */
        msg_rules[i].result_max_requests = 0;    /* unknown & default */
    }
    return (msg_rules);
}


/*
 * Debug / IO routines
 *
 */
int ompi_coll_tuned_dump_msg_rule (ompi_coll_msg_rule_t* msg_p)
{
    char max_msg_size[24]; // should only need 19+1 chars.
    char *alg_name;
    int rc;

    if (!msg_p) {
        opal_output(ompi_coll_tuned_stream, "ERROR: Message rule was a NULL ptr?!\n");
        return (-1);
    }
    if (msg_p->msg_size_max == COLL_RULES_MESSAGE_SIZE_INF) {
        snprintf(max_msg_size, 23, "%s", "Inf");
    } else {
        snprintf(max_msg_size, 23, "%lu", msg_p->msg_size_max);
    }
    max_msg_size[23] = '\0';

    if (msg_p->msg_size_max < msg_p->msg_size_min) {
        opal_output(ompi_coll_tuned_stream,"\t\t\tIGNORED RULE (check log)\n");
    } else {
        rc = coll_tuned_alg_to_str(msg_p->coll_id, msg_p->result_alg, &alg_name);
        if (rc != OPAL_SUCCESS) {
            alg_name = "ERROR_BAD_ALG_ID";
        }
        opal_output(ompi_coll_tuned_stream,"\t\t\tmsg_size %lu:%s -> %s (%2d).  Params: topo in/out %2d, segsize %5ld, max_requests %4d\n",
                    msg_p->msg_size_min, max_msg_size, alg_name, msg_p->result_alg, msg_p->result_topo_faninout, msg_p->result_segsize,
                    msg_p->result_max_requests);
        if (rc == OPAL_SUCCESS) {
            free(alg_name);
        }
    }

    return (0);
}


int ompi_coll_tuned_dump_com_rule (ompi_coll_com_rule_t* com_p)
{
    int i;

    if (!com_p) {
        opal_output(ompi_coll_tuned_stream,"\t\tERROR: Com rule was a NULL ptr?!\n");
        return (-1);
    }

    if (!com_p->n_rules) {
        opal_output(ompi_coll_tuned_stream,"\t\tno message size rules defined\n");
        return (0);
    }

    opal_output(ompi_coll_tuned_stream, "\t\t[%s] comm_size: %d:%d, distro:%s -> %d message-size rules",
        mca_coll_base_colltype_to_str(com_p->coll_id), com_p->mpi_comsize_min, com_p->mpi_comsize_max,
        coll_rules_comm_rank_distro_to_str(com_p->comm_rank_distribution), com_p->n_rules);

    for (i=0;i<com_p->n_rules;i++) {
        ompi_coll_tuned_dump_msg_rule (&(com_p->msg_rules[i]));
    }

    return (0);
}


int ompi_coll_tuned_dump_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
    int i;

    if (!alg_p) {
        opal_output(ompi_coll_tuned_stream,"ERROR: Algorithm rule was a NULL ptr?!\n");
        return (-1);
    }

    if (!alg_p->n_com_sizes) {
        opal_output(ompi_coll_tuned_stream,"\tno rules defined\n");
        return (0);
    }

    opal_output(ompi_coll_tuned_stream,"\tnumber of communicator rules: %d\n", alg_p->n_com_sizes);

    for (i=0;i<alg_p->n_com_sizes;i++) {
        ompi_coll_tuned_dump_com_rule (&(alg_p->com_rules[i]));
    }

    return (0);
}


int ompi_coll_tuned_dump_all_rules (ompi_coll_alg_rule_t* alg_p)
{
    int i;

    if (!alg_p) {
        opal_output(ompi_coll_tuned_stream,"Algorithm rule was a NULL ptr?!\n");
        return (-1);
    }

    for (i=0;i<COLLCOUNT;i++) {
        opal_output(ompi_coll_tuned_stream, "Rules for %s (id=%3d)\n", mca_coll_base_colltype_to_str(i), i);
        ompi_coll_tuned_dump_alg_rule( &alg_p[i] );
    }

    return (0);
}


/*
 * Memory free routines
 *
 */
int ompi_coll_tuned_free_msg_rules_in_com_rule (ompi_coll_com_rule_t* com_p)
{
    int rc=0;
    ompi_coll_msg_rule_t* msg_p;

    if (!com_p) {
        opal_output_verbose(1, ompi_coll_tuned_stream,
            "attempt to free NULL com_rule ptr\n");
        return (-1);
    }

    if (com_p->n_rules) {
        msg_p = com_p->msg_rules;

        if (!msg_p) {
            opal_output_verbose(1, ompi_coll_tuned_stream,
                "attempt to free NULL n_rules when msg count was %d\n", com_p->n_rules);
            rc = -1; /* some error */
        }
        else {
            /* ok, memory exists for the msg rules so free that first */
            free (com_p->msg_rules);
            com_p->msg_rules = (ompi_coll_msg_rule_t*) NULL;
        }

    } /* if we have msg rules to free as well */

    return (rc);
}


int ompi_coll_tuned_free_coms_in_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
    int rc=0;
    int i;

    ompi_coll_com_rule_t* com_p;

    if (!alg_p) {
        opal_output_verbose(1, ompi_coll_tuned_stream,"attempt to free NULL alg_rule ptr\n");
        return (-1);
    }

    if (alg_p->n_com_sizes) {
        com_p = alg_p->com_rules;

        if (!com_p) {
            opal_output_verbose(1, ompi_coll_tuned_stream,"attempt to free NULL com_rules when com count was %d\n", alg_p->n_com_sizes);
        } else {
            /* ok, memory exists for the com rules so free their message rules first */
            for( i = 0; i < alg_p->n_com_sizes; i++ ) {
                com_p = &(alg_p->com_rules[i]);
                ompi_coll_tuned_free_msg_rules_in_com_rule (com_p);
            }
            /* we are now free to free the com rules themselives */
            free (alg_p->com_rules);
            alg_p->com_rules = (ompi_coll_com_rule_t*) NULL;
        }

    } /* if we have msg rules to free as well */

    return (rc);
}


int ompi_coll_tuned_free_all_rules (ompi_coll_alg_rule_t* alg_p)
{
    int i;
    int rc = 0;

    for( i = 0; i < COLLCOUNT; i++ ) {
        rc += ompi_coll_tuned_free_coms_in_alg_rule (&(alg_p[i]));
    }

    free (alg_p);

    return (rc);
}

/*
 * query functions
 * i.e. the functions that get me the algorithm, topo fanin/out and segment size fast
 * and also get the rules that are needed by each communicator as needed
 *
 */

/*
 * This function is used to get the pointer to the nearest (less than or equal)
 * com rule for this MPI collective (coll_id) for a given
 * MPI communicator size. The complete rule base must be presented.
 *
 * If no rule exits returns NULL, else the com rule ptr
 * (which can be used in the coll_tuned_get_target_method_params() call)
 *
 */
ompi_coll_com_rule_t* ompi_coll_tuned_get_com_rule_ptr (ompi_coll_alg_rule_t* rules, int coll_id, struct ompi_communicator_t *comm)
{
    ompi_coll_alg_rule_t*  alg_p = (ompi_coll_alg_rule_t*) NULL;
    ompi_coll_com_rule_t*  com_p = (ompi_coll_com_rule_t*) NULL;
    ompi_coll_com_rule_t*  best_com_p = (ompi_coll_com_rule_t*) NULL;
    int i;
    int mpi_comsize;

    if (!rules) {                    /* no rule base no resulting com rule */
        return ((ompi_coll_com_rule_t*)NULL);
    }

    alg_p = &(rules[coll_id]); /* get the algorithm rule pointer */

    if (!alg_p->n_com_sizes) {   /* check for count of communicator sizes */
        return ((ompi_coll_com_rule_t*)NULL);    /* no com sizes so no rule */
    }

    if (OMPI_COMM_IS_INTER(comm)) {
        mpi_comsize = ompi_comm_remote_size(comm);
    } else {
        mpi_comsize = ompi_comm_size(comm);
    }

    for (i=0; i<alg_p->n_com_sizes; i++) {
        com_p = &alg_p->com_rules[i];
        if (com_p->comm_rank_distribution == COLL_RULES_DISTRO_DISJOINT) {
            if (!OMPI_COMM_IS_DISJOINT_SET(comm) || !OMPI_COMM_IS_DISJOINT(comm) ) {
                /* rule says disjoint, and comm is not disjoint, skip this rule */
                continue;
            }
        }
        if (com_p->comm_rank_distribution == COLL_RULES_DISTRO_SINGLENODE) {
            int local_peers = ompi_group_count_local_peers(comm->c_local_group);
            int all_peers = ompi_comm_size(comm);
            if (local_peers != all_peers) {
                /* rule says single-node, but we have non-local peers, skip this rule */
                continue;
            }
        }

        if (com_p->mpi_comsize_min <= mpi_comsize &&
                                      mpi_comsize <= com_p->mpi_comsize_max ) {
            best_com_p = com_p;
            break;
        }
    }

    if ( opal_output_check_verbosity(25, ompi_coll_tuned_stream) ) {
        if (!best_com_p) {
            opal_output( ompi_coll_tuned_stream, "coll:tuned:dynamic: For %s with communicator size %d "
                "no matching rule found.  Using fixed defaults.",
                mca_coll_base_colltype_to_str(coll_id), mpi_comsize);
        } else {
            opal_output(ompi_coll_tuned_stream,"coll:tuned:dynamic: For %s with communicator size %d selected rule number %d\n",
                mca_coll_base_colltype_to_str(coll_id), mpi_comsize, i+1);
            ompi_coll_tuned_dump_com_rule(best_com_p);
        }
    }

    return best_com_p;
}

/*
 * This function takes a com_rule ptr (from the communicators coll tuned data structure)
 * (Which is chosen for a particular MPI collective)
 * and a (total_)msg_size and it returns (0) and a algorithm to use and a recommended topo faninout and segment size
 * all based on the user supplied rules
 *
 * Just like the above functions it uses a less than or equal msg size
 * (hense config file must have a default defined for '0' if we reach this point)
 * else if no rules match we return '0' + '0,0' or used fixed decision table with no topo chand and no segmentation
 * of users data.. shame.
 *
 * On error return 0 so we default to fixed rules anyway :)
 *
 */

int ompi_coll_tuned_get_target_method_params (ompi_coll_com_rule_t* base_com_rule, size_t mpi_msgsize, int *result_topo_faninout,
                                              int* result_segsize, int* max_requests)
{
    ompi_coll_msg_rule_t*  msg_p = (ompi_coll_msg_rule_t*) NULL;
    ompi_coll_msg_rule_t*  best_msg_p = (ompi_coll_msg_rule_t*) NULL;
    int i;

    /* No rule or zero rules */
    if( (NULL == base_com_rule) || (0 == base_com_rule->n_rules)) {
        return (0);
    }

    /* search for the first comm rule that matches */
    for(i=0; i<base_com_rule->n_rules; i++) {
        msg_p = &base_com_rule->msg_rules[i];
        if (msg_p->msg_size_min <= mpi_msgsize &&
                                   mpi_msgsize <= msg_p->msg_size_max ) {
            best_msg_p = msg_p;
            break;
        }
    }

    if ( opal_output_check_verbosity(25, ompi_coll_tuned_stream) ) {
        if (!best_msg_p) {
            opal_output(ompi_coll_tuned_stream,
                "coll:tuned:dynamic For %s with msg_size=%lu: no matching rule, using fixed defaults.",
                mca_coll_base_colltype_to_str(msg_p->coll_id), mpi_msgsize);
        } else {
            opal_output(ompi_coll_tuned_stream,
                "coll:tuned:dynamic For %s with msg_size=%ld: selected rule number %d:",
                mca_coll_base_colltype_to_str(msg_p->coll_id), mpi_msgsize, i+1);
            ompi_coll_tuned_dump_msg_rule (best_msg_p);
        }
    }

    if (!best_msg_p) {
        /* no match, use defaults */
        return 0;
    }


    /* return the segment size */
    *result_topo_faninout = best_msg_p->result_topo_faninout;

    /* return the segment size */
    *result_segsize = best_msg_p->result_segsize;

    /* return the maximum requests */
    *max_requests = best_msg_p->result_max_requests;

    /* return the algorithm/method to use */
    return (best_msg_p->result_alg);
}

static const char* coll_rules_comm_rank_distro_table[] = {
    [COLL_RULES_DISTRO_ANY] = "any",
    [COLL_RULES_DISTRO_DISJOINT] = "one-per-node",
    [COLL_RULES_DISTRO_SINGLENODE] = "single-node",
};

const char* coll_rules_comm_rank_distro_to_str(enum comm_rank_distro_t distro)
{
    if( (distro < 0) || (distro >= COLL_RULES_DISTRO_COUNT) ) {
        return NULL;
    }
    return coll_rules_comm_rank_distro_table[distro];
}

int coll_rules_comm_rank_distro_from_str(const char *name, enum comm_rank_distro_t *distro)
{
    for (int i=0; i<COLL_RULES_DISTRO_COUNT; i++) {
        if (0 == strcmp(name, coll_rules_comm_rank_distro_table[i])) {
            *distro = i;
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}
