/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_TUNED_DYNAMIC_RULES_EXPORT_H
#define MCA_COLL_TUNED_DYNAMIC_RULES_EXPORT_H

#include "ompi_config.h"
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/include/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* type is prob not needed - GEF */
typedef enum {
    RULE,
    FPTR
} type_t;


typedef enum {
    LT,
    LTEQ,
    GT,
    GTEQ,
    EQ
} condition_op_t;


typedef enum {
    COMMSIZE,
    COMMPOWER2,
    DATASIZE,
    DATAPOWER2,
    DATACONTG,
    NONZEROROOT
}   param_index_t;

#define PARAMS 6

typedef struct {
    int values[PARAMS];
} params_t;



typedef struct condition_s {
    int cond_id;
    param_index_t param;
    condition_op_t op;
    int value;
    struct condition_s *next;
} condition_t;


typedef int (*ifp)();


typedef struct rule_s {
    int rule_id;
    /*     type_t type; */
    /* not sure if we need different types of RULEs yet? */
    /* maybe a faster single condition rule to avoid loops and more ifs */
    /* current eval thinks that a nconditions value of 0 = true fptr */
    int nconditions;
    /* we have a ptr to first and last just to speed up eval and add_to */
    condition_t *first_condition;
    condition_t *last_condition;
    ifp true_fptr;
    int* true_extraargs;
    ifp false_fptr;
    int* false_extraargs;
    struct rules_s* next_true_rule;
    struct rules_s* next_false_rule;
} rule_t;



rule_t* mk_rule ();

int mk_and_add_condition_to_rule (rule_t* rule, param_index_t param,
                                    condition_op_t op, int target);

int set_rule_links (rule_t * rule, ifp true_fptr, int* true_extraargs,
                    ifp false_fptr, int* false_extraargs,
                    rule_t* true_rule, rule_t* false_rule);

int free_rule (rule_t *rule);

int eval_rule (rule_t* rule, params_t* params, ifp* fptr, int** extraargs);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif



#endif /* MCA_COLL_TUNED_DYNAMIC_RULES_EXPORT_H */
