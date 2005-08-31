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

#include "ompi_config.h"
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/include/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "coll_tuned_dynamic_rules.h"

int static rule_count = 0;
int static cond_count = 0;

/* makes a rule */
/* this is hanging in space until linked to either other rules or a */
/* collective decision function */
rule_t* mk_rule () 
{
rule_t* ptr;

ptr = (rule_t*) calloc (1, sizeof(rule_t));
if (!ptr) {
    fprintf(stderr,"calloc on mk_rule failed!\n");
    exit (-1);
}

/* set values in the hanging rule */
ptr->rule_id = rule_count++;

return (ptr);
}

/* adds a condition to a rule */
int mk_and_add_condition_to_rule (rule_t* rule, param_index_t param,
                                    condition_op_t op, int target)
{
condition_t* ptr;
condition_t* last;

if (!rule) {
    fprintf(stderr,"rule given in add_condition_to_rule is NULL?!\n");
    return (-2);
}
if (param>=PARAMS) {
    fprintf(stderr,"param given in add_condition_to_rule is %d?!\n", param);
    return (-3);
}

ptr = (condition_t*) calloc (1, sizeof(condition_t));
if (!ptr) {
    fprintf(stderr,"calloc on add_condition_to_rule failed!\n");
    return (-5);
}

/* set values in the condition */
ptr->cond_id = cond_count++;
ptr->param = param;
ptr->op = op;
ptr->value = target;

/* set values in the rule */
if (rule->nconditions) { /* if we already have conditions add to the last one */
    last = rule->last_condition;
    last->next = ptr;
}
else { /* its the very first / head condition */
    rule->first_condition = ptr;
}

/* common to both all cases */
rule->nconditions++;
rule->last_condition = ptr;

return (0);
}

/* attaches a rules/collective functions TO a rule (not otherway round) */
int set_rule_links (rule_t * rule, ifp true_fptr, int* true_extraargs,
                    ifp false_fptr, int* false_extraargs, 
                    rule_t* true_rule, rule_t* false_rule)
{

if (!rule) {
    fprintf(stderr,"rule given in set_rule_links is NULL?!\n");
    return (-2);
}

/* check rule results.. we must have one set for true and one for false */
if ((true_fptr)&&(true_rule)) {
    fprintf(stderr,"BAD. Two links for TRUE on rule %d!\n", rule->rule_id);
    return (-6);
}
if ((false_fptr)&&(false_rule)) {
    fprintf(stderr,"BAD. Two links for FALSE on rule %d!\n", rule->rule_id);
    return (-7);
}
if ((!true_fptr)&&(!true_rule)) {
    fprintf(stderr,"BAD. NO links for TRUE on rule %d!\n", rule->rule_id);
    return (-8);
}
if ((!false_fptr)&&(!false_rule)) {
    fprintf(stderr,"BAD. NO links for FALSE on rule %d!\n", rule->rule_id);
    return (-9);
}

/* can set the links now */
rule->true_fptr = true_fptr;
rule->true_extraargs = true_extraargs;
rule->next_true_rule = (struct rules_s *) true_rule;
rule->false_fptr = false_fptr;
rule->false_extraargs = false_extraargs;
rule->next_false_rule = (struct rules_s *) false_rule;

return (0); 
}

/* free rule (and all attached conditions */
/* oneday multiple rules might be able to share conditions to save memory */
int free_rule (rule_t *rule)
{
condition_t* ptr;
condition_t* next;
int i;

if (!rule) {
    fprintf(stderr,"rule given in free_rule is NULL?!\n");
    return (-2);
}

/* free conditions first */
if (rule->nconditions) {
    ptr = rule->first_condition;
    for (i=0;i<rule->nconditions;i++) {
        next = ptr->next;
        free (ptr);
        ptr = next;
    }
}

/* all conditions freed, free the rule */
free (rule);

return (0);
}

/* evaluates a rule and returns the final function pointer that matches */
int eval_rule (rule_t *rule, params_t *params, ifp* fptr, int** extraargs)
{
rule_t* currentrule;
condition_t* currentcond;
int true=1;

if (!rule) {
    fprintf(stderr,"rule given in eval_rule is NULL?!\n");
    return (-2);
}

/* first special case is a very fast path... sorta not really grr */
if (!rule->nconditions) {
#ifdef VERBOSE
    printf("Rule %d has no conditions so forcing first available\n",
                    rule->rule_id);
#endif /* VERBOSE */
    *fptr = rule->true_fptr;
    return (0);
}

/* ok we have some conditions so start the evaluation */
/* make it as lazy as possible, so a single false condition bumps us */

currentrule = rule;

while (currentrule) { /* rules to evaluate */
#ifdef RULEVERBOSE
    printf("Eval Rule %d  ", currentrule->rule_id);
#endif

    /* eval each of the current rules conditions */
    /* we do this until we have either: */
    /* (a) completed all conditions and are still true */
    /* (b) found a single false */
    /* once we have either a true true or false we can then check */
    /* the next values and see if we are returning a function */
    /* OR skipping to the next rule and hense iterate */

    true = 1; /* we are ok so far */

    /* first get the first condition to eval */
    currentcond = currentrule->first_condition;

    while ((currentcond)&&(true)) { /* while conditions to eval */
#ifdef RULEVERBOSE
       printf("Eval Cond %d ", currentcond->cond_id);
#endif
       switch (currentcond->op) {
        case LT: if (params->values[currentcond->param] < currentcond->value) {true = 1;}
            else {true = 0;}
            break;
        case LTEQ: if (params->values[currentcond->param] <= currentcond->value) {true = 1;}
            else {true = 0;}
            break;
        case GT: if (params->values[currentcond->param] > currentcond->value) {true = 1;}
            else {true = 0;}
            break;
        case GTEQ: if (params->values[currentcond->param] >= currentcond->value) {true = 1;}
            else {true = 0;}
            break;
        case EQ: if (params->values[currentcond->param] == currentcond->value) {true = 1;}
            else {true = 0;}
            break;
        default:
            fprintf(stderr, "Eval: BAD operator of value %d rule %d cond %d\n",
                    currentcond->op, currentrule->rule_id,
                    currentcond->cond_id);
            true = 0;
            return (-1); /* ?! what else can I do, should have caught before */
       } /* switch on condition operator */

       /* if we are still true we go to the next condition if there is one */
       /* if there is not another then we are truely true */
       /* else if we are false, immediately fall out */

       if (!true) {
#ifdef RULEVERBOSE
       printf("Eval Cond %d returned FALSE\n", currentcond->cond_id);
#endif
           break; /* if false drop out asap */
       }
       if ((true)&&(currentcond->next)) {   /* next condition to check */
#ifdef RULEVERBOSE
       printf("Eval Cond %d returned TRUE. Moving to next\n", currentcond->cond_id);
#endif
           currentcond = currentcond->next;
       } 
       else { /* we are true with no more conditions to check */
#ifdef RULEVERBOSE
       printf("Eval Cond %d (LAST) returned TRUE.\n", currentcond->cond_id);
#endif
           break; /* so return so we can find out what to do next */
       }

    } /* while conditions to eval or a false */

    /* condition is either fully met or lazy false */
    /* we do these IFs in the fasted/most important order */
    if ((true)&&(currentrule->true_fptr)) {
#ifdef RULEVERBOSE
    printf("Eval Rule %d is TRUE returning fptr\n", currentrule->rule_id);
#endif
        *fptr = currentrule->true_fptr;
        *extraargs = currentrule->true_extraargs;
        return (0);
    }
    if ((!true)&&(currentrule->false_fptr)) {
#ifdef RULEVERBOSE
    printf("Eval Rule %d is FALSE returning fptr\n", currentrule->rule_id);
#endif
        *fptr = currentrule->false_fptr;
        *extraargs = currentrule->false_extraargs;
        return (0);
    }
    if (true) {
#ifdef RULEVERBOSE
    printf("Eval Rule %d is TRUE jumping to next rule %d\n", 
            currentrule->rule_id, 
            ((rule_t*)(currentrule->next_true_rule))->rule_id);
#endif
        currentrule = (rule_t *) currentrule->next_true_rule;
    }
    else { /* i.e. not true / lazy eval */
#ifdef RULEVERBOSE
    printf("Eval Rule %d is FALSE jumping to next rule %d\n", 
            currentrule->rule_id, 
            ((rule_t*)(currentrule->next_false_rule))->rule_id);
#endif
        currentrule = (rule_t *) currentrule->next_false_rule;
    }
   
   if (!currentrule) {
    fprintf(stderr, "eval: disaster, we have gone off into the weeds..  panic!\n");
    exit (-10);
   }
}


}


