
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_tuned_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

#include <stdlib.h>
#include <stdio.h>


ompi_coll_alg_rule_t* coll_tuned_mk_alg_rules (int n_alg)
{
   int i;
   ompi_coll_alg_rule_t* alg_rules;

   alg_rules = (ompi_coll_alg_rule_t *) calloc (n_alg, sizeof (ompi_coll_alg_rule_t));
   if (!alg_rules) return (alg_rules);
   
   /* set all we can at this point */
   for (i=0;i<n_alg;i++) {
      alg_rules[i].alg_rule_id = i;
   }
   return (alg_rules);
}


ompi_coll_com_rule_t* coll_tuned_mk_com_rules (int n_com_rules, int alg_rule_id) 
{
   int i;
   ompi_coll_com_rule_t * com_rules;

   com_rules = (ompi_coll_com_rule_t *) calloc (n_com_rules, sizeof (ompi_coll_com_rule_t));
   if (!com_rules) return (com_rules);

   for (i=0;i<n_com_rules;i++) {
       com_rules[i].mpi_comsize = 0;   /* unknown */
       com_rules[i].alg_rule_id = alg_rule_id;
       com_rules[i].com_rule_id = i;
       com_rules[i].n_msg_sizes = 0;   /* unknown */
       com_rules[i].msg_rules = (ompi_coll_msg_rule_t *) NULL;
   }
   return (com_rules);
}


ompi_coll_msg_rule_t* coll_tuned_mk_msg_rules (int n_msg_rules, int alg_rule_id, int com_rule_id, int mpi_comsize)
{
    int i;
    ompi_coll_msg_rule_t *msg_rules;

    msg_rules = (ompi_coll_msg_rule_t *) calloc (n_msg_rules, sizeof (ompi_coll_msg_rule_t));
    if (!msg_rules) return (msg_rules);

    for (i=0;i<n_msg_rules;i++) {
       msg_rules[i].mpi_comsize = mpi_comsize;
       msg_rules[i].alg_rule_id = alg_rule_id;
       msg_rules[i].com_rule_id = com_rule_id;
       msg_rules[i].msg_rule_id = i;
       msg_rules[i].msg_size = 0; /* unknown */
       msg_rules[i].result_alg = 0; /* unknown */
       msg_rules[i].result_segsize = 0; /* unknown */
    }
    return (msg_rules);
}


/*
 * Debug / IO routines 
 *
 */ 


int coll_tuned_dump_msg_rule (ompi_coll_msg_rule_t* msg_p)
{
   if (!msg_p) {
      fprintf(stderr,"Message rule was a NULL ptr?!\n");
      return (-1);
   }

   printf("alg_id %3d\tcom_id %3d\tcom_size %3d\tmsg_id %3d\t", msg_p->alg_rule_id, msg_p->com_rule_id, 
                                                                msg_p->mpi_comsize, msg_p->msg_rule_id);

   printf("msg_size %6d -> algorithm %2d\tsegsize %5d\n", msg_p->msg_size, msg_p->result_alg, msg_p->result_segsize);

   return (0);
}


int coll_tuned_dump_com_rule (ompi_coll_com_rule_t* com_p)
{
   int i;
   ompi_coll_msg_rule_t* msg_p;

   if (!com_p) {
      fprintf(stderr,"Com rule was a NULL ptr?!\n");
      return (-1);
   }

   printf("alg_id %3d\tcom_id %3d\tcom_size %3d\t", com_p->alg_rule_id, com_p->com_rule_id, com_p->mpi_comsize);

   if (!com_p->n_msg_sizes) {
      printf("no msgsizes defined\n");
      return (0);
   }

   printf("number of message sizes %3d\n", com_p->n_msg_sizes);

   for (i=0;i<com_p->n_msg_sizes;i++) {
      coll_tuned_dump_msg_rule (&(com_p->msg_rules[i]));
   }

   return (0);
}


int coll_tuned_dump_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
   int i;
   ompi_coll_com_rule_t* com_p;

   if (!alg_p) {
      fprintf(stderr,"Algorithm rule was a NULL ptr?!\n");
      return (-1);
   }

   printf("alg_id %3d\t", alg_p->alg_rule_id);

   if (!alg_p->n_com_sizes) {
      printf("no coms defined\n");
      return (0);
   }

   printf("number of com sizes %3d\n", alg_p->n_com_sizes);

   for (i=0;i<alg_p->n_com_sizes;i++) {
      coll_tuned_dump_com_rule (&(alg_p->com_rules[i]));
   }

   return (0);
}


int coll_tuned_dump_all_rules (ompi_coll_alg_rule_t* alg_p, int n_rules)
{
   int i;

   if (!alg_p) {
      fprintf(stderr,"Algorithm rule was a NULL ptr?!\n");
      return (-1);
   }

   printf("Number of algorithm rules %3d\n", n_rules);

   for (i=0;i<n_rules;i++) {
      coll_tuned_dump_alg_rule (&(alg_p[i]));
   }

}


/*
 * Memory free routines
 *
 */


int coll_tuned_free_msg_rules_in_com_rule (ompi_coll_com_rule_t* com_p)
{
   int rc=0;
   ompi_coll_msg_rule_t* msg_p;

   if (!com_p) {
      fprintf(stderr,"attempt to free NULL com_rule ptr\n");
      return (-1);
   }

   if (com_p->n_msg_sizes) {
      msg_p = com_p->msg_rules;

      if (!msg_p) {
         fprintf(stderr,"attempt to free NULL msg_rules when msg count was %d\n", com_p->n_msg_sizes);
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



int coll_tuned_free_coms_in_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
   int rc=0;
   int i;

   ompi_coll_com_rule_t* com_p;

   if (!alg_p) {
      fprintf(stderr,"attempt to free NULL alg_rule ptr\n");
      return (-1);
   }

   if (alg_p->n_com_sizes) {
      com_p = alg_p->com_rules;

      if (!com_p) {
         fprintf(stderr,"attempt to free NULL com_rules when com count was %d\n", alg_p->n_com_sizes);
      }
      else {
        /* ok, memory exists for the com rules so free their message rules first */
        for (i=0;i<alg_p->n_com_sizes;i++) {
           com_p = &(alg_p->com_rules[i]);
           coll_tuned_free_msg_rules_in_com_rule (com_p);
        }
        /* we are now free to free the com rules themselives */
        free (alg_p->com_rules);
        alg_p->com_rules = (ompi_coll_com_rule_t*) NULL;
      }

   } /* if we have msg rules to free as well */

   return (rc);
}


int coll_tuned_free_all_rules (ompi_coll_alg_rule_t* alg_p, int n_algs)
{
   int i;
   int rc;

   for(i=0;i<n_algs;i++) {
     rc += coll_tuned_free_coms_in_alg_rule (&(alg_p[i]));
   }

   free (alg_p);

   return (rc);
}



/* 
 * query functions
 * i.e. the functions that get me the algorithm and segment size fast
 * and also get the rules that are needed by each communicator as needed
 *
 */

/*
 * This function is used to get the pointer to the nearest (less than or equal)
 * com rule for this MPI collective (alg_id) for a given 
 * MPI communicator size. The complete rule base must be presented.
 *
 * If no rule exits returns NULL, else the com rule ptr
 * (which can be used in the coll_tuned_get_target_method_params() call)
 *
 */
ompi_coll_com_rule_t* coll_tuned_get_com_rule_ptr (ompi_coll_alg_rule_t* rules, int alg_id, int mpi_comsize)
{
   ompi_coll_alg_rule_t*  alg_p = (ompi_coll_alg_rule_t*) NULL;
   ompi_coll_com_rule_t*  com_p = (ompi_coll_com_rule_t*) NULL;
   ompi_coll_com_rule_t*  best_com_p = (ompi_coll_com_rule_t*) NULL;
   int i, best;

   if (!rules) {                    /* no rule base no resulting com rule */
      return ((ompi_coll_com_rule_t*)NULL);
   }

   alg_p = &(rules[alg_id]); /* get the algorithm rule pointer */

   if (!alg_p->n_com_sizes) {   /* check for count of communicator sizes */
      return ((ompi_coll_com_rule_t*)NULL);    /* no com sizes so no rule */
   }

   /* ok have some com sizes, now to find the one closest to my mpi_comsize */
   
   /* make a copy of the first com rule */
   best_com_p = com_p = alg_p->com_rules;
   i = best = 0;

   while (i<alg_p->n_com_sizes) {
      printf("checking comsize %d against alg_id %d com_id %d index %d com_size %d", 
            mpi_comsize, com_p->alg_rule_id, com_p->com_rule_id, i, com_p->mpi_comsize);
      if (com_p->mpi_comsize <= mpi_comsize) {
         best = i;
         best_com_p = com_p;
         printf(":ok\n");
      }
      else {
         printf(":nop\n");
         break;
      }
      /* go to the next entry */
      com_p++;
      i++;
   }

  printf("Selected the following com rule id %d\n", best_com_p->com_rule_id);
  coll_tuned_dump_com_rule (best_com_p);

  return (best_com_p);
}

/* 
 * This function takes a com_rule ptr (from the communicators coll tuned data structure) 
 * (Which is chosen for a particular MPI collective)
 * and a (total_)msg_size and it returns (0) and a algorithm to use and a recommended segment size
 * all based on the user supplied rules
 *
 * Just like the above functions it uses a less than or equal msg size 
 * (hense config file must have a default defined for '0' if we reach this point)
 * else if no rules match we return '0' + '0' or used fixed decision table with no segmentation
 * of users data.. shame.
 *
 * On error return 0 so we default to fixed rules anyway :)
 *
 */

int coll_tuned_get_target_method_params (ompi_coll_com_rule_t* base_com_rule, int mpi_msgsize, int* result_segsize)
{
   ompi_coll_msg_rule_t*  msg_p = (ompi_coll_msg_rule_t*) NULL;
   ompi_coll_msg_rule_t*  best_msg_p = (ompi_coll_msg_rule_t*) NULL;
   int i, best;

   if (!base_com_rule) {
      return (0);
   }

   if (!result_segsize) {
      return (0);
   }

   if (!base_com_rule->n_msg_sizes) {   /* check for count of message sizes */
      return (0);    /* no msg sizes so no rule */
   }

   /* ok have some msg sizes, now to find the one closest to my mpi_msgsize */
   
   /* make a copy of the first msg rule */
   best_msg_p = msg_p = base_com_rule->msg_rules;
   i = best = 0;

   while (i<base_com_rule->n_msg_sizes) {
      printf("checking mpi_msgsize %d against com_id %d msg_id %d index %d msg_size %d", 
            mpi_msgsize, msg_p->com_rule_id, msg_p->msg_rule_id, i, msg_p->msg_size);
      if (msg_p->msg_size <= mpi_msgsize) {
         best = i;
         best_msg_p = msg_p;
         printf(":ok\n");
      }
      else {
         printf(":nop\n");
         break;
      }
      /* go to the next entry */
      msg_p++;
      i++;
   }

  printf("Selected the following msg rule id %d\n", best_msg_p->msg_rule_id);
  coll_tuned_dump_msg_rule (best_msg_p);

  /* return the segment size */
  *result_segsize = best_msg_p->result_segsize;
  return (best_msg_p->result_alg);
}

