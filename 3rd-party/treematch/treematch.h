#ifndef __TREEMATCH_H__
#define __TREEMATCH_H__

/* size_t definition */
#include <stddef.h>
#include "tm_verbose.h"
#include "ompi_config.h"

/********* TreeMatch Public Enum **********/

/*type of topology files that can be read*/
typedef enum{
  TM_FILE_TYPE_UNDEF,
  TM_FILE_TYPE_XML,
  TM_FILE_TYPE_TGT
} tm_file_type_t;

/* different metrics to evaluate the solution */
typedef enum{
  TM_METRIC_SUM_COM  = 1,
  TM_METRIC_MAX_COM  = 2,
  TM_METRIC_HOP_BYTE = 3
} tm_metric_t;

/* numbering */
typedef enum{
  TM_NUMBERING_LOGICAL   = 0,
  TM_NUMBERING_PHYSICAL  = 1
} tm_numbering_t;

/********* TreeMatch Public Structures **********/

typedef struct _job_info_t{
  int submit_date;
  int job_id;
  int finish_date;
} tm_job_info_t;

typedef struct _tm_tree_t{
  int constraint; /* tells if the tree has been constructed with constraints on the nodes or not.
		     Usefull for freeing it. needs to be set on the root only*/
  struct _tm_tree_t **child;
  struct _tm_tree_t *parent;
  struct _tm_tree_t *tab_child; /* The pointer to be freed */
  double val;
  int arity;
  int depth;
  int id;   /* id of the node or the leaf. Ids are different onmly on a given level */
  int uniq; /* uniq id in the whole tree */
  int dumb; /* 1 if the node belongs to a dumb tree: hence has to be freed separately */
  tm_job_info_t *job_info;
  int nb_processes; /* number of grouped processes (i.e. the order of the affinity matrix). Set at the root only */
}tm_tree_t; /* FT : changer le nom : tm_grouap_hierachy_t ? */

/* Maximum number of levels in the tree*/
#define TM_MAX_LEVELS 100

typedef struct {
  int *arity;             /* Arity of the nodes of each level*/
  int nb_levels;          /* Number of levels of the tree. Levels are numbered from top to bottom starting at 0*/
  size_t *nb_nodes;       /* Number of nodes of each level*/
  int physical_num;       /* Flag set to !=0 if se use physical numberig and set to 0 is we use logical numbering */
  int *node_id;           /* ID of the nodes of the tree of the last level*/
  int *node_rank ;        /* Rank of the nodes of the tree for the last level given its ID: this is the inverse tab of node_id*/

  size_t *nb_free_nodes;  /* Nb of available nodes of each level*/
  int **free_nodes;       /* array of node that are free: useful to simulate batch scheduler*/
  double *cost;           /* Cost of the communication depending on the distance:
			    cost[i] is the cost for communicating at distance nb_levels-i*/
  
  int *constraints;       /* Array of constraints: id of the nodes where it is possible to map processes */
  int nb_constraints;     /* Size of the above array */
  int oversub_fact;       /* Maximum number of processes to be mapped on a given node */
  int nb_proc_units;      /* The real number of units used for computation */
}tm_topology_t;


typedef struct {
  double ** mat;
  double *  sum_row;
  int order;
  long int nnz; /* number of non zero entries */
} tm_affinity_mat_t;

/*
 sigma[i] is such that  process i is mapped on core sigma[i]
 k[i][j] is such that core i executes process k[i][j] (0<=j<<=oversubscribing factor - 1)

 size of sigma is the number of processes (nb_objs)
 size of k is the number of cores/nodes   (nb_compute_units)
 size of k[i] is the number of process we can execute per nodes (1 if no oversubscribing)

 We must have number of process<=number of cores

 k[i] == NULL if no process is mapped on core i
*/

typedef struct {
  int *sigma;
  size_t sigma_length;
  int **k;
  size_t k_length;
  int oversub_fact;
}tm_solution_t;


/************ TreeMatch Public API ************/
/* construct topology from local one using hwloc */
OMPI_HIDDEN tm_topology_t* tm_get_local_topology_with_hwloc(void);

/* Aletrnatively, load XML or TGT topology */
OMPI_HIDDEN tm_topology_t *tm_load_topology(char *arch_filename, tm_file_type_t arch_file_type);
/*
   Alternatively, build a synthetic balanced topology.

   nb_levels : number of levels of the topology +1 (the last level must be of cost 0 and arity 0).
   arity : array of arity of the first nb_level (of size nb_levels)
   cost : array of costs between the levels (of size nb_levels)
   core_numbering: numbering of the core by the system. Array of size nb_core_per_node

   nb_core_per_nodes: number of cores of a given node. Size of the array core_numbering

   both arity and cost are copied inside tm_build_synthetic_topology

   The numbering of the cores is done in round robin fashion after a width traversal of the topology.
   for example:
       {0,1,2,3} becomes 0,1,2,3,4,5,6,7...
   and
       {0,2,1,3} becomes 0,2,1,3,4,6,5,7,...

   Example of call to build the 128.tgt file: tleaf 4 16 500 2 100 2 50 2 10

   double cost[5] = {500,100,50,10,0};
   int arity[5] = {16,2,2,2,0};
   int cn[2]={0,1};

   topology = tm_build_synthetic_topology(arity,cost,5,cn,2);

 */
OMPI_HIDDEN tm_topology_t  *tm_build_synthetic_topology(int *arity, double *cost, int nb_levels, int *core_numbering, int nb_core_per_nodes);
/* load affinity matrix */
OMPI_HIDDEN tm_affinity_mat_t *tm_load_aff_mat(char *com_filename);
/*
   Alternativelly, build the affinity matrix from a array of array of matrix of size order by order
   For performance reason mat is not copied.
*/
OMPI_HIDDEN tm_affinity_mat_t * tm_build_affinity_mat(double **mat, int order);
/* Add constraints to toplogy
   Return 1 on success and 0  if the constari,ts id are not compatible withe nodes id */
OMPI_HIDDEN int tm_topology_add_binding_constraints(char *bind_filename, tm_topology_t *topology);
/* Alternatively, set the constraints from an array.
   Return 1 on success and 0  if the constari,ts id are not compatible withe nodes id

   The array constraints is copied inside tm_topology_set_binding_constraints

*/
OMPI_HIDDEN int tm_topology_set_binding_constraints(int *constraints, int nb_constraints, tm_topology_t *topology);
/* display arity of the topology */
OMPI_HIDDEN void  tm_display_arity(tm_topology_t *topology);
/* display the full topology */
OMPI_HIDDEN void  tm_display_topology(tm_topology_t *topology);
/* Optimize the topology by decomposing arities */
OMPI_HIDDEN void tm_optimize_topology(tm_topology_t **topology);
/* Manage oversubscribing */
OMPI_HIDDEN void tm_enable_oversubscribing(tm_topology_t *topology, unsigned int oversub_fact);
/* core of the treematch: compute the solution tree */
OMPI_HIDDEN tm_tree_t *tm_build_tree_from_topology(tm_topology_t *topology, tm_affinity_mat_t *aff_mat, double *obj_weight, double *com_speed);
/* compute the mapping according to the tree and the core numbering*/
OMPI_HIDDEN tm_solution_t *tm_compute_mapping(tm_topology_t *topology, tm_tree_t *comm_tree);
/* display the solution*/
OMPI_HIDDEN double tm_display_solution(tm_topology_t *topology, tm_affinity_mat_t *aff_mat, tm_solution_t *sol, tm_metric_t metric);
/* display RR, packed, MPIPP*/
OMPI_HIDDEN void tm_display_other_heuristics(tm_topology_t *topology, tm_affinity_mat_t *aff_mat, tm_metric_t metric);
/* free TM strutures*/
OMPI_HIDDEN void tm_free_topology(tm_topology_t *topology);
OMPI_HIDDEN void tm_free_tree(tm_tree_t *comm_tree);
OMPI_HIDDEN void tm_free_solution(tm_solution_t *sol);
OMPI_HIDDEN void tm_free_affinity_mat(tm_affinity_mat_t *aff_mat);
/* manage verbosity of TM*/
void tm_set_verbose_level(unsigned int level);
unsigned int  tm_get_verbose_level(void);
/* finalize treematch :check memory if necessary, and free internal variables (thread pool)*/
OMPI_HIDDEN void tm_finalize(void);

/*
Ask for exhaustive search: may be very long
   new_val == 0 : no exhuative search
   new_val != 0 : exhuative search
*/
OMPI_HIDDEN void tm_set_exhaustive_search_flag(int new_val);
OMPI_HIDDEN int tm_get_exhaustive_search_flag(void);

/*
Ask for greedy k-partitionning even if scotch is available
   new_val == 0 : no greedy k-partitionning
   new_val != 0 : greedy k-partitionning
*/
OMPI_HIDDEN void tm_set_greedy_flag(int new_val);
OMPI_HIDDEN int tm_get_greedy_flag(void);


/* Setting the maximum number of threads you want to use in parallel parts of TreeMatch */
OMPI_HIDDEN void tm_set_max_nb_threads(unsigned int val);

/* managing the usage of physical vs. logical core numbering when using hwloc/xml files */
OMPI_HIDDEN void tm_set_numbering(tm_numbering_t new_val); /* TM_NUMBERING_LOGICAL or TM_NUMBERING_PHYSICAL */
OMPI_HIDDEN tm_numbering_t  tm_get_numbering(void); /* TM_NUMBERING_LOGICAL or TM_NUMBERING_PHYSICAL */

#include "tm_malloc.h"

#endif
