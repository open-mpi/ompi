#ifndef __TM_MAPPING_H__
#define __TM_MAPPING_H__
#include "tm_tree.h"
#include "tm_topology.h"
#include "tm_timings.h"
#include "tm_verbose.h"
#include "ompi_config.h"

OMPI_HIDDEN tm_affinity_mat_t * tm_new_affinity_mat(double **mat, double *sum_row, int order, long int nnz);
OMPI_HIDDEN tm_topology_t  *build_synthetic_topology(int *arity, int nb_levels, int *core_numbering, int nb_core_per_nodes);
OMPI_HIDDEN int tm_compute_nb_leaves_from_level(int depth,tm_topology_t *topology);
OMPI_HIDDEN int  tm_fill_tab(int **new_tab,int *tab, int n, int start, int max_val, int shift);
OMPI_HIDDEN void tm_map_topology(tm_topology_t *topology,tm_tree_t *comm_tree, int level,
		  int *sigma, int nb_processes, int **k, int nb_compute_units);
OMPI_HIDDEN int tm_nb_processing_units(tm_topology_t *topology);
OMPI_HIDDEN void tm_print_1D_tab(int *tab,int N);
OMPI_HIDDEN tm_solution_t * tm_compute_mapping(tm_topology_t *topology,tm_tree_t *comm_tree);
OMPI_HIDDEN void tm_free_affinity_mat(tm_affinity_mat_t *aff_mat);
OMPI_HIDDEN tm_affinity_mat_t *tm_load_aff_mat(char *filename);

/* use to split a constaint into subconstraint according the tree*/
typedef struct{
  int *constraints; /* the subconstraints*/
  int length; /*length of *constraints*/
  int id;  /* id of the corresponding subtree*/
}constraint_t;

#endif
