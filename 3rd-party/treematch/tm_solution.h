#ifndef TM_SOLUION_H
#define TM_SOLUION_H

#include "ompi_config.h"
#include "treematch.h"

OMPI_HIDDEN void tm_free_solution(tm_solution_t *sol);
OMPI_HIDDEN double tm_display_solution(tm_topology_t *topology, tm_affinity_mat_t *aff_mat, tm_solution_t *sol, tm_metric_t metric);
OMPI_HIDDEN void tm_display_other_heuristics(tm_topology_t *topology, tm_affinity_mat_t *aff_mat, tm_metric_t metric);
int tm_in_tab(int *tab, int n, int val);
OMPI_HIDDEN void tm_map_Packed(tm_topology_t *topology, int N, int *sigma);
OMPI_HIDDEN void tm_map_RR(tm_topology_t *topology, int N, int *sigma);
OMPI_HIDDEN void tm_map_MPIPP(tm_topology_t *topology,int nb_seed,int N,int *sigma,double **comm, double **arch);


#endif
