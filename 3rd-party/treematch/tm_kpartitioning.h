#include "ompi_config.h"

typedef struct _com_mat_t{
  double **comm; 
  int n;  /*comm is of size n by n the other element are zeroes*/
  
} com_mat_t;


OMPI_HIDDEN int  *tm_kpartition(int, com_mat_t*, int, int *, int);
OMPI_HIDDEN tm_tree_t * tm_kpartition_build_tree_from_topology(tm_topology_t *topology,double **com_mat,int N, int *constraints, int nb_constraints, double *obj_weight, double *com_speed);
