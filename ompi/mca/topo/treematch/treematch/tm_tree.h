#ifndef __TREE_H__
#define __TREE_H__
#include <stdlib.h>


typedef struct _node_info_t{
  int submit_date;
  int job_id;
  int finish_date;
} job_info_t;

typedef struct _tree_t{
  int constraint; /* tells if the tree has been constructed with constraints on the nodes or not.                    usefull for freeing it. needs to be set on the root only*/
  struct _tree_t **child;
  struct _tree_t *parent;
  struct _tree_t *tab_child; /*the pointer to be freed*/
  double val;
  int arity;
  int depth;
  int id;
  int uniq;
  int dumb; /* 1 if the node belongs to a dumb tree: hence has to be freed separately*/
  job_info_t *job_info;
}tree_t;

/* Maximum number of levels in the tree*/
#define MAX_LEVELS 100

typedef struct {
  int *arity; /* arity of the nodes of each level*/
  int nb_levels; /*number of levels of the tree. Levels are numbered from top to bottom starting at 0*/
  int *nb_nodes; /*nb of nodes of each level*/
  int *nb_free_nodes; /*nb of available nodes of each level*/
  int **node_id;    /*ID of the nodes of the tree for each level*/
  int **free_nodes; /*ID of the nodes of the tree for each level*/
}tm_topology_t;


typedef struct {
  double ** mat;
  double *  sum_row;
  int order;
} affinity_mat_t;



tree_t * build_tree(double **tab,int N);
tree_t * build_tree_from_topology(tm_topology_t *topology,double **tab,int N, double *obj_weight, double *comm_speed);
void map_tree(tree_t *,tree_t*);
void display_tab(double **tab,int N);
double speed(int depth);
void set_node(tree_t *node,tree_t ** child, int arity,tree_t *parent,int id,double val,tree_t *deb_tab_child, int depth);
void free_constraint_tree(tree_t *tree);
void free_tree(tree_t *tree);
void free_tab_double(double**tab,int N);
void free_tab_int(int**tab,int N);
void update_val(affinity_mat_t *aff_mat,tree_t *parent);
void FREE_tree(tree_t *tree);
void FREE_tab_double(double**,int);

typedef struct _group_list_t{
  struct _group_list_t *next;
  tree_t **tab;
  double val;
  double sum_neighbour;
  double wg;
}group_list_t;


typedef struct{
  int i;
  int j;
  double val;
}adjacency_t;



/* for debugging malloc */
/* #define __DEBUG_MY_MALLOC__ */
#undef __DEBUG_MY_MALLOC__
#ifdef __DEBUG_MY_MALLOC__
#include "tm_malloc.h"
#define MALLOC(x) my_malloc(x,__FILE__,__LINE__)
#define CALLOC(x,y) my_calloc(x,y,__FILE__,__LINE__)
#define FREE   my_free
#define MEM_CHECK my_mem_check
#else
#define MALLOC malloc
#define CALLOC calloc
#define FREE   free
#define MEM_CHECK my_mem_check
#endif

#endif
