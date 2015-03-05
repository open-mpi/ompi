#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "tm_tree.h"
#include "tm_timings.h"
#include "tm_bucket.h"
#include "tm_kpartitioning.h"
#include "tm_mapping.h"
#include "tm_verbose.h"
#include "tm_thread_pool.h"


#define MIN(a,b) ((a)<(b)?(a):(b))
#define MAX(a,b) ((a)>(b)?(a):(b))

#ifndef __CHARMC__
#define __CHARMC__ 0
#endif

#if __CHARMC__
#include "converse.h"
#else
static int ilog2(int val)
{
  int i = 0;
  for( ; val != 0; val >>= 1, i++ );
  return i;
}
#define CmiLog2(VAL)  ilog2((int)(VAL))
#endif



static int verbose_level = ERROR;


void FREE_list_child(tree_t *);
void FREE_tab_child(tree_t *);
unsigned long int choose (long,long);
void display_node(tree_t *);
void clone_tree(tree_t *,tree_t *);
double *aggregate_obj_weight(tree_t *,double *,int);
affinity_mat_t *aggregate_com_mat(tree_t *,affinity_mat_t *,int);
double eval_grouping(affinity_mat_t *,tree_t **,int);
group_list_t *new_group_list(tree_t **,double,group_list_t *);
void add_to_list(group_list_t *,tree_t **,int,double);
void  list_all_possible_groups(affinity_mat_t *,tree_t *,int,int,int,tree_t **,group_list_t *);
int independent_groups(group_list_t **,int,group_list_t *,int);
void display_selection (group_list_t**,int,int,double);
void display_grouping (tree_t *,int,int,double);
int recurs_select_independent_groups(group_list_t **,int,int,int,int,
				     int,double,double *,group_list_t **,group_list_t **);
int test_independent_groups(group_list_t **,int,int,int,int,int,double,double *,
			    group_list_t **,group_list_t **);
void delete_group_list(group_list_t *);
int group_list_id(const void*,const void*);
int group_list_asc(const void*,const void*);
int group_list_dsc(const void*,const void*);
int weighted_degree_asc(const void*,const void*);
int weighted_degree_dsc(const void*,const void*);
int  select_independent_groups(group_list_t **,int,int,int,double *,group_list_t **,int,double);
int  select_independent_groups_by_largest_index(group_list_t **,int,int,int,double *,
						group_list_t **,int,double);
void list_to_tab(group_list_t *,group_list_t **,int);
void display_tab_group(group_list_t **,int,int);
int independent_tab(tree_t **,tree_t **,int);
void compute_weighted_degree(group_list_t **,int,int);
void  group(affinity_mat_t *,tree_t *,tree_t *,int,int,int,double *,tree_t **);
void  fast_group(affinity_mat_t *,tree_t *,tree_t *,int,int,int,double *,tree_t **, int *, int);
int adjacency_asc(const void*,const void*);
int adjacency_dsc(const void*,const void*);
		 void super_fast_grouping(affinity_mat_t *,tree_t *,tree_t *,int, int);
affinity_mat_t *build_cost_matrix(affinity_mat_t *,double *,double);
void group_nodes(affinity_mat_t *,tree_t *,tree_t *,int ,int,double*,double);
void fast_grouping(affinity_mat_t *,tree_t *,tree_t *,int,int,long int);
void complete_aff_mat(affinity_mat_t **,int,int);
void complete_obj_weight(double **,int,int);
void create_dumb_tree(tree_t *,int,tm_topology_t *);
void complete_tab_node(tree_t **,int,int,int,tm_topology_t *);
void set_deb_tab_child(tree_t *,tree_t *,int);
tree_t *build_level_topology(tree_t *,affinity_mat_t *,int,int,tm_topology_t *,double *,double *);
int check_constraints(tm_topology_t  *,int **);
tree_t *bottom_up_build_tree_from_topology(tm_topology_t *,double **, int ,double *,double *);
void FREE_non_constraint_tree(tree_t *);
void FREE_constraint_tree(tree_t *);
void FREE_tab_double(double**,int);
void FREE_tab_int(int**,int );
void partial_aggregate_com_mat (int, void **);
affinity_mat_t *new_affinity_mat(double **, double *, int);
void partial_aggregate_aff_mat (int, void **);
affinity_mat_t *aggregate_aff_mat(tree_t *, affinity_mat_t *, int);
affinity_mat_t * build_affinity_mat(double **, int);

affinity_mat_t *new_affinity_mat(double **mat, double *sum_row, int order){
  affinity_mat_t *res = (affinity_mat_t *) MALLOC (sizeof(affinity_mat_t));

  res -> mat     = mat;
  res -> sum_row = sum_row;
  res -> order   = order;

  return res;
}

void FREE_list_child(tree_t *tree)
{
  int i;

  if(tree)
    for(i=0;i<tree->arity;i++)
      FREE_list_child(tree->child[i]);

  FREE(tree->child);
  if(tree->dumb)
    FREE(tree);
}

void FREE_tab_child(tree_t *tree)
{
  if(tree){
    FREE_tab_child(tree->tab_child);
    FREE(tree->tab_child);
  }
}

void FREE_non_constraint_tree(tree_t *tree)
{

  FREE_list_child(tree);
  FREE_tab_child(tree);
  FREE(tree);
}

void FREE_constraint_tree(tree_t *tree)
{
  int i;
  if(tree){
    for(i=0;i<tree->arity;i++)
      FREE_constraint_tree(tree->child[i]);
    FREE(tree->child);
    FREE(tree);
  }
}


void FREE_tree(tree_t *tree)
{
  if(tree->constraint)
    FREE_constraint_tree(tree);
  else
    FREE_non_constraint_tree(tree);
}

unsigned long int choose (long n,long k)
{
  /* compute C_n_k */
  double res = 1;
  int i;

  for( i = 0 ; i < k ; i++ )
    res *= (double)(n-i)/(double)(k-i);

  return (unsigned long int)res;
}

void set_node(tree_t *node,tree_t ** child, int arity,tree_t *parent,
	      int id,double val,tree_t *tab_child,int depth)
{
  static int uniq = 0;
  node->child = child;
  node->arity = arity;
  node->tab_child = tab_child;
  node->parent = parent;
  node->id = id;
  node->val = val;
  node->uniq = uniq++;
  node->depth= depth;
  node->dumb = 0;
}

void display_node(tree_t *node)
{
  if (verbose_level >= DEBUG)
    printf("child : %p\narity : %d\nparent : %p\nid : %d\nval : %f\nuniq : %d\n\n",
	   (void *)(node->child),node->arity,(void *)(node->parent),node->id,node->val,node->uniq);
}

void clone_tree(tree_t *new,tree_t *old)
{
  int i;
  new->child = old->child;
  new->parent = old->parent;
  new->tab_child = old->tab_child;
  new->val = old->val;
  new->arity = old->arity;
  new->depth = old->depth;
  new->id = old->id;
  new->uniq = old->uniq;
  new->dumb = old->dumb;
  for( i = 0 ; i < new->arity ; i++ )
    new->child[i]->parent = new;
}


double *aggregate_obj_weight(tree_t *new_tab_node, double *tab, int M)
{
  int i,i1,id1;
  double *res = NULL;

  if(!tab)
    return NULL;

  res = (double*)MALLOC(M*sizeof(double));

  for( i = 0 ; i < M ; i++ ){
    res[i] = 0.0;
    for( i1 = 0 ; i1 < new_tab_node[i].arity ; i1++ ){
      id1 = new_tab_node[i].child[i1]->id;
      res[i] += tab[id1];
    }
  }
  return res;
}



void partial_aggregate_aff_mat (int nb_args, void **args){
  int inf = *(int*)args[0];
  int sup = *(int*)args[1];
  double **old_mat = (double**)args[2];
  tree_t *tab_node = (tree_t*)args[3];
  int M = *(int*)args[4];
  double **mat = (double**)args[5];
  double *sum_row = (double*)args[6];
  int i,j,i1,j1;
  int id1, id2;


  if(nb_args != 6){
    if(verbose_level >= ERROR)
      fprintf(stderr,"Wrong number of args in %s: %d\n",__FUNCTION__, nb_args);
    exit(-1);
  }

  if(verbose_level >= INFO)
    printf("Aggregate in parallel (%d-%d)\n",inf,sup-1);

  for( i = inf ; i < sup ; i++ )
    for( j = 0 ; j < M ; j++ ){
      if(i != j){
	for( i1 = 0 ; i1 < tab_node[i].arity ; i1++ ){
	  id1 = tab_node[i].child[i1]->id;
	  for( j1 = 0 ; j1 < tab_node[j].arity ; j1++ ){
	    id2 = tab_node[j].child[j1]->id;
	    mat[i][j] += old_mat[id1][id2];
	    /* printf("mat[%d][%d]+=old_mat[%d][%d]=%f\n",i,j,id1,id2,old_mat[id1][id2]);*/
	  }
	  sum_row[i] += mat[i][j];
	}
      }
    }
}


affinity_mat_t *aggregate_aff_mat(tree_t *tab_node, affinity_mat_t *aff_mat, int M)
{
  int i,j,i1,j1,id1,id2;
  double **new_mat = NULL, **old_mat = aff_mat->mat;
  double *sum_row = NULL;

  new_mat = (double**)MALLOC(M*sizeof(double*));
  for( i = 0 ; i < M ; i++ )
    new_mat[i] = (double*)CALLOC((M),sizeof(double));

  sum_row = (double*)CALLOC(M,sizeof(double));

  if(M>512){ /* perform this part in parallel*/
    int id;
    int nb_threads;
    work_t **works;
    int *inf;
    int *sup;

    nb_threads = MIN(M/512,get_nb_threads());
    works = (work_t**)MALLOC(sizeof(work_t*)*nb_threads);
    inf = (int*)MALLOC(sizeof(int)*nb_threads);
    sup = (int*)MALLOC(sizeof(int)*nb_threads);
    for(id=0;id<nb_threads;id++){
      void **args=(void**)MALLOC(sizeof(void*)*7);
      inf[id]=id*M/nb_threads;
      sup[id]=(id+1)*M/nb_threads;
      if(id == nb_threads-1) sup[id]=M;
      args[0]=(void*)(inf+id);
      args[1]=(void*)(sup+id);
      args[2]=(void*)old_mat;
      args[3]=(void*)tab_node;
      args[4]=&M;
      args[5]=(void*)new_mat;
      args[6]=(void*)sum_row;

      works[id]= create_work(7,args,partial_aggregate_aff_mat);
      if(verbose_level >= DEBUG)
	printf("Executing %p\n",(void *)works[id]);

      submit_work( works[id], id);
    }

    for(id=0;id<nb_threads;id++){
      wait_work_completion(works[id]);
      FREE(works[id]->args);
    }


    FREE(inf);
    FREE(sup);
    FREE(works);

  }else{
  for( i = 0 ; i < M ; i++ )
    for( j = 0 ; j < M ; j++ ){
      if(i != j){
	for( i1 = 0 ; i1 < tab_node[i].arity ; i1++ ){
	  id1 = tab_node[i].child[i1]->id;
	  for( j1 = 0 ; j1 < tab_node[j].arity ; j1++ ){
	    id2 = tab_node[j].child[j1]->id;
	    new_mat[i][j] += old_mat[id1][id2];
	    /* printf("mat[%d][%d]+=old_mat[%d][%d]=%f\n",i,j,id1,id2,old_mat[id1][id2]);*/
	  }
	  sum_row[i] += new_mat[i][j];
	}
      }
    }
  }
  return new_affinity_mat(new_mat,sum_row,M);
}

void FREE_tab_double(double**tab,int N)
{
  int i;
  for( i = 0 ; i < N ; i++ )
    FREE(tab[i]);
  FREE(tab);
}

void FREE_tab_int(int**tab,int N)
{
  int i;
  for( i = 0 ; i < N ; i++ )
    FREE(tab[i]);
  FREE(tab);
}

void display_tab(double **tab,int N)
{
  int i,j;
  double line,total = 0;


  for( i = 0 ; i < N ; i++ ){
    line = 0;
    for( j = 0 ; j < N ; j++ ){
      printf("%g ",tab[i][j]);
      line += tab[i][j];
    }
    total += line;
    /* printf(": %g",line);*/
    printf("\n");
  }
  /* printf("Total: %.2f\n",total);*/
}


double eval_grouping(affinity_mat_t *aff_mat,tree_t **cur_group,int arity)
{
  double res = 0;
  int i,j,id,id1,id2;
  double **mat = aff_mat->mat;
  double * sum_row = aff_mat -> sum_row;

  /*display_tab(tab,N);*/

  for( i = 0 ; i < arity ; i++ ){
    id = cur_group[i]->id;
    res += sum_row[id];
  }

  for( i = 0 ; i < arity ; i++ ){
    id1 = cur_group[i]->id;
    for( j = 0 ; j < arity ; j++ ){
      id2 = cur_group[j]->id;
      /*printf("res-=tab[%d][%d]=%f\n",id1,id2,tab[id1][id2]);*/
      res -= mat[id1][id2];
    }
  }
  /*printf(" = %f\n",res);*/
  return res;
}


group_list_t *new_group_list(tree_t **tab,double val,group_list_t *next)
{
  group_list_t *res = NULL;

  res = (group_list_t *)MALLOC(sizeof(group_list_t));
  res->tab = tab;
  res->val = val;
  res->next = next;
  res->sum_neighbour = 0;
  return res;
}


void add_to_list(group_list_t *list,tree_t **cur_group, int arity, double val)
{
  group_list_t *elem = NULL;
  tree_t **tab = NULL;
  int i;

  tab=(tree_t **)MALLOC(sizeof(tree_t *)*arity);

  for( i = 0 ; i < arity ; i++ ){
    tab[i] = cur_group[i];
    if(verbose_level>=INFO)
      printf("cur_group[%d]=%d ",i,cur_group[i]->id);
  }
  if(verbose_level>=INFO)
    printf(": %f\n",val);

  /*printf("\n");*/
  elem = new_group_list(tab,val,list->next);
  list->next = elem;
  list->val++;
}


void  list_all_possible_groups(affinity_mat_t *aff_mat,tree_t *tab_node,int id,int arity, int depth,
			       tree_t **cur_group, group_list_t *list)
{
  double val;
  int i;
  int N = aff_mat->order;

  if(depth == arity){
    val = eval_grouping(aff_mat,cur_group,arity);
    add_to_list(list,cur_group,arity,val);
    return;
  }else if( (N+depth) >= (arity+id) ){
    /*}else if(1){*/
    for( i = id ; i < N ; i++ ){
      if(tab_node[i].parent)
	continue;
      cur_group[depth] = &tab_node[i];
      if(verbose_level>=INFO)
	printf("%d<-%d\n",depth,i);
      list_all_possible_groups(aff_mat,tab_node,i+1,arity,depth+1,cur_group,list);
    }
  }
}

void update_val(affinity_mat_t *aff_mat,tree_t *parent)
{
  /* int i; */

  parent->val = eval_grouping(aff_mat,parent->child,parent->arity);
  /*printf("connecting: ");*/
  /*for( i = 0 ; i < parent->arity ; i++ ){ */
    /*printf("%d ",parent->child[i]->id);*/
    /*  if(parent->child[i]->parent!=parent){
	parent->child[i]->parent=parent;
	}else{
	fprintf(stderr,"redundant operation!\n");
	exit(-1);
	}*/
  /* } */
  /*printf(": %f\n",parent->val);*/
}

int independent_groups(group_list_t **selection,int d,group_list_t *elem,int arity)
{
  int i,j,k;

  if(d == 0)
    return 1;

  for( i = 0 ; i < arity ; i++ )
    for( j = 0 ; j < d ; j++ )
      for( k = 0 ; k < arity ; k++ )
	if(elem->tab[i]->id == selection[j]->tab[k]->id)
	  return 0;
  return 1;
}

void display_selection (group_list_t** selection,int M,int arity,double val)
{
  int i,j;

  if(verbose_level<INFO)
    return;


  for( i = 0 ; i < M ; i++ ) {
    for( j = 0 ; j < arity ; j++ )
      printf("%d ",selection[i]->tab[j]->id);
    printf("-- ");
  }
  printf(":%f\n",val);
}

void display_grouping (tree_t *father,int M,int arity,double val)
{
  int i,j;

  if(verbose_level < INFO)
    return;

  printf("Grouping : ");
  for( i = 0  ; i < M ; i++ ){
    for( j = 0 ; j < arity ; j++ )
      printf("%d ",father[i].child[j]->id);
    printf("-- ");
  }
  printf(":%f\n",val);
}


int recurs_select_independent_groups(group_list_t **tab,int i,int n,int arity,int d,int M,double val,double *best_val,group_list_t **selection,group_list_t **best_selection)
{
  group_list_t *elem = NULL;
  /*
    if(val>=*best_val)
    return 0;
  */

  if( d == M ){
    if(verbose_level>=INFO)
      display_selection(selection,M,arity,val);
    if( val < *best_val ){
      *best_val = val;
      for( i = 0 ; i < M ; i++ )
	best_selection[i] = selection[i];
      return 1;
    }
    return 0;
  }

  while( i < n ){
    elem = tab[i];
    if(independent_groups(selection,d,elem,arity)){
      if(verbose_level>=INFO)
	printf("%d: %d\n",d,i);
      selection[d] = elem;
      val += elem->val;
      return recurs_select_independent_groups(tab,i+1,n,arity,d+1,M,val,best_val,selection,best_selection);
    }
    i++;
  }
  return 0;
}


int test_independent_groups(group_list_t **tab,int i,int n,int arity,int d,int M,double val,double *best_val,group_list_t **selection,group_list_t **best_selection)
{
  group_list_t *elem = NULL;

  if( d == M ){
    /*display_selection(selection,M,arity,val);*/
    return 1;
  }

  while( i < n ){
    elem = tab[i];
    if(independent_groups(selection,d,elem,arity)){
      /*printf("%d: %d\n",d,i);*/
      selection[d] = elem;
      val += elem->val;
      return recurs_select_independent_groups(tab,i+1,n,arity,d+1,M,val,best_val,selection,best_selection);
    }
    i++;
  }
  return 0;
}

void  delete_group_list(group_list_t *list)
{
  if(list){
    delete_group_list(list->next);
    FREE(list->tab);
    FREE(list);
  }
}

int group_list_id(const void* x1,const void* x2)
{
  group_list_t *e1 = NULL,*e2= NULL;

  e1 = *((group_list_t**)x1);
  e2 = *((group_list_t**)x2);

  return (e1->tab[0]->id < e2->tab[0]->id) ? - 1 : 1;
}

int group_list_asc(const void* x1,const void* x2)
{
  group_list_t *e1 = NULL,*e2 = NULL;

  e1 = *((group_list_t**)x1);
  e2 = *((group_list_t**)x2);

  return (e1->val < e2->val) ? - 1 : 1;
}

int group_list_dsc(const void* x1,const void* x2)
{
  group_list_t *e1 = NULL,*e2 = NULL;

  e1 = *((group_list_t**)x1);
  e2 = *((group_list_t**)x2);

  return (e1->val > e2->val) ? -1 : 1;
}

int weighted_degree_asc(const void* x1,const void* x2)
{
  group_list_t *e1= NULL,*e2 = NULL;

  e1 = *((group_list_t**)x1);
  e2 = *((group_list_t**)x2);

  return (e1->wg > e2->wg) ? 1 : -1;
}

int weighted_degree_dsc(const void* x1,const void* x2)
{
  group_list_t *e1 = NULL,*e2 = NULL;

  e1 = *((group_list_t**)x1);
  e2 = *((group_list_t**)x2);

  return (e1->wg > e2->wg) ? - 1 : 1;
}

int  select_independent_groups(group_list_t **tab_group,int n,int arity,int M,double *best_val,
			       group_list_t **best_selection,int bound,double max_duration)
{
  int i,j;
  group_list_t **selection = NULL;
  double val,duration;
  CLOCK_T time1,time0;

  if(verbose_level>=INFO){
    for(i=0;i<n;i++){
      for(j=0;j<arity;j++){
	printf("%d ",tab_group[i]->tab[j]->id);
      }
      printf(" : %f\n",tab_group[i]->val);
    }
  }



  selection = (group_list_t **)MALLOC(sizeof(group_list_t*)*M);
  CLOCK(time0);
  for( i = 0 ; i < MIN(bound,n) ; i++ ){
    /* if(!(i%100)) {printf("%d/%d ",i, MIN(bound,n)); fflush(stdout);} */
    selection[0] = tab_group[i];
    val = tab_group[i]->val;
    recurs_select_independent_groups(tab_group,i+1,n,arity,1,M,val,best_val,selection,best_selection);
    if((!(i%5)) && (max_duration>0)){
     CLOCK(time1);
      duration = CLOCK_DIFF(time1,time0);
      if(duration>max_duration){
	FREE(selection);
	return 1;
      }
    }
  }
  FREE(selection);


  if(verbose_level>=INFO)
    display_selection(best_selection,M,arity,*best_val);
  return 0;
}

int  select_independent_groups_by_largest_index(group_list_t **tab_group,int n,int arity,int M,double *best_val,group_list_t **best_selection,int bound,double max_duration)
{
  int i,dec,nb_groups=0;
  group_list_t **selection = NULL;
  double val,duration;
  CLOCK_T time1,time0;

  selection = (group_list_t **)MALLOC(sizeof(group_list_t*)*M);
  CLOCK(time0);

  dec = MAX(n/10000,2);
  for( i = n-1 ; i >= 0 ; i -= dec*dec){
    selection[0] = tab_group[i];
    val = tab_group[i]->val;
    nb_groups += test_independent_groups(tab_group,i+1,n,arity,1,M,val,best_val,selection,best_selection);
    if(verbose_level>=DEBUG)
      printf("%d:%d\n",i,nb_groups);

    if(nb_groups >= bound){
      FREE(selection);
      return 0;
    }
    if((!(i%5)) && (max_duration>0)){
      CLOCK(time1);
      duration=CLOCK_DIFF(time1,time0);
      if(duration>max_duration){
	FREE(selection);
	return 1;
      }
    }
  }

  FREE(selection);
  return 0;
}

void list_to_tab(group_list_t *list,group_list_t **tab,int n)
{
  int i;
  for( i = 0 ; i < n ; i++ ){
    if(!list){
      if(verbose_level>=CRITICAL)
	fprintf(stderr,"Error not enough elements. Only %d on %d\n",i,n);
      exit(-1);
    }
    tab[n-i-1] = list;
    list = list->next;
  }
  if(list){
    if(verbose_level>=DEBUG)
      fprintf(stderr,"Error too many elements\n");
    exit(-1);
  }
}

void display_tab_group(group_list_t **tab, int n,int arity)
{
  int i,j;
  if(verbose_level<DEBUG)
    return;
  for( i = 0 ; i < n ; i++ ){
    for( j = 0 ; j < arity ; j++ )
      printf("%d ",tab[i]->tab[j]->id);
    printf(": %.2f %.2f\n",tab[i]->val,tab[i]->wg);
  }
}

int independent_tab(tree_t **tab1,tree_t **tab2,int n)
{
  int i = 0,j = 0;

  while( (i<n) && (j<n) ){
    if(tab1[i]->id == tab2[j]->id)
      return 0;
    else if(tab1[i]->id > tab2[j]->id)
      j++;
    else
      i++;
  }
  return 1;
}

void compute_weighted_degree(group_list_t **tab, int n,int arity)
{
  int i,j;
  for( i = 0 ; i < n ; i++)
    tab[i]->sum_neighbour = 0;
  for( i = 0 ; i < n ; i++ ){
    /*printf("%d/%d=%f%%\n",i,n,(100.0*i)/n);*/
    for( j = i+1 ; j < n ; j++ )
      /*if(!independent_groups(&tab[i],1,tab[j],arity)){*/
      if(!independent_tab(tab[i]->tab,tab[j]->tab,arity)){
	tab[i]->sum_neighbour += tab[j]->val;
	tab[j]->sum_neighbour += tab[i]->val;
      }

    tab[i]->wg = tab[i]->sum_neighbour/tab[i]->val;
    if(tab[i]->sum_neighbour == 0)
      tab[i]->wg = 0;
    /*printf("%d:%f/%f=%f\n",i,tab[i]->sum_neighbour,tab[i]->val,tab[i]->wg);*/
  }
}

/*
  Very slow: explore all possibilities
  aff_mat : the affiity matrix at the considered level (used to evaluate a grouping)
  tab_node: array of the node to group
  parent: node to which attached the computed group
  id: current considered node of tab_node
  arity: number of children of parent (i.e.) size of the group to compute
  best_val: current value of th grouping
  cur_group: current grouping
 */
void  group(affinity_mat_t *aff_mat,tree_t *tab_node,tree_t *parent,int id,int arity, int n,double *best_val,tree_t **cur_group)
{

  int N = aff_mat->order;
  double val;
  int i;

  /*if we have found enough noide in the group*/
  if( n == arity){
    /* evaluate this group*/
    val = eval_grouping(aff_mat,cur_group,arity);
    /* If we improve compared to previous grouping: uodate the children of parent accordingly */
    if( val < *best_val ){
      *best_val = val;
      for( i = 0 ; i < arity ; i++ )
	  parent->child[i] = cur_group[i];
      parent->arity = arity;
    }
    return;
  }

  /*
    If we need more node in the group
    Continue to explore avilable nodes
  */
  for( i = id+1 ; i < N ; i++ ){
    /* If this node is allready in a group: skip it*/
    if(tab_node[i].parent)
      continue;
    /*Otherwise, add it to the group at place n*/
    cur_group[n] = &tab_node[i];
    /*
    printf("%d<-%d\n",n,i);
    recursively add the next element to this group
    */
    group(aff_mat,tab_node,parent,i,arity,n+1,best_val,cur_group);
  }
}

/*
  aff_mat : the affiity matrix at the considered level (used to evaluate a grouping)
  tab_node: array of the node to group
  parent: node to which attached the computed group
  id: current considered node of tab_node
  arity: number of children of parent (i.e.) size of the group to compute
  best_val: current value of th grouping
  cur_group: current grouping
  N: size of tab and tab_node. i.e. number of nodes at the considered level
 */
void  fast_group(affinity_mat_t *aff_mat,tree_t *tab_node,tree_t *parent,int id,int arity, int n,
		 double *best_val,tree_t **cur_group, int *nb_groups,int max_groups)
{
  double val;
  int i;
  int N = aff_mat->order;

  /*printf("Max groups=%d\n",max_groups);*/

  /*if we have found enough node in the group*/
  if( n == arity ){
    (*nb_groups)++;
    /*evaluate this group*/
    val = eval_grouping(aff_mat,cur_group,arity);
    /* If we improve compared to previous grouping: uodate the children of parent accordingly*/
    if( val < *best_val ){
      *best_val = val;
      for( i = 0 ; i < arity ; i++ )
	parent->child[i] = cur_group[i];

      parent->arity = arity;
    }
    return;
  }

  /*
    If we need more node in the group
    Continue to explore avilable nodes
  */
  for( i = id+1 ; i < N ; i++ ){
    /* If this node is allready in a group: skip it*/
    if(tab_node[i].parent)
      continue;
    /*Otherwise, add it to the group at place n */
    cur_group[n] = &tab_node[i];
    /*
    printf("%d<-%d %d/%d\n",n,i,*nb_groups,max_groups);
    exit(-1);
    recursively add the next element to this group
    */
    fast_group(aff_mat,tab_node,parent,i,arity,n+1,best_val,cur_group,nb_groups,max_groups);
    if(*nb_groups > max_groups)
      return;
  }
}


void fast_grouping(affinity_mat_t *aff_mat,tree_t *tab_node, tree_t *new_tab_node, int arity, int M,long int k)
{
  tree_t **cur_group = NULL;
  int l,i,nb_groups;
  double best_val,val=0;

  cur_group = (tree_t**)MALLOC(sizeof(tree_t*)*arity);
  for( l = 0 ; l < M ; l++ ){
    best_val = DBL_MAX;
    nb_groups = 0;
    /*printf("k%d/%d, k=%ld\n",l,M,k);*/
    /* select the best greedy grouping among the 10 first one*/
    /*fast_group(tab,tab_node,&new_tab_node[l],-1,arity,0,&best_val,cur_group,N,&nb_groups,MAX(2,(int)(50-log2(k))-M/10));*/
    fast_group(aff_mat,tab_node,&new_tab_node[l],-1,arity,0,&best_val,cur_group,&nb_groups,MAX(1,(int)(50-CmiLog2(k))-M/10));
    val += best_val;
    for( i = 0 ; i < new_tab_node[l].arity ; i++ )
      new_tab_node[l].child[i]->parent=&new_tab_node[l];
    update_val(aff_mat,&new_tab_node[l]);
  }

  FREE(cur_group);

  if(verbose_level>=INFO)
    printf("val=%f\n",val);
  /*exit(-1);*/

  if(verbose_level>=INFO)
    display_grouping(new_tab_node,M,arity,val);

}


int adjacency_asc(const void* x1,const void* x2)
{
  adjacency_t *e1 = NULL,*e2 = NULL;

  e1 = ((adjacency_t*)x1);
  e2 = ((adjacency_t*)x2);

  return (e1->val < e2->val) ? - 1 : 1;
}

int adjacency_dsc(const void* x1,const void* x2)
{
  adjacency_t *e1 = NULL,*e2 = NULL;

  e1 = ((adjacency_t*)x1);
  e2 = ((adjacency_t*)x2);


  return (e1->val > e2->val) ? -1 : 1;
}

void super_fast_grouping(affinity_mat_t *aff_mat,tree_t *tab_node, tree_t *new_tab_node, int arity, int M)
{
  double val = 0,duration;
  adjacency_t *graph;
  int i,j,e,l,nb_groups;
  int N = aff_mat->order;
  double **mat = aff_mat->mat;

  assert( 2 == arity);

  TIC;
  graph = (adjacency_t*)MALLOC(sizeof(adjacency_t)*((N*N-N)/2));
  e = 0;
  for( i = 0 ; i < N ; i++ )
    for( j = i+1 ; j < N ; j++){
      graph[e].i = i;
      graph[e].j = j;
      graph[e].val = mat[i][j];
      e++;
    }

  duration = TOC;
  if(verbose_level>=DEBUG)
    printf("linearization=%fs\n",duration);


  assert( e == (N*N-N)/2);
  TIC;
  qsort(graph,e,sizeof(adjacency_t),adjacency_dsc);
  duration = TOC;
  if(verbose_level>=DEBUG)
    printf("sorting=%fs\n",duration);

  TIC;

TIC;
  l = 0;
  nb_groups = 0;
  for( i = 0 ; (i < e) && (l < M) ; i++ )
    if(try_add_edge(tab_node,&new_tab_node[l],arity,graph[i].i,graph[i].j,&nb_groups))
      l++;

  for( l = 0 ; l < M ; l++ ){
    update_val(aff_mat,&new_tab_node[l]);
    val += new_tab_node[l].val;
  }

  duration = TOC;
  if(verbose_level>=DEBUG)
    printf("Grouping=%fs\n",duration);


  if(verbose_level>=DEBUG)
    printf("val=%f\n",val);


  display_grouping(new_tab_node,M,arity,val);

}


affinity_mat_t *build_cost_matrix(affinity_mat_t *aff_mat, double* obj_weight, double comm_speed)
{
  double **mat = NULL, *sum_row;
  double **old_mat;
  double avg;
  int i,j,N;

  if(!obj_weight)
    return aff_mat;

  N = aff_mat->order;
  old_mat = aff_mat -> mat;

  mat = (double**)MALLOC(N*sizeof(double*));
  for( i = 0 ; i < N ; i++ )
    mat[i] = (double*)MALLOC(N*sizeof(double));

  sum_row = (double*)CALLOC(N,sizeof(double));



  avg = 0;
  for( i = 0 ; i < N ; i++ )
    avg += obj_weight[i];
  avg /= N;


  if(verbose_level>=DEBUG)
    printf("avg=%f\n",avg);

  for( i = 0 ; i < N ; i++ )
    for( j = 0 ; j < N ; j++){
      if( i == j )
	mat[i][j] = 0;
      else{
	mat[i][j] = 1e-4*old_mat[i][j]/comm_speed-fabs(avg-(obj_weight[i]+obj_weight[j])/2);
	sum_row[i] += mat[i][j];
      }
    }
  return new_affinity_mat(mat,sum_row,N);

}


/*
  aff_mat: affinity matrix at the considered level (use to evaluate a grouping)
  tab_node: array of the node to group
  new_tab_node: array of nodes at the next level (the parents of the node in tab_node once the grouping will be done).
  arity: number of children of parent (i.e.) size of the group to compute
  M: size of new_tab_node (i.e) the number of parents
*/
void group_nodes(affinity_mat_t *aff_mat,tree_t *tab_node, tree_t *new_tab_node, int arity, int M, double* obj_weigth, double comm_speed)
{

 /*
    N: size of tab and tab_node. i.e. number of nodes at the considered level
    Hence we have: M*arity=N
 */
  int N = aff_mat -> order;
  tree_t **cur_group = NULL;
  int j,l;
  unsigned int n;
  unsigned long int k;
  group_list_t list,**best_selection = NULL,**tab_group = NULL;
  double best_val,last_best;
  int timeout;
  affinity_mat_t *cost_mat = NULL; /*cost matrix taking into account the communiocation cost but also the weight of the object*/
  double duration;

  TIC;

  /* might return aff_mat (if obj_weight==NULL): do not FREE this tab in this case*/
  cost_mat = build_cost_matrix(aff_mat,obj_weigth,comm_speed);

  k = choose(N,arity);
  if(verbose_level>=INFO)
    printf("Number of groups:%ld\n",k);

  /* Todo: check if the depth is a criteria for speeding up the computation*/
  /*  if(k>30000||depth>5){*/
  if( k > 30000 ){

    double duration;

    TIC;
    if( arity <= 2 ){
      /*super_fast_grouping(tab,tab_node,new_tab_node,arity,N,M,k);*/
      if(verbose_level >= INFO )
	printf("Bucket Grouping...\n");
      bucket_grouping(cost_mat,tab_node,new_tab_node,arity,M);
    }else{
      if(verbose_level >= INFO)
	printf("Fast Grouping...\n");
      fast_grouping(cost_mat,tab_node,new_tab_node,arity,M,k);
    }

    duration = TOC;
    if(verbose_level>=INFO)
      printf("Fast grouping duration=%f\n",duration);

    if(verbose_level>=DEBUG)
      display_grouping(new_tab_node,M,arity,-1);

  }else{
    if(verbose_level>=INFO)
      printf("Grouping nodes...\n");
    list.next = NULL;
    list.val = 0; /*number of elements in the list*/
    cur_group = (tree_t**)MALLOC(sizeof(tree_t*)*arity);
    best_selection = (group_list_t **)MALLOC(sizeof(group_list_t*)*M);

    list_all_possible_groups(cost_mat,tab_node,0,arity,0,cur_group,&list);
    n = (int)list.val;
    assert( n == k );
    tab_group = (group_list_t**)MALLOC(sizeof(group_list_t*)*n);
    list_to_tab(list.next,tab_group,n);
    if(verbose_level>=INFO)
      printf("List to tab done\n");

    best_val = DBL_MAX;

    /* perform the pack mapping fist*/
    /* timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,1,0.1); */
    timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,1,100);
    if(verbose_level>=INFO)
      if(timeout)
	printf("Packed mapping timeout!\n");
    /* give this mapping an exra credit (in general MPI application are made such that
       neighbour process communicates more than distant ones) */
    best_val /= 1.001;
    /* best_val *= 1.001; */
    if(verbose_level>=INFO)
      printf("Packing computed\n");

    /* perform a mapping trying to use group that cost less first*/
    qsort(tab_group,n,sizeof(group_list_t*),group_list_asc);
    last_best = best_val;
    timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,10,0.1);
    /* timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,n,0); */
    if(verbose_level>=INFO){
      if(timeout){
	printf("Cost less first timeout!\n");
      }else if(last_best>best_val){
	printf("Cost less first Impoved solution\n");
      }
      printf("----\n");
    }
    /* perform a mapping trying to minimize the use of groups that cost a lot */
    qsort(tab_group,n,sizeof(group_list_t*),group_list_dsc);
    last_best=best_val;
    timeout=select_independent_groups_by_largest_index(tab_group,n,arity,M,&best_val,best_selection,10,0.1);
    if(verbose_level>=DEBUG){
      if(timeout)
	printf("Cost most last timeout!\n");
      else if(last_best>best_val)
	printf("Cost most last impoved solution\n");
    }
      if( n < 10000 ){
      /* perform a mapping in the weighted degree order */


    if(verbose_level>=INFO)
      printf("----WG----\n");

      compute_weighted_degree(tab_group,n,arity);

      if(verbose_level>=INFO)
	printf("Weigted degree computed\n");

      qsort(tab_group,n,sizeof(group_list_t*),weighted_degree_dsc);
      /* display_tab_group(tab_group,n,arity);*/
      last_best = best_val;
      timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,10,0.1);
      /* timeout = select_independent_groups(tab_group,n,arity,M,&best_val,best_selection,n,0); */

      if(verbose_level>=DEBUG){
	if(timeout)
	  printf("WG timeout!\n");
	else if(last_best>best_val)
	  printf("WG impoved solution\n");
      }
    }

    qsort(best_selection,M,sizeof(group_list_t*),group_list_id);

    for( l = 0 ; l < M ; l++ ){
      for( j = 0 ; j < arity ; j++ ){
	new_tab_node[l].child[j]         = best_selection[l]->tab[j];
	new_tab_node[l].child[j]->parent = &new_tab_node[l];
      }
      new_tab_node[l].arity = arity;

      /* printf("arity=%d\n",new_tab_node[l].arity); */
      update_val(cost_mat,&new_tab_node[l]);
    }

    delete_group_list((&list)->next);
    FREE(best_selection);
    FREE(tab_group);
    FREE(cur_group);
  }

  if(cost_mat != aff_mat){
    FREE_tab_double(cost_mat->mat,N);
    FREE(cost_mat->sum_row);
    FREE(cost_mat);
  }

  duration = TOC;

  if(verbose_level>=INFO)
    display_grouping(new_tab_node,M,arity,-1);


  if(verbose_level>=INFO)
    printf("Grouping done in %.4fs!\n",duration);
}

void complete_aff_mat(affinity_mat_t **aff_mat ,int N, int K)
{
  double **old_mat = NULL,**new_mat = NULL; double *sum_row;
  int M,i;

  old_mat = (*aff_mat) -> mat;

  M = N+K;
  new_mat = (double**)MALLOC(M*sizeof(double*));
  for( i = 0 ; i < M ; i++ )
    new_mat[i] = (double*)CALLOC((M),sizeof(double));

  sum_row = (double*) CALLOC(M,sizeof(double));

  for( i = 0 ; i < N ; i++ ){
    memcpy(new_mat[i],old_mat[i],N*sizeof(double));
    sum_row[i] = (*aff_mat)->sum_row[i];
  }

  *aff_mat = new_affinity_mat(new_mat,sum_row,M);
}

void complete_obj_weight(double **tab,int N, int K)
{
  double *old_tab = NULL,*new_tab = NULL,avg;
  int M,i;

  old_tab = *tab;

  if(!old_tab)
    return;

  avg = 0;
  for( i = 0 ; i < N ; i++ )
    avg += old_tab[i];
  avg /= N;

  M = N+K;
  new_tab = (double*)MALLOC(M*sizeof(double));

  *tab = new_tab;
  for( i = 0 ; i < M ; i++ )
    if(i < N)
      new_tab[i] = old_tab[i];
    else
      new_tab[i] = avg;
}

void create_dumb_tree(tree_t *node,int depth,tm_topology_t *topology)
{
  tree_t **list_child = NULL;
  int arity,i;

  if( depth == topology->nb_levels-1) {
    set_node(node,NULL,0,NULL,-1,0,NULL,depth);
    return;
  }

  arity = topology->arity[depth];
  assert(arity>0);
  list_child = (tree_t**)CALLOC(arity,sizeof(tree_t*));
  for( i = 0 ; i < arity ; i++ ){
    list_child[i] = (tree_t*)MALLOC(sizeof(tree_t));
    create_dumb_tree(list_child[i],depth+1,topology);
    list_child[i]->parent = node;
    list_child[i]->dumb = 1;
  }

  set_node(node,list_child,arity,NULL,-1,0,list_child[0], depth);
}

void complete_tab_node(tree_t **tab,int N, int K,int depth,tm_topology_t *topology)
{
  tree_t *old_tab = NULL,*new_tab = NULL;
  int M,i;

  if( K == 0 )
    return;

  old_tab = *tab;

  M = N+K;
  new_tab = (tree_t*)MALLOC(M*sizeof(tree_t));

  *tab = new_tab;
  for( i = 0 ; i < M ; i++ )
    if(i < N)
      clone_tree(&new_tab[i],&old_tab[i]);
    else{
      create_dumb_tree(&new_tab[i],depth,topology);
      new_tab[i].id = i;
    }

  /* do not suppress tab if you are at the depth-most level it will be used at the mapping stage */
  FREE(old_tab);
}

void set_deb_tab_child(tree_t *tree, tree_t *child,int depth)
{
  /* printf("depth=%d\t%p\t%p\n",depth,child,tree);*/
  if( depth > 0 )
    set_deb_tab_child(tree->tab_child,child,depth-1);
  else
    tree->tab_child=child;
}

/*
Build the tree of the matching. It is a bottom up algorithm: it starts from the bottom of the tree on proceed by decreasing the depth
It groups nodes of the matrix tab and link these groups to the nodes of the under level.
Then it calls recursively the function to prefrom the grouping at the above level.

tab_node: array of nodes of the under level.
aff_mat: local affinity matrix
arity: arity of the nodes of the above level.
depth: current depth of the algorithm
toplogy: description of the hardware topology.
constraints:  set of constraints: core ids where to bind the processes
*/
tree_t *build_level_topology(tree_t *tab_node, affinity_mat_t *aff_mat,int arity,int depth,tm_topology_t *topology,
			     double *obj_weight, double *comm_speed)
{

  /* N: number of nodes. Order of com_mat, size of obj_weight */
  int N=aff_mat->order ;
  int i,K=0,M; /*M = N/Arity: number the groups*/
  tree_t *new_tab_node = NULL; /*array of node for this level (of size M): there will be linked to the nodes of tab_nodes*/
  affinity_mat_t * new_aff_mat= NULL; /*New communication matrix (after grouyping nodes together)*/
  tree_t *res = NULL; /*resulting tree*/
  int completed = 0;
  double speed; /* communication speed at this level*/
  double *new_obj_weight = NULL;
  double duration;

  if( 0 == depth ){
    if((1 == N) && (0 == depth))
      return &tab_node[0];
    else {
      if(verbose_level >= CRITICAL)
	fprintf(stderr,"Error: matrix size: %d and depth:%d (should be 1 and -1 respectively)\n",N,depth);
      exit(-1);
    }
  }

  /* If the number of nodes does not divide the arity: we add K nodes  */
  if( N%arity != 0 ){
    TIC;
    K = arity*((N/arity)+1)-N;
    /*printf("****N=%d arity=%d K=%d\n",N,arity,K);  */
    /*display_tab(tab,N);*/
    /* add K rows and columns to comm_matrix*/
    complete_aff_mat(&aff_mat,N,K);
    /* add K element to the object weight*/
    complete_obj_weight(&obj_weight,N,K);
    /*display_tab(tab,N+K);*/
    /* add a dumb tree to the K new "virtual nodes"*/
    complete_tab_node(&tab_node,N,K,depth,topology);
    completed = 1; /*flag this addition*/
    N += K; /*increase the number of nodes accordingly*/
    duration = TOC;
    if(verbose_level >= INFO)
      fprintf(stderr,"Completing matrix duration= %fs\n ", duration);
  } /*display_tab(tab,N);*/

  M = N/arity;
  if(verbose_level >= INFO)
    printf("Depth=%d\tnb_nodes=%d\tnb_groups=%d\tsize of groups(arity)=%d\n",depth,N,M,arity);

  TIC;
  /*create the new nodes*/
  new_tab_node = (tree_t*)MALLOC(sizeof(tree_t)*M);
  /*intitialize each node*/
  for( i = 0 ; i < M ; i++ ){
    tree_t **list_child = NULL;
    list_child = (tree_t**)CALLOC(arity,sizeof(tree_t*));
    set_node(&new_tab_node[i],list_child,arity,NULL,i,0,tab_node,depth);
  }
  duration = TOC;
  if(verbose_level >= INFO)
    printf("New nodes creation= %fs\n ", duration);

  /*Core of the algorithm: perfrom the grouping*/
  if(comm_speed)
    speed = comm_speed[depth];
  else
    speed = -1;
  group_nodes(aff_mat, tab_node, new_tab_node, arity, M, obj_weight, speed);

  TIC;
  /*based on that grouping aggregate the communication matrix*/
  new_aff_mat = aggregate_aff_mat(new_tab_node,aff_mat,M);
  duration = TOC;
  if(verbose_level >= INFO)
    printf("Aggregate_com_mat= %fs\n", duration);
  TIC;


  /*based on that grouping aggregate the object weight matrix*/
  new_obj_weight = aggregate_obj_weight(new_tab_node,obj_weight,M);
  duration = TOC;
  if(verbose_level >= INFO)
    printf("Aggregate obj_weight= %fs\n ", duration);

  /* set ID of virtual nodes to -1*/
  for( i = N-K ; i < N ; i++ )
    tab_node[i].id = -1;
  /*
  for(i=0;i<N;i++)
    display_node(&tab_node[i]);
  display_tab(new_com_mat,M);
  */

  /* decrease depth and compute arity of the above level*/
  depth--;
  if(depth > 0)
    arity = topology->arity[depth-1];
  else
    arity = 1;
  /* assume all objects have the same arity*/
  res = build_level_topology(new_tab_node, new_aff_mat, arity, depth,topology, new_obj_weight, comm_speed);

  set_deb_tab_child(res,tab_node,depth);

  /* if we have extended the matrix with zero, free the data here as they are local to this recursive step only*/
  if(completed){
    FREE_tab_double(aff_mat->mat,aff_mat->order);
    FREE(aff_mat->sum_row);
    FREE(aff_mat);
    FREE(obj_weight);
  }
  FREE_tab_double(new_aff_mat->mat,new_aff_mat->order);
  FREE(new_aff_mat->sum_row);
  FREE(new_aff_mat);
  FREE(new_obj_weight);

  FREE(new_obj_weight);

  return res;
}

double speed(int depth)
{
  /*
    Bertha values
    double tab[5]={21,9,4.5,2.5,0.001};
    double tab[5]={1,1,1,1,1};
    double tab[6]={100000,10000,1000,500,100,10};
  */
  double tab[11] = {1024,512,256,128,64,32,16,8,4,2,1};

  return 1.0/tab[depth];
  /*
   return 10*log(depth+2);
   return (depth+1);
   return (long int)pow(100,depth);
  */
}



/* check the leaf numbering of the topology
   this number must be between 0 and n-1 (the number of leaves)
   teh number must all be different
   However if a given leaf number is -1, it means that this
   leaf cannot bee used for the mapping

   The function returns the number of constraints (leaves that can be used)
   and their numbers (in increasing order) in the array pointed by contraints

*/

int check_constraints(tm_topology_t  *topology, int **constraints)
{
  int j,i,n = nb_processing_units(topology);
  int *tab_constraints = NULL, nb_constraints = 0;
  int *tab_node = NULL;
  int *count = NULL;

  /* tab_node: array of core numbers.
     tab_node[i]=-1 if this core is forbiden
     numbering is such that
     0<=tab_node[i]<n
     and that there is only one core of a given number
  */
  tab_node = topology->node_id[topology->nb_levels-1];

  /* "count" counts the number of cores of a given  number.
     count[i]: number of cores of number i.
     0<=count[i]<=1
  */
  count = (int *)CALLOC(n,sizeof(int));
  for( i = 0 ; i < n ; i++ )
    if (tab_node[i] != -1){
      if( (tab_node[i] >= 0) && (tab_node[i] < n)){
	/* In the remaining, we assume that the core numbering is logical from 0 to n
	   so if tab_node[i]!=-1 this mean sthat we have to use core number i*/
	count[i]++;
	nb_constraints++;
      }else{
	if(verbose_level >= ERROR)
	  fprintf(stderr, "*** Error: Core numbering not between 0 and %d: tab_node[%d]=%d\n", n , i, tab_node[i]);
	*constraints = NULL;
	return 0;
      }
    }

  if(nb_constraints == 0){
    FREE(count);
    *constraints = NULL;
    return 0;
  }

  tab_constraints = (int*) MALLOC(sizeof(int)*nb_constraints);

  /* we can now use the "counting sort" to sort the constraint tab in increasing order in linear time*/
  j = 0;
  for( i = 0 ; i < n ; i++ )
    if(count[i])
      tab_constraints[j++] = i;

  /* if the constraint_tab is not full, this means that some count[i]>1*/
  if( j != nb_constraints ){
    if(verbose_level >= ERROR)
    fprintf(stderr,"*** Error: Duplicate numbering: j=%d, nb_constraints= %d\n",j, nb_constraints);
    FREE(tab_constraints);
    FREE(count);
    *constraints = NULL;
    return 0;
  }

  /* FREE local variables, assign result, return result*/
  FREE(count);
  *constraints = tab_constraints;
  return nb_constraints;
}

affinity_mat_t * build_affinity_mat(double **mat, int order){
  int i,j;
  double *sum_row = (double*) CALLOC (order, sizeof(double));

  for (i=0 ; i<order ; i++)
    for (j=0 ; j<order ; j++)
      sum_row[i] += mat[i][j];

  return new_affinity_mat(mat,sum_row,order);
}


tree_t *bottom_up_build_tree_from_topology(tm_topology_t *topology,double **com_mat,int N, double *obj_weight, double *comm_speed)
{
  int depth,i;
  tree_t *res = NULL,*tab_node = NULL;
  affinity_mat_t *aff_mat;

  tab_node = (tree_t*)MALLOC(sizeof(tree_t)*N);
  depth = topology->nb_levels;
  for( i = 0 ; i < N ; i++ )
    set_node(&tab_node[i],NULL,0,NULL,i,0,NULL,depth);

  aff_mat = build_affinity_mat(com_mat,N);

  if(verbose_level >= INFO)
    printf("nb_levels=%d\n",depth);
  /* assume all objects have the same arity*/
  res = build_level_topology(tab_node, aff_mat , topology->arity[depth-2], depth-1, topology, obj_weight, comm_speed);
  if(verbose_level >= INFO)
    printf("Build (top down) tree done!\n");

  /* tell the system it is not a constraint tree, this is usefull for freeing pointers*/
  res->constraint = 0;
  FREE(aff_mat -> sum_row);
  FREE(aff_mat);

  return res;
}




tree_t * build_tree_from_topology(tm_topology_t *topology, double **com_mat, int N, double *obj_weight, double *com_speed)
{
  int *constraints = NULL, nb_constraints;
  tree_t * result;

  verbose_level = get_verbose_level();

  nb_constraints = check_constraints (topology, &constraints);

  printf("nb_constraints = %d, N= %d; nb_processing units = %d\n",nb_constraints, N, nb_processing_units(topology));

  if(N>nb_constraints){
    if(verbose_level >= CRITICAL){
      printf("Error : More processes (%d) than number of constraints (%d)!\n",N ,nb_constraints);
    }
    exit(-1);
  }

  if(verbose_level >= INFO){
    printf("Com matrix size: %d\n",N);
    printf("nb_constraints: %d\n",nb_constraints);
  }

  if(nb_constraints == nb_processing_units(topology))
    {
      nb_constraints = 0;
      FREE(constraints);
    }

  if(nb_constraints){
    if(verbose_level >= INFO){
      printf("Partitionning with constraints\n");
    }
    result = kpartition_build_tree_from_topology(topology, com_mat, N, constraints, nb_constraints, obj_weight, com_speed);
    FREE(constraints);
    return result;
  }
  else{
    if(verbose_level >= INFO){
      printf("Partitionning without constraints\n");
    }
    return bottom_up_build_tree_from_topology(topology, com_mat, N, obj_weight, com_speed);
  }
}
