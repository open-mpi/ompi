#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#include "tm_mt.h"
#include "tm_mapping.h"
#include "tm_timings.h"
#include "tm_tree.h"

#ifdef _WIN32
#include <windows.h>
#include <winbase.h>
#endif

#define TEST_ERROR(n) do{ \
    if( (n) != 0 ){       \
       fprintf(stderr,"Error %d Line %d\n",n,__LINE__); \
       exit(-1);} \
  }while(0)

#define LINE_SIZE (1000000)

typedef struct {
  int  val;
  long key;
} hash_t;


typedef struct {
  double val;
  int key1;
  int key2;
} hash2_t;

int distance(tm_topology_t *topology,int i, int j);
int nb_lines(char *);
void init_comm(char *,int,double **);void map_Packed(tm_topology_t *,int,int *);
void map_RR(int ,int *,int *);
int hash_asc(const void*,const void*);
int *generate_random_sol(tm_topology_t *,int,int,int);
double eval_sol(int *,int,double **,double **);
double eval_sol_inv(int *,int,double **,double **);
void exchange(int *,int,int);
double gain_exchange(int *,int,int,double,int,double **,double **);
void select_max(int *,int *,double **,int,int *);
void compute_gain(int *,int,double **,double **,double **);
void map_MPIPP(tm_topology_t *,int,int,int *,double **,double **);
void depth_first(tree_t *,int *,int *);
int nb_leaves(tree_t *);
void map_topology(tm_topology_t *,tree_t *,int,int,int *,int,int *);
int int_cmp(const void*,const void*);
int decompose(int,int,int *);
tree_t *build_synthetic_topology_old(int *,int,int,int);
void update_comm_speed(double **,int,int);
void topology_numbering(tm_topology_t *,int **,int *);
void topology_arity(tm_topology_t *,int **,int *);
void optimize_arity(int **,int *,int);
int get_indice(int *,int,int);
int  fill_tab(int **,int *,int,int,int,int);
void update_canonical(int *,int,int,int);
int constraint_dsc(const void*,const void*);
void  display_contsraint_tab(constraint_t *,int);
void update_perm(int *,int,constraint_t *,int,int);
void recursive_canonicalization(int,tm_topology_t *,int *,int *,int *,int,int);
void FREE_topology(tm_topology_t *);


int distance(tm_topology_t *topology,int i, int j)
{
  int level = topology->nb_levels;
  int arity;
  int f_i = i,f_j = j;

  do{
    level--;
    arity = topology->arity[level];
    if( arity == 0 )
      arity = 1;
    f_i = f_i/arity;
    f_j = f_j/arity;
  } while(f_i!=f_j);

  /* printf("(%d,%d):%d\n",i,j,level);*/
  /* exit(-1); */
  return level;
}

int nb_processing_units(tm_topology_t *topology)
{
  return topology->nb_nodes[topology->nb_levels-1];
}


void FREE_topology(tm_topology_t *topology)
{
  int i;
  for( i = 0 ; i < topology->nb_levels ; i++ )
    FREE(topology->node_id[i]);
  FREE(topology->node_id);
  FREE(topology->nb_nodes);
  FREE(topology->arity);
  FREE(topology);
}

double print_sol(int N,int *Value,double **comm, double *cost, tm_topology_t *topology)
{
  double a,c,sol;
  int i,j;

  sol = 0;
  for ( i = 0 ; i < N ; i++ )
    for ( j = i+1 ; j < N ; j++){
      c = comm[i][j];
      a = cost[distance(topology,Value[i],Value[j])];
      /* printf("T_%d_%d %f/%f=%f\n",i,j,c,a,c/a); */
      sol += c/a;
    }

  for (i = 0; i < N; i++) {
    printf("%d", Value[i]);
    if(i<N-1)
      printf(",");
  }
  printf(" : %g\n",sol);

  return sol;
}



double print_sol_inv(int N,int *Value,double **comm, double *cost, tm_topology_t *topology)
{
  double a,c,sol;
  int i,j;

  sol = 0;
  for ( i = 0 ; i < N ; i++)
    for ( j = i+1 ; j < N ; j++){
      c = comm[i][j];
      a = cost[distance(topology,Value[i],Value[j])];
      /* printf("--T_%d_%d %f/%f=%f\n",i,j,c,a,c/a);  */
      sol += c*a;
    }

  for (i = 0; i < N; i++) {
    printf("%d", Value[i]);
    if( i < (N-1) )
      printf(",");
  }
  printf(" : %g\n",sol);

  return sol;
}

void print_1D_tab(int *tab,int N)
{
  int i;
  for (i = 0; i < N; i++) {
    printf("%d", tab[i]);
    if( i < (N-1) )
      printf(",");
  }
  printf("\n");
}

int nb_lines(char *filename)
{
  FILE *pf = NULL;
  char line[LINE_SIZE];
  int N = 0;

  if(!(pf = fopen(filename,"r"))){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",filename);
      exit(-1);
  }

  while(fgets(line,LINE_SIZE,pf))
    N++;

  if(get_verbose_level() >= DEBUG)
    printf("Number of lines of file %s = %d\n",filename,N);

  fclose(pf);
  return N;
}

void init_comm(char *filename,int N,double **comm)
{
  FILE *pf = NULL;
  char *ptr= NULL;
  char line[LINE_SIZE];
  int i,j;
  unsigned int vl = get_verbose_level();



  if(!(pf=fopen(filename,"r"))){
    if(vl >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",filename);
    exit(-1);
  }

  j = -1;
  i = 0;
  while(fgets(line,LINE_SIZE,pf)){
    char *l = line;
    j = 0;
    comm[i][N] = 0;
    /* printf("%s|",line); */
    while((ptr=strtok(l," \t"))){
      l = NULL;
      if((ptr[0]!='\n')&&(!isspace(ptr[0]))&&(*ptr)){
	comm[i][j] = atof(ptr);
	comm[i][N] += comm [i][j];
	/* printf ("comm[%d][%d]=%f|%s|\n",i,j,comm[i][j],ptr); */
	j++;
      }
    }
    if( j != N){
      if(vl >= CRITICAL)
	fprintf(stderr,"Error at %d %d (%d!=%d)for %s\n",i,j,j,N,filename);
      exit(-1);
    }
    i++;
  }
  if( i != N ){
    if(vl >= CRITICAL)
      fprintf(stderr,"Error at %d %d for %s\n",i,j,filename);
    exit(-1);
  }
  /*
    printf("%s:\n",filename);
  for(i=0;i<N;i++){
    for(j=0;j<N+1;j++){
      printf("%6.1f ",comm[i][j]);
    }
    printf("\n");
    }
  */

  fclose (pf);
}

int  build_binding_constraints(char *filename, int **ptab)
{
  int *tab = NULL;
  FILE *pf = NULL;
  char  line[LINE_SIZE],*l = NULL;
  char *ptr = NULL;
  int i,n;
  unsigned int vl = get_verbose_level();

  if (!(pf = fopen(filename,"r"))) {
    if(vl >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",filename);
    exit(-1);
  }

  /* compute the size od the array to store the constraints*/
  n = 0;
  fgets(line, LINE_SIZE, pf);
  l = line;
  while((ptr=strtok(l," \t"))){
    l = NULL;
    if((ptr[0] != '\n') && ( !isspace(ptr[0])) && (*ptr) && (ptr))
      n++;
  }

  tab = (int*)MALLOC(n*sizeof(int));

  rewind(pf);
  fgets(line, LINE_SIZE, pf);
  l = line;
  i = 0;
  while((ptr=strtok(l," \t"))){
    l = NULL;
    if((ptr[0] != '\n') && ( !isspace(ptr[0])) && (*ptr) && (ptr)){
      if(i <= n)
	tab[i] = atoi(ptr);
      else{
	if(vl >= CRITICAL)
	  fprintf(stderr, "More than %d entries in %s\n", n, filename);
	exit(-1);
      }
      i++;
    }
  }

  if( i != n ){
    if(vl >= CRITICAL)
      fprintf(stderr, "Read %d entries while expecting %d ones\n", i, n);
    exit(-1);
  }

  *ptab = tab;
  return n;
}

int  build_comm(char *filename,double ***pcomm)
{
  double **comm = NULL;
  int i,N;

  if(get_verbose_level() >= INFO)
    printf("Reading communication matrix file: %s\n",filename);

  N = nb_lines(filename);
  comm = (double**)MALLOC(N*sizeof(double*));
  for( i = 0 ; i < N ; i++)
    /* the last column stores the sum of the line*/
    comm[i] = (double*)MALLOC((N+1)*sizeof(double));
  init_comm(filename,N,comm);
  *pcomm = comm;

  if(get_verbose_level() >= INFO)
    printf("Communication matrix built from %s!\n",filename);

  return N;
}

void map_Packed(tm_topology_t *topology,int N,int *Value)
{
  int i,j = 0,depth;

  depth = topology->nb_levels-1;

  for( i = 0 ; i < nb_processing_units(topology) ; i++){
    /* printf ("%d -> %d\n",objs[i]->os_index,i); */
    if(topology->node_id[depth][i] != -1){
      Value[j++]=topology->node_id[depth][i];
      if(j == N)
	break;
    }
  }
}

void map_RR(int N,int *Value, int *constraints)
{
  int i;

  for( i = 0 ; i < N ; i++ ){
    /*printf ("%d -> %d\n",i,i);*/
    if(constraints)
      Value[i]=constraints[i];
    else
      Value[i]=i;
  }
}

int hash_asc(const void* x1,const void* x2)
{
  hash_t *e1 = NULL,*e2 = NULL;

  e1 = ((hash_t*)x1);
  e2 = ((hash_t*)x2);

  return (e1->key < e2->key) ? -1 : 1;
}


int *generate_random_sol(tm_topology_t *topology,int N,int level,int seed)
{
  hash_t *hash_tab = NULL;
  int *sol = NULL;
  int *nodes_id= NULL;
  int i;

  nodes_id = topology->node_id[level];

  hash_tab = (hash_t*)MALLOC(sizeof(hash_t)*N);
  sol = (int*)MALLOC(sizeof(int)*N);

  init_genrand(seed);

  for( i = 0 ; i < N ; i++ ){
    hash_tab[i].val = nodes_id[i];
    hash_tab[i].key = genrand_int32();
  }

  qsort(hash_tab,N,sizeof(hash_t),hash_asc);
  for( i = 0 ; i < N ; i++ )
    sol[i] = hash_tab[i].val;

  FREE(hash_tab);
  return sol;
}


double eval_sol(int *sol,int N,double **comm, double **arch)
{
  double a,c,res;
  int i,j;

  res = 0;
  for ( i = 0 ; i < N ; i++ )
    for ( j = i+1 ; j < N ; j++ ){
      c = comm[i][j];
      a = arch[sol[i]][sol[j]];
      res += c/a;
    }

  return res;
}

double eval_sol_inv(int *sol,int N,double **comm, double **arch)
{
  double a,c,res;
  int i,j;

  res = 0;
  for ( i = 0 ; i < N ; i++ )
    for ( j = i+1 ; j < N ; j++ ){
      c = comm[i][j];
      a = arch[sol[i]][sol[j]];
      res += c*a;
    }

  return res;
}

void exchange(int *sol,int i,int j)
{
  int tmp;
  tmp = sol[i];
  sol[i] = sol[j];
  sol[j] = tmp;
}

double gain_exchange(int *sol,int l,int m,double eval1,int N,double **comm, double **arch)
{
  double eval2;
  if( l == m )
    return 0;
  exchange(sol,l,m);
  eval2 = eval_sol(sol,N,comm,arch);
  exchange(sol,l,m);

  return eval1-eval2;
}

void select_max(int *l,int *m,double **gain,int N,int *state)
{
  double max;
  int i,j;

  max = -DBL_MAX;

  for( i = 0 ; i < N ; i++ )
    if(!state[i])
      for( j = 0 ; j < N ; j++ )
	if( (i != j) && (!state[j]) ){
	  if(gain[i][j] > max){
	    *l = i;
	    *m = j;
	    max=gain[i][j];
	  }
	}
}

void compute_gain(int *sol,int N,double **gain,double **comm, double **arch)
{
  double eval1;
  int i,j;

  eval1 = eval_sol(sol,N,comm,arch);
  for( i = 0 ; i < N ; i++ )
    for( j = 0 ; j <= i ; j++)
      gain[i][j] = gain[j][i] = gain_exchange(sol,i,j,eval1,N,comm,arch);
}



/* Randomized Algorithm of
Hu Chen, Wenguang Chen, Jian Huang ,Bob Robert,and H.Kuhn. Mpipp: an automatic profile-guided
parallel process placement toolset for smp clusters and multiclusters. In
Gregory K. Egan and Yoichi Muraoka, editors, ICS, pages 353-360. ACM, 2006.
 */

void map_MPIPP(tm_topology_t *topology,int nb_seed,int N,int *Value,double **comm, double **arch)
{
  int *sol = NULL;
  int *state = NULL;
  double **gain = NULL;
  int **history = NULL;
  double *temp = NULL;
  int i,j,t,l=0,m=0,seed=0;
  double max,sum,best_eval,eval;

  gain = (double**)MALLOC(sizeof(double*)*N);
  history = (int**)MALLOC(sizeof(int*)*N);
  for( i = 0 ; i < N ; i++){
    gain[i] = (double*)MALLOC(sizeof(double)*N);
    history[i] = (int*)MALLOC(sizeof(int)*3);
  }

  state = (int*)MALLOC(sizeof(int)*N);
  temp = (double*)MALLOC(sizeof(double)*N);

  sol = generate_random_sol(topology,N,topology->nb_levels-1,seed++);
  for( i = 0 ; i < N ; i++)
    Value[i] = sol[i];

  best_eval = DBL_MAX;
  while(seed <= nb_seed){
    do{
      for( i =  0 ; i < N ; i++ ){
	state[i] = 0;
	/* printf("%d ",sol[i]); */
      }
      /* printf("\n"); */
      compute_gain(sol,N,gain,comm,arch);
      /*
      display_tab(gain,N);
      exit(-1);
      */
      for( i = 0 ; i < N/2 ; i++ ){
	select_max(&l,&m,gain,N,state);
	/* printf("%d: %d <=> %d : %f\n",i,l,m,gain[l][m]); */
	state[l] = 1;
	state[m] = 1;
	exchange(sol,l,m);
	history[i][1] = l;
	history[i][2] = m;
	temp[i] = gain[l][m];
	compute_gain(sol,N,gain,comm,arch);
      }

      t = -1;
      max = 0;
      sum = 0;
      for(i = 0 ; i < N/2 ; i++ ){
	sum += temp[i];
	if( sum > max ){
	  max = sum;
	  t = i;
	}
      }
      /*for(j=0;j<=t;j++)
	printf("exchanging: %d with %d for gain: %f\n",history[j][1],history[j][2],temp[j]); */
      for( j = t+1 ; j < N/2 ; j++ ){
	exchange(sol,history[j][1],history[j][2]);
	/* printf("Undoing: %d with %d for gain: %f\n",history[j][1],history[j][2],temp[j]);  */
      }
      /* printf("max=%f\n",max); */

      /*for(i=0;i<N;i++){
	printf("%d ",sol[i]);
	}
	printf("\n");*/
      eval = eval_sol(sol,N,comm,arch);
      if(eval < best_eval){
	best_eval = eval;
	for(i = 0 ; i < N ; i++)
	  Value[i] = sol[i];
	/* print_sol(N); */
      }
    }while( max > 0 );

    sol=generate_random_sol(topology,N,topology->nb_levels-1,seed++);
  }
}

/* void map_tree(tree_t* t1,tree_t *t2) */
/* { */
  /*  double x1,x2;
  if((!t1->left)&&(!t1->right)){
    printf ("%d -> %d\n",t1->id,t2->id);
    Value[t2->id]=t1->id;
   return;
  }
  x1=t2->right->val/t1->right->val+t2->left->val/t1->left->val;
  x2=t2->left->val/t1->right->val+t2->right->val/t1->left->val;
  if(x1<x2){
    map_tree(t1->left,t2->left);
    map_tree(t1->right,t2->right);
  }else{
    map_tree(t1->right,t2->left);
    map_tree(t1->left,t2->right);
    }*/
/* } */

void depth_first(tree_t *comm_tree, int *proc_list,int *i)
{
  int j;
  if(!comm_tree->child){
    proc_list[(*i)++] = comm_tree->id;
    return;
  }

  for( j  = 0 ; j < comm_tree->arity ; j++ )
    depth_first(comm_tree->child[j],proc_list,i);
}

int nb_leaves(tree_t *comm_tree)
{
  int j,n=0;

  if(!comm_tree->child)
    return 1;

  for( j = 0 ; j < comm_tree->arity ; j++)
    n += nb_leaves(comm_tree->child[j]);

  return n;
}


/*Map topology to cores:
 sigma_i is such that  process i is mapped on core sigma_i
 k_i is such that core i exectutes process k_i

 size of sigma is the number of process "nb_processes"
 size of k is the number of cores/nodes "topology->nb_nodes[level]"

 We must have numbe of process<=number of cores

 k_i =-1 if no process is mapped on core i
*/

void map_topology(tm_topology_t *topology,tree_t *comm_tree,int nb_compute_units,
		  int level,int *sigma, int nb_processes, int *k)
{
  int *nodes_id = NULL;
  int *proc_list = NULL;
  int i,N,M,block_size;
  unsigned int vl = get_verbose_level();

  M = nb_leaves(comm_tree);
  nodes_id = topology->node_id[level];
  N = topology->nb_nodes[level];

  if(vl >= INFO){
    printf("nb_leaves=%d\n",M);
    printf("level=%d, nodes_id=%p, N=%d\n",level,(void *)nodes_id,N);
    printf("N=%d,nb_compute_units=%d\n",N,nb_compute_units);
  }

  /* The number of node at level "level" in the tree should be equal to the number of processors*/
  assert(N==nb_compute_units);

  proc_list = (int*)MALLOC(sizeof(int)*M);
  i = 0;
  depth_first(comm_tree,proc_list,&i);

  if(vl >= DEBUG)
    for(i=0;i<M;i++){
      printf ("%d\n",proc_list[i]);
    }

  block_size = M/N;

  if(k){/*if we need the k vector*/
    if(vl >= INFO)
      printf("M=%d, N=%d, BS=%d\n",M,N,block_size);
    for( i = 0 ; i < nb_processing_units(topology) ; i++ )
      k[i] = -1;

    for( i = 0 ; i < M ; i++ )
      if(proc_list[i] != -1){
	if(vl >= DEBUG)
	  printf ("%d->%d\n",proc_list[i],nodes_id[i/block_size]);

	if( proc_list[i] < nb_processes ){
	  sigma[proc_list[i]] = nodes_id[i/block_size];
	  k[nodes_id[i/block_size]] = proc_list[i];
	}
      }
  }else{
    if(vl >= INFO)
      printf("M=%d, N=%d, BS=%d\n",M,N,block_size);
    for( i = 0 ; i < M ; i++ )
      if(proc_list[i] != -1){
	if(vl >= DEBUG)
	  printf ("%d->%d\n",proc_list[i],nodes_id[i/block_size]);
	if( proc_list[i] < nb_processes )
	  sigma[proc_list[i]] = nodes_id[i/block_size];
      }
  }

  if(vl >= DEBUG){
    printf("k: ");
    for( i = 0 ; i < nb_processing_units(topology) ; i++ )
      printf("%d ",k[i]);
    printf("\n");
  }


  FREE(proc_list);
}

void map_topology_simple(tm_topology_t *topology,tree_t *comm_tree, int *sigma, int nb_processes, int *k)
{
  map_topology(topology,comm_tree,topology->nb_nodes[topology->nb_levels-1],
	       topology->nb_levels-1,sigma,nb_processes,k);
}

int int_cmp(const void* x1,const void* x2)
{
  int *e1 = NULL,*e2= NULL;

  e1 = ((int *)x1);
  e2 = ((int *)x2);

  return ((*e1) > (*e2)) ? -1 : 1;
}


int decompose(int n,int optimize,int *tab)
{
  int primes[6] = {2,3,5,7,11,0};
  int i = 0,j = 1,flag = 2;
  unsigned int vl = get_verbose_level();

  while( primes[i] && (n!=1) ){
    /*    printf("[%d] before=%d\n",primes[i],n); */
    if( flag && optimize && (n%primes[i]!= 0) ){
      n += primes[i] - n%primes[i];
      flag--;
      i = 0;
      continue;
    }
    /* printf("after=%d\n",n); */
    if( n%primes[i] == 0 ){
      tab[j++] = primes[i];
      n /= primes[i];
    }else{
      i++;
      flag = 1;
    }
  }
  if( n != 1 )
    tab[j++] = n;

  qsort(tab+1,j-1,sizeof(int),int_cmp);

  if(vl >= DEBUG){
    for( i = 0 ; i < j ; i++ )
      printf("%d:",tab[i]);
    printf("\n");
  }

  tab[j] = 0;

  return (j+1);
}


tree_t *build_synthetic_topology_old(int *synt_tab,int id,int depth,int nb_levels)
{
  tree_t *res = NULL,**child = NULL;
  int arity = synt_tab[0];
  int val,i;

  res = (tree_t*)MALLOC(sizeof(tree_t));
  val = 0;
  if(depth >= nb_levels)
    child = NULL;
  else{
    child = (tree_t**)MALLOC(sizeof(tree_t*)*arity);
    for( i  = 0 ; i < arity ; i++ ){
      child[i] = build_synthetic_topology_old(synt_tab+1,i,depth+1,nb_levels);
      child[i]->parent = res;
      val += child[i]->val;
    }
  }
  set_node(res,child,arity,NULL,id,val+speed(depth),child[0],depth);
  return res;
}

void display_topology(tm_topology_t *topology)
{
  int i,j;

  for( i = 0 ; i < topology->nb_levels ; i++ ){
    printf("%d: ",i);
    for( j = 0 ; j < topology->nb_nodes[i] ; j++)
      printf("%d ",topology->node_id[i][j]);
    printf("\n");
  }
}

/*
   Build a synthetic balanced topology

   arity : array of arity of the first nb_level (of size nb_levels-1)
   core_numbering: numbering of the core by the system. Array of size nb_core_per_node

   nb_core_per_nodes: number of cores of a given node

   The numbering of the cores is done in round robin fashion after a width traversal of the topology
 */

tm_topology_t  *build_synthetic_topology(int *arity, int nb_levels, int *core_numbering, int nb_core_per_nodes)
{
  tm_topology_t *topology = NULL;
  int i,j,n = 1;

  topology = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  topology->arity = (int*)MALLOC(sizeof(int)*nb_levels);
  memcpy(topology->arity,arity,sizeof(int)*nb_levels);
  topology->nb_levels = nb_levels;

  topology->node_id = (int**)MALLOC(sizeof(int*)*topology->nb_levels);
  topology->nb_nodes = (int*)MALLOC(sizeof(int)*topology->nb_levels);

  for( i = 0 ; i < topology->nb_levels ; i++ ){
    topology->nb_nodes[i] = n;
    topology->node_id[i] = (int*)MALLOC(sizeof(int)*n);
    if( i < topology->nb_levels-1)
      for( j = 0 ; j < n ; j++ )
	topology->node_id[i][j] = j;
    else
      for( j = 0 ; j < n ; j++ )
	topology->node_id[i][j] = core_numbering[j%nb_core_per_nodes] + (nb_core_per_nodes)*(j/nb_core_per_nodes);

    n *= topology->arity[i];
  }
  return topology;
}


void   build_synthetic_proc_id(tm_topology_t *topology)
{
  int i;
  size_t j,n = 1;

  topology->node_id = (int**)MALLOC(sizeof(int*)*topology->nb_levels);
  topology->nb_nodes = (int*)MALLOC(sizeof(int)*topology->nb_levels);

  for( i = 0 ; i < topology->nb_levels ; i++ ){
    /* printf("n= %lld, arity := %d\n",n, topology->arity[i]); */
    topology->nb_nodes[i] = n;
    topology->node_id[i] = (int*)MALLOC(sizeof(long int)*n);
    if ( !topology->node_id[i] ){
      if(get_verbose_level() >= CRITICAL)
	fprintf(stderr,"Cannot allocate level %d (of size %ld) of the topology\n", i, (unsigned long int)n);
      exit(-1);
    }
    for( j = 0 ; j < n ; j++ )
      topology->node_id[i][j] = j;
    n *= topology->arity[i];
  }
}

void update_comm_speed(double **comm_speed,int old_size,int new_size)
{
  double *old_tab = NULL,*new_tab= NULL;
  int i;
  unsigned int vl = get_verbose_level();

  if(vl >= DEBUG)
    printf("comm speed [%p]: ",(void *)*comm_speed);

  old_tab = *comm_speed;
  new_tab = (double*)MALLOC(sizeof(double)*new_size);
  *comm_speed = new_tab;

  for( i = 0 ; i < new_size ; i++ ){
    if( i < old_size)
      new_tab[i] = old_tab[i];
    else
      new_tab[i] = new_tab[i-1];

    if(vl >= DEBUG)
      printf("%f ",new_tab[i]);
  }
  if(vl >= DEBUG)
    printf("\n");
}


/* d: size of comm_speed */
void TreeMatchMapping(int nb_obj, int nb_proc, double **comm_mat,  double *obj_weight, double * comm_speed, int d, int *sol)
{
  tree_t *comm_tree = NULL;
  tm_topology_t *topology= NULL;
  double duration;
  int i;
  unsigned int vl = get_verbose_level();

  TIC;

  for( i = 0 ; i < nb_obj ; i++ ){
    sol[i] = i;
    /* printf("%f ",obj_weight[i]); */
  }
  /*
  printf("\n");
  return;
  */

  topology = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  topology->arity = (int*)MALLOC(sizeof(int)*MAX_LEVELS);
  topology->arity[0] = nb_proc;
  topology->nb_levels = decompose((int)ceil((1.0*nb_obj)/nb_proc),1,topology->arity);
  if(vl >= INFO)
    printf("Topology nb levels=%d\n",topology->nb_levels);
  build_synthetic_proc_id(topology);

  if(topology->nb_levels > d)
    update_comm_speed(&comm_speed,d,topology->nb_levels);

  /*
  exit(-1);
  topology_to_arch(topology);

  display_tab(arch,hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_PROC));
  display_tab(arch,96);
  exit(-1);
  int nb_core=topo_nb_proc(topology,1000);

  display_tab(comm_mat,N);
  */

  TIC;
  comm_tree = build_tree_from_topology(topology,comm_mat,nb_obj,obj_weight,comm_speed);
  if(vl >= INFO)
    printf("Tree building time=%f\n",TOC);
  TIC;
  map_topology(topology,comm_tree,nb_proc,1,sol,nb_obj,NULL);
  if(vl >= INFO)
    printf("Topology mapping time=%f\n",TOC);

  if(topology->nb_levels > d)
    FREE(comm_speed);

  FREE_topology(topology);
  FREE_tree(comm_tree);

  duration=TOC;
  if(vl >= INFO)
    printf("-------------- Mapping done in %.4fs!\n",duration);
}

void display_other_heuristics(tm_topology_t *topology,int N,double **comm,int TGT_flag, int *constraints, double *cost)
{
  int *sol = NULL;

  sol = (int*)MALLOC(sizeof(int)*N);

  map_Packed(topology,N,sol);
  printf("Packed: ");
  if (TGT_flag == 1)
    print_sol_inv(N,sol,comm,cost, topology);
  else
    print_sol(N,sol,comm,cost, topology);

  map_RR(N,sol,constraints);
  printf("RR: ");
  if (TGT_flag == 1)
    print_sol_inv(N,sol,comm, cost, topology);
  else
    print_sol(N,sol,comm, cost, topology);

/*   double duration; */
/*   CLOCK_T time1,time0; */
/*   CLOCK(time0); */
/*   map_MPIPP(topology,1,N,sol,comm,arch); */
/*   CLOCK(time1); */
/*   duration=CLOCK_DIFF(time1,time0); */
/*   printf("MPIPP-1-D:%f\n",duration); */
/*   printf("MPIPP-1: "); */
/*   if (TGT_flag == 1)  */
/*     print_sol_inv(N,sol,comm,arch); */
/*   else */
/*   print_sol(N,sol,comm,arch); */

/*   CLOCK(time0); */
/*   map_MPIPP(topology,5,N,sol,comm,arch); */
/*   CLOCK(time1); */
/*   duration=CLOCK_DIFF(time1,time0); */
/*   printf("MPIPP-5-D:%f\n",duration); */
/*   printf("MPIPP-5: "); */
/*   if (TGT_flag == 1)  */
/*     print_sol_inv(N,sol,comm,arch); */
/*   else */
/*   print_sol(N,sol,comm,arch); */

  FREE(sol);
}

void topology_numbering(tm_topology_t *topology,int **numbering,int *nb_nodes)
{
  int nb_levels;
  unsigned int vl = get_verbose_level();

  nb_levels = topology->nb_levels;
  *nb_nodes = topology->nb_nodes[nb_levels-1];
  if(vl >= INFO)
    printf("nb_nodes=%d\n",*nb_nodes);
  *numbering = (int*)MALLOC(sizeof(int)*(*nb_nodes));
  memcpy(*numbering,topology->node_id[nb_levels-1],sizeof(int)*(*nb_nodes));
}

void topology_arity(tm_topology_t *topology,int **arity,int *nb_levels)
{
  *nb_levels = topology->nb_levels;
  *arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
  memcpy(*arity,topology->arity,sizeof(int)*(*nb_levels));
}

void optimize_arity(int **arity, int *nb_levels,int n)
{
  int a,i;
  int *new_arity = NULL;

  if( n < 0 )
    return;
  /*   printf("n=%d\tnb_levels=%d\n",n,*nb_levels); */
  /*   for(i=0;i<*nb_levels;i++) */
  /*     printf("%d:",(*arity)[i]); */
  /*   printf("\n");   */
  /* if(n==(*nb_levels)-3) */
  /*  exit(-1); */
  a = (*arity)[n];
  if( (a%3 == 0) && (a > 3) ){
    /*
    check if the a rity of level n devides 3
    If this is the case:
    Add a level
    */
    (*nb_levels)++;
    /* Build a new arity array  */
    new_arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
    /*  Copy the begining if the old array */
    for( i = 0 ; i < n ; i++)
      new_arity[i] = (*arity)[i];
    /* set the nth level to arity 3  */
    new_arity[n] = 3;
    /* printf("a=%d\n",a); */
    /* Set the (n+1) level to arity a/3 */
    new_arity[n+1] = a/3;
    /* Copy the end of the array */
    for( i = n+2 ; i < *nb_levels ; i++)
      new_arity[i] = (*arity)[i-1];
    FREE(*arity);
    /* if a/3 =3 then go to the next level */
    if(new_arity[n+1] == 3)
      optimize_arity(&new_arity,nb_levels,n);
    else /* continue to this level (remember we just add a new level */
      optimize_arity(&new_arity,nb_levels,n+1);
    *arity=new_arity;
  }else if( (a%2==0) && (a>2) ){/* same as above but for arity == 2 instead of 3 */
    (*nb_levels)++;
    new_arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
    for( i = 0 ; i < n ; i++ )
      new_arity[i] = (*arity)[i];
    new_arity[n] = 2;
    /* printf("a=%d\n",a); */
    new_arity[n+1] = a/2;
    for( i = n+2 ; i < *nb_levels ; i++ )
      new_arity[i] = (*arity)[i-1];
    FREE(*arity);
    if(new_arity[n+1] == 2)
      optimize_arity(&new_arity,nb_levels,n);
    else
      optimize_arity(&new_arity,nb_levels,n+1);
    *arity = new_arity;
  }else /* if nothing works go to next level.  */
    optimize_arity(arity,nb_levels,n-1);
}



tm_topology_t *optimize_topology(tm_topology_t *topology){
  int *arity = NULL,nb_levels;
  int *numbering = NULL,nb_nodes;
  tm_topology_t *new_topo;

  topology_arity(topology,&arity,&nb_levels);
  /*   printf("nb_levels=%d\n",nb_levels); */
  /*   for(i=0;i<nb_levels;i++) */
  /*     printf("%d:",arity[i]); */
  /*   printf("\n");   */

  topology_numbering(topology,&numbering,&nb_nodes);
  /*   printf("nb_nodes=%d\n",nb_nodes); */
  /*   for(i=0;i<nb_nodes;i++) */
  /*     printf("%d,",numbering[i]); */
  /*   printf("\n"); */

  optimize_arity(&arity,&nb_levels,nb_levels-2);
  new_topo = build_synthetic_topology(arity,nb_levels,numbering,nb_nodes);
  /* display_topology(*topology); */
  FREE(arity);
  FREE(numbering);

  return new_topo;
  /*  exit(-1); */
}


/* compute the number of leaves of any subtree starting froma node of depth depth*/
int compute_nb_leaves_from_level(int depth,tm_topology_t *topology)
{
  int res = 1;

  while(depth < topology->nb_levels-1)
    res *= topology->arity[depth++];

  return res;
}



/* return the indice of the greatest element of tab slower than val
   tab needs to be sorted in increasing order*/
int get_indice(int *tab, int n, int val)
{
  int i = 0, j = n-1, k;

  if( tab[n-1] < val )
    return n-1;

  while( i != j){
    k = (i+j)/2;
    if( (tab[k]<val) && (k!=i) )
      i = k;
    else
      j = k;
  }
  if(tab[i] == val)
    i--;
  return i;
}

/*
   copy element of tab in *new_tab from start to end and shift negativeley them
   allocates *new_tab
*/
int  fill_tab(int **new_tab,int *tab, int n, int start, int max_val, int shift)
{
  int *res = NULL,i,j,end;

  if(!n){
    *new_tab = NULL;
    return 0;
  }
  end = start;

  /* find how many cell to copy*/
  while( end < n ){
    if(tab[end] >= max_val)
      break;
    end++;
  }

  /* if none return */
  if( start == end ){
    *new_tab = NULL;
    return end;
  }

  /* allocate the result*/
  res = (int*) MALLOC (sizeof(int)*(end-start));

  /* copy and shift*/
  j = 0;
  for( i = start ; i < end ; i++ ){
    res[j] = tab[i] - shift;
    j++;
  }

  /* set the pointer passed in parameter and return */
  *new_tab = res;
  return end;
}


/*
   update the table of canonical values by adding the shift from start to end
   This is required because value in canonical are initialized by 0
   When we know which part of this table belong to which  subtree we can update the values
*/
void update_canonical(int *canonical,int start, int end, int shift)
{
  int i;
  for( i = start ;  i < end ; i++ )
    canonical[i] += shift;
}



/* function to sort constraint_t* tab using qsort*/
int constraint_dsc(const void* x1,const void* x2)
{
  constraint_t *e1 = NULL,*e2 = NULL;

  e1 = ((constraint_t*)x1);
  e2 = ((constraint_t*)x2);

  return (e1->length > e2->length) ? -1 : 1;
}


/* display function*/
void  display_contsraint_tab(constraint_t *const_tab, int n)
{
  int i;
  for( i = 0; i < n; i++ )
    printf("tab %d:",i);print_1D_tab(const_tab[i].constraints, const_tab[i].length);
}


/*
   We shift perm in new_perm  and then copy back
   perm is decomposed in m part of size 'size'

   in part k of new_perm we copy part constratint[k].id
*/

void update_perm(int *perm, int n, constraint_t *const_tab, int m, int size)
{
  int k;
  int *new_perm = NULL;

  if( n <= 1 )
    return;

  new_perm = (int*)MALLOC(sizeof(int)*n);

  for ( k = 0 ; k < m ; k++ )
    memcpy(new_perm+k*size,perm+const_tab[k].id*size,size*sizeof(int));

  memcpy(perm,new_perm,n*sizeof(int));
  /*printf("perm:");print_1D_tab(perm,n);*/

  FREE(new_perm);
}



/* we are at a given subtree of depth depth of the topology
   the mapping constraints are in the table constraints of size n
   The value of constraints are between 0 and the number of leaves-1 of the current subtree

   Canonical is the output of the function and is a just a renumbering of constraints in the canonical way
   perm is a way to go from canonical[i] to the corresponding  constraints[k]: perm[canonical[i]]=constraints[k]
*/

void recursive_canonicalization(int depth, tm_topology_t *topology, int *constraints, int *canonical, int *perm, int n, int m)
{
  constraint_t *const_tab = NULL;
  int nb_leaves,nb_subtrees;
  int k, prec, start, end;

  /* if there is no constraints stop and return*/
  if( !constraints ){
    assert( n == 0 );
    return;
  }

  /* if we are at teh bottom of the tree set canonical to the 0 value: it will be shifted by update_canonical
   and return*/
  if ( depth == topology->nb_levels ){
      assert( n==1 );
      canonical[0] = 0;
      return;
  }

  /* compute in how many subtrees we need to devide the curret one*/
  nb_subtrees = topology->arity[depth];
  /* construct a tab of constraints of this size*/
  const_tab = (constraint_t *) MALLOC( nb_subtrees * sizeof(constraint_t) );

  /*printf("tab (%d):",nb_subtrees,n);print_1D_tab(constraints,n);*/
  /* nb_leaves is the number of leaves of the current subtree
     this will help to detremine where to split constraints and how to shift values
   */
  nb_leaves = compute_nb_leaves_from_level( depth + 1, topology );

  /* split the constraints into nb_subtrees sub-constraints
     each sub-contraints k contains constraints of value in [k*nb_leaves,(k+1)*nb_leaves[
   */
  start = 0;
  for(k = 0; k < nb_subtrees; k++){
    /*returns the indice in contsraints that contains the smallest value not copied
      end is used to compute the number of copied elements (end-size) and is used as the next staring indices*/
    end=fill_tab(&(const_tab[k].constraints), constraints, n,start, (k+1) * nb_leaves, k * nb_leaves);
    const_tab[k].length = end-start;
    const_tab[k].id = k;
    start = end;
  }

  /* sort constraint tab such that subtrees with the largest number of
     constraints are put on the left and managed first, this how we canonize subtrees*/
  qsort(const_tab, nb_subtrees, sizeof(constraint_t), constraint_dsc);
  /*display_contsraint_tab(const_tab,nb_subtrees);*/

  /* update perm such taht we can backtrack the changes between constraints and caononical
   To go from canonical[i] to the corresponding  constraints[k] perm is such that perm[canonical[i]]=constraints[k]*/
  update_perm(perm, m, const_tab, nb_subtrees, nb_leaves);

  /* recursively call each subtree*/
  prec = 0;
  for(k = 0; k < nb_subtrees; k++){
    /* the tricky part is here : we send only a subtab of canonical that will be updated recursively
       This will greatly simplify the merging*/
    recursive_canonicalization(depth+1, topology, const_tab[k].constraints, canonical+prec, perm+k*nb_leaves,
			       const_tab[k].length, nb_leaves);
    prec += const_tab[k].length;
  }

  /*  merging consist only in shifting the right part of canonical*/
  start = const_tab[0].length;
  for( k = 1; k < nb_subtrees ; k++){
    update_canonical(canonical, start, start+const_tab[k].length, k * nb_leaves);
    start += const_tab[k].length;
  }

  /* FREE local subconstraints*/
  for( k = 0; k < nb_subtrees; k++ )
    if(const_tab[k].length)
      FREE(const_tab[k].constraints);

  FREE(const_tab);
}

/*
  shuffle the constraints such that for each node there are more constraints on the left subtree than on the right subtree

  This is required to avoid handling permutations. On a 2:2:2:2 tree, if the
  contraints are (0,1,3), it is equivalent to (0,1,2) The canonical form is the
  second one. This help to handle the case (0,6,7,9,11,13,14,15) which are
  symetric constaints and for which the canonical form is (0,1,2,4,6,8,9,12))



  We store in *perm the way to go from the canonical form to the original constraints.
  perm is a way to go from canonical[i] to the corresponding  constraints[k]: perm[canonical[i]]=constraints[k]
 */
void canonize_constraints(tm_topology_t *topology, int *constraints, int **canonical, int n, int **perm, int *m)
{
  int *p = NULL, *c = NULL;
  int i;
  unsigned int vl = get_verbose_level();

  *m = compute_nb_leaves_from_level(0,topology);

  p = (int*) MALLOC(sizeof(int)*(*m));
  for( i = 0 ; i < *m ; i++ )
    p[i] = i;

  c = (int*) MALLOC(sizeof(int)*n);

  if(vl>=DEBUG){
    printf("constraints:");
    print_1D_tab(constraints, n);
  }

  recursive_canonicalization(0, topology, constraints, c, p, n, *m);

  if(vl>=DEBUG){
    printf("canonical:");
    print_1D_tab(c, n);
    printf("perm:");
    print_1D_tab(p, *m);
  }

  *perm = p;
  *canonical = c;
}
