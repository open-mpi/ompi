#include "tm_mapping.h"
#include "tm_mt.h"
#include "tm_kpartitioning.h"
#include "k-partitioning.h"
#include <stdlib.h>
#include <stdio.h>

#define USE_KL_KPART 0
#define KL_KPART_GREEDY_TRIALS 0

static int verbose_level = ERROR;

#define MAX_TRIALS 10
#define USE_KL_STRATEGY 1


#define MIN(a,b) ((a)<(b)?(a):(b))


int  fill_tab(int **,int *,int,int,int,int);
void complete_com_mat(double ***,int,int);
void complete_obj_weight(double **,int,int);

void allocate_vertex(int,int *,com_mat_t *,int,int *,int);
double eval_cost(int *, com_mat_t *);
int *kpartition_greedy(int, com_mat_t *,int,int *,int);
constraint_t *split_constraints (int *,int,int,tm_topology_t *,int);
com_mat_t **split_com_mat(com_mat_t *,int,int,int *);
int **split_vertices(int *,int,int,int *);
void FREE_tab_com_mat(com_mat_t **,int);
void FREE_tab_local_vertices(int **,int);
void FREE_const_tab(constraint_t *,int);
void kpartition_build_level_topology(tree_t *,com_mat_t *,int,int,tm_topology_t *,
				     int *,int *,int,double *,double *);



void allocate_vertex(int u, int *res, com_mat_t *com_mat, int n, int *size, int max_size)
{
  int i,best_part=0;
  double cost, best_cost = -1;

  /*printf("\n");
    print_1D_tab(res,n);*/
  if(u>=com_mat->n){
    for( i = 0 ; i < n ; i++)
      if (( res[i] != -1 ) && ( size[res[i]] < max_size )){
	best_part = res[i];
	break;
      }
  }else{
    for( i = 0 ; i < n ; i++){
      if (( res[i] != -1 ) && ( size[res[i]] < max_size )){
	cost = (((i)<com_mat->n)) ?com_mat->comm[u][i]:0;
	if (( cost > best_cost)){
	  best_cost = cost;
	  best_part = res[i];
	}
      }
    }
  }
  /*  printf("size[%d]: %d\n",best_part, size[best_part]);*/
  /* printf("putting(%.2f): %d -> %d\n",best_cost, u, best_part); */

  res[u] = best_part;
  size[best_part]++;
}

double eval_cost(int *partition, com_mat_t *com_mat)
{
  double cost = 0;
  int i,j;

  for( i = 0 ; i < com_mat->n ; i++ )
    for( j = i+1 ; j < com_mat->n ; j++ )
      if(partition[i] != partition[j])
	cost += com_mat->comm[i][j];

  return cost;
}

int  *kpartition_greedy(int k, com_mat_t *com_mat, int n, int *constraints, int nb_constraints)
{
  int *res = NULL, *best_res=NULL, *size = NULL;
  int i,j,nb_trials;
  int max_size, max_val;
  double cost, best_cost = -1;
  int start, end;
  int dumb_id, nb_dumb;




  for( nb_trials = 0 ; nb_trials < MAX_TRIALS ; nb_trials++ ){
    res = (int *)MALLOC(sizeof(int)*n);
    for ( i = 0 ; i < n ; i ++ )
      res[i] = -1;

    size = (int *)CALLOC(k,sizeof(int));
    max_size = n/k;

    /*printf("Constraints: ");print_1D_tab(constraints,nb_constraints);*/

    /* put "dumb" vertices in the correct partition if there are any*/
    if (nb_constraints){
      start = 0;
      dumb_id = n-1;
      for( i = 0 ; i < k ; i ++){
	max_val = (i+1)* (n/k);
	end = start;
	while( end < nb_constraints){
	  if(constraints[end] >= max_val)
	    break;
	  end++;
	}
	/* now end - start is the number of constarints for the ith subtree
	   hence the number of dumb vertices is the differences between the
	   number of leaves of the subtree (n/k) and the number of constraints
	*/
	nb_dumb = n/k - (end-start);
	/*printf("max_val: %d, nb_dumb=%d, start=%d, end=%d, size=%d\n",max_val, nb_dumb, start, end, n/k);*/

	/* dumb vertices are the one with highest indices:
	   put them in the ith partitions*/
	for( j = 0; j < nb_dumb; j ++ ){
	  res[dumb_id] = i;
	  dumb_id--;
	}
	/* increase the size of the ith partition accordingly*/
	size[i] += nb_dumb;
	start=end;
      }
    }
    /*printf("After dumb vertices mapping: ");print_1D_tab(res,n);*/

    /* choose k initial "true" vertices at random and put them in a different partition */
    for ( i = 0 ; i < k ; i ++ ){
      /* if the partition is full of dumb vertices go to next partition*/
      if(size[i] >= max_size)
	continue;
      /* find a vertex not allready partitionned*/
      do{
	/* call the mersenne twister PRNG of tm_mt.c*/
	j =  genrand_int32() % n;
      } while ( res[j] != -1 );
      /* allocate and update size of partition*/
      res[j] = i;
      /* printf("random: %d -> %d\n",j,i); */
      size[i]++;
    }

    /* allocate each unaloacted vertices in the partition that maximize the communication*/
    for( i = 0 ;  i < n ; i ++)
      if( res[i] == -1)
	allocate_vertex(i, res, com_mat, n, size, max_size);

    cost = eval_cost(res,com_mat);
    /*print_1D_tab(res,n);
    printf("cost=%.2f\n",cost);*/
    if((cost<best_cost) || (best_cost == -1)){
      best_cost=cost;
      FREE(best_res);
      best_res=res;
    }else
      FREE(res);

    FREE(size);
  }

  /*print_1D_tab(best_res,n);
  printf("best_cost=%.2f\n",best_cost);
  */
  return best_res;
}

int *kpartition(int k, com_mat_t *com_mat, int n, int *constraints, int nb_constraints)
{
  int *res= NULL;

  if( n%k != 0){
    if(verbose_level >= ERROR)
      fprintf(stderr,"Error: Cannot partition %d elements in %d parts\n",n,k);
    return NULL;
  }

  /* if(USE_KL_KPART) */
  /*   res = kPartitioning(comm, n, k, constraints, nb_constraints, KL_KPART_GREEDY_TRIALS); */
  /* else */
    res = kpartition_greedy(k, com_mat, n, constraints, nb_constraints);

  return res;
}

constraint_t *split_constraints (int *constraints, int nb_constraints, int k, tm_topology_t *topology, int depth)
{
  constraint_t *const_tab = NULL;
  int nb_leaves, start, end;
  int i;

  const_tab = (constraint_t *)CALLOC(k,sizeof(constraint_t));

  /* nb_leaves is the number of leaves of the current subtree
     this will help to detremine where to split constraints and how to shift values
  */
  nb_leaves = compute_nb_leaves_from_level( depth + 1, topology );

/* split the constraints into k sub-constraints
     each sub-contraints 'i' contains constraints of value in [i*nb_leaves,(i+1)*nb_leaves[
   */
  start = 0;
  for( i = 0; i < k; i++ ){
    /*returns the indice in contsraints that contains the smallest value not copied
      end is used to compute the number of copied elements (end-size) and is used as the next staring indices*/
    end = fill_tab(&(const_tab[i].constraints), constraints, nb_constraints,start, (i+1) * nb_leaves, i * nb_leaves);
    const_tab[i].length = end-start;
    const_tab[i].id = i;
    start = end;
  }

  return const_tab;
}


com_mat_t **split_com_mat(com_mat_t *com_mat, int n, int k, int *partition)
{
  com_mat_t **res = NULL, *sub_com_mat;
  double **sub_mat = NULL;
  int *perm = NULL;
  int cur_part, i, ii, j, jj, m = n/k, s;

  res = (com_mat_t**)MALLOC(k*sizeof(com_mat_t *));


  if(verbose_level >= DEBUG){
    printf("Partition: "); print_1D_tab(partition,n);
    display_tab(com_mat->comm,com_mat->n);
  }

  perm  = (int*)MALLOC(sizeof(int)*m);
  for( cur_part = 0 ; cur_part < k ; cur_part ++ ){

    /* build perm such that submat[i][j] correspond to com_mat[perm[i]][perm[j]] according to the partition*/
    s = 0;
    for( j = 0; j < com_mat->n; j ++) /* check only non zero element of of com_mat*/
      if ( partition[j] == cur_part )
	perm[s++] = j;

    /* s is now the size of the non zero sub matrix for this partition*/
    /* built a sub-matrix for partition cur_part*/
    sub_mat = (double **) MALLOC(sizeof(double *) * s);
    for( i = 0 ; i < s ; i++)
      sub_mat[i] = (double *) MALLOC(sizeof(double ) * s);

    /* build the sub_mat corresponding to the partiion cur_part*/
    for ( i = 0 ; i < s ; i ++){
      ii = perm[i];
      for( j = i ; j < s ; j ++){
	jj = perm[j];
	sub_mat[i][j] = com_mat->comm[ii][jj];
	sub_mat[j][i] = sub_mat[i][j];
      }
    }

    sub_com_mat = (com_mat_t *)malloc(sizeof(com_mat_t));
    sub_com_mat -> n = s;
    sub_com_mat -> comm = sub_mat;


    /*  printf("\n\npartition:%d\n",cur_part);display_tab(sub_mat,m);*/

    /* assign the sub_mat to the result*/
    res[cur_part] = sub_com_mat;
  }

  FREE(perm);

  return res;
}

int **split_vertices( int *vertices, int n, int k, int *partition)
{
  int **res = NULL, *sub_vertices = NULL;
  int m = n/k;
  int i, j, cur_part;

  /*allocate resuts*/
  res = (int**) MALLOC(sizeof(int*) * k);


  if(verbose_level >= DEBUG){
    printf("Partition: ");print_1D_tab(partition,n);
    printf("Vertices id: ");print_1D_tab(vertices,n);
  }

  /*split the vertices tab of the partition cur_part  to the sub_vertices tab*/
  for( cur_part = 0; cur_part < k ; cur_part ++){
    sub_vertices = (int*) MALLOC(sizeof(int) * m);
    i = 0;
    for( j = 0; j < n; j ++)
      if ( partition[j] == cur_part )
	sub_vertices[i++] = vertices[j];
    res[cur_part] = sub_vertices;
    if(verbose_level >= DEBUG){
      printf("partition %d: ",cur_part);print_1D_tab(sub_vertices,m);
    }
  }
  /*exit(-1);*/
  return res;
}

void FREE_tab_com_mat(com_mat_t **mat,int k)
{
  int i,j;
  if( !mat )
    return;

  for ( i = 0 ; i < k ; i ++){
    for ( j = 0 ; j < mat[i]->n ; j ++)
      FREE( mat[i]->comm[j] );
    FREE( mat[i]->comm );
  }
  FREE(mat);
}

void FREE_tab_local_vertices(int **mat, int k)
{
  int i; /* m=n/k; */
  if( !mat )
    return;

  for ( i = 0 ; i < k ; i ++){
    FREE( mat[i] );
  }
  FREE(mat);
}


void FREE_const_tab(constraint_t *const_tab, int k)
{
  int i;

  if( !const_tab )
    return;

  for(i = 0; i < k; i++){
    if(const_tab[i].length)
      FREE(const_tab[i].constraints);
  }

  FREE(const_tab);
}

void kpartition_build_level_topology(tree_t *cur_node, com_mat_t *com_mat, int N, int depth,
				     tm_topology_t *topology, int *local_vertices,
				     int *constraints, int nb_constraints,
				     double *obj_weight, double *comm_speed)
{
  com_mat_t **tab_com_mat = NULL; /* table of comunication matrix. We will have k of such comunication matrix, one for each subtree */
  int k = topology->arity[depth];
  tree_t **tab_child = NULL;
  int *partition = NULL;
  int **tab_local_vertices = NULL;
  constraint_t *const_tab = NULL;
  int i;
  verbose_level = get_verbose_level();

  /* if we are at the bottom of the tree set cur_node
   and return*/
  if ( depth == topology->nb_levels - 1 ){
    if(verbose_level>=DEBUG)
      printf("id : %d, com_mat= %p\n",local_vertices[0], (void *)com_mat->comm);
    set_node(cur_node,NULL, 0, NULL, local_vertices[0], 0, NULL, depth);
    return;
  }


  /* partition the com_matrix in k partitions*/
  partition = kpartition(topology->arity[depth], com_mat, N, constraints, nb_constraints);

  /* split the communication matrix in k parts according to the partition just found above */
  tab_com_mat = split_com_mat( com_mat, N, k, partition);

  /* split the local vertices in k parts according to the partition just found above */
  tab_local_vertices = split_vertices( local_vertices, N, k, partition);

  /* construct a tab of constraints of  size k: one for each partitions*/
  const_tab = split_constraints (constraints, nb_constraints, k, topology, depth);

  /* create the table of k nodes of the resulting sub-tree */
  tab_child = (tree_t **) CALLOC (k,sizeof(tree_t));
  for( i = 0 ; i < k ; i++){
    tab_child[i] = (tree_t *) MALLOC(sizeof(tree_t));
  }

  /* for each child, proceeed recursively*/
  for( i = 0 ; i < k ; i++){
    tab_child[i]->id = i;
    kpartition_build_level_topology ( tab_child[i], tab_com_mat[i], N/k, depth + 1,
				      topology, tab_local_vertices[i],
				      const_tab[i].constraints, const_tab[i].length,
				      obj_weight, comm_speed);
    tab_child[i]->parent = cur_node;
  }

  /* link the node with its child */
  set_node( cur_node, tab_child, k, NULL, cur_node->id, 0, NULL, depth);

  /* FREE local data*/
  FREE(partition);
  FREE_tab_com_mat(tab_com_mat,k);
  FREE_tab_local_vertices(tab_local_vertices,k);
  FREE_const_tab(const_tab,k);
}


tree_t *kpartition_build_tree_from_topology(tm_topology_t *topology,double **comm,int N, int *constraints, int nb_constraints, double *obj_weight, double *com_speed)
{
  int depth,i, K;
  tree_t *root = NULL;
  int *local_vertices = NULL;
  int nb_cores;
  com_mat_t com_mat;

  verbose_level = get_verbose_level();

  if(verbose_level>=INFO)
    printf("Number of constraints: %d\n", nb_constraints);
  printf("Number of constraints: %d, N=%d\n", nb_constraints, N);

  nb_cores=nb_processing_units(topology);

  if((constraints == NULL) && (nb_constraints != 0)){
    if(verbose_level>=ERROR)
      fprintf(stderr,"size of constraint table not zero while constraint tab is NULL\n");
    return NULL;
  }

  if((constraints != NULL) && (nb_constraints > nb_cores)){
    if(verbose_level>=ERROR)
      fprintf(stderr,"size of constraint table (%d) is greater than the number of cores (%d)\n", nb_constraints, nb_cores);
    return NULL;
  }

  depth = 0;

  /* if we have more cores than processes add new dumb process to the com matrix*/
  if((K=nb_cores - N)>0){
    /* add K element to the object weight*/
    complete_obj_weight(&obj_weight,N,K);
    /* display_tab(tab,N+K);*/
  } else if( K < 0){
    if(verbose_level>=ERROR)
      fprintf(stderr,"Not enough cores!\n");
    return NULL;
  }

  com_mat.comm = comm;
  com_mat.n    = N;

  /*
     local_vertices is the array of vertices that can be used
     the min(N,nb_contraints) 1st element are number from 0 to N
     the last ones have value -1
     the value of this array will be used to number the leaves of the tree_t tree
     that start at "root"

     min(N,nb_contraints) is used to takle the case where thre is less processes than constraints

   */

  local_vertices = (int*) MALLOC (sizeof(int) * (K+N));

  for( i = 0 ; i < MIN(N,nb_constraints) ; i++)
    local_vertices[i] = i;
  for( i = MIN(N,nb_constraints) ;i < N + K ; i++)
    local_vertices[i] = -1;

  /* we assume all objects have the same arity*/
  /* assign the root of the tree*/
  root = (tree_t*) MALLOC (sizeof(tree_t));



  /*build the tree downward from the root*/
  kpartition_build_level_topology(root, &com_mat, N+K,  depth, topology, local_vertices,
					constraints, nb_constraints, obj_weight, com_speed);

  /*print_1D_tab(local_vertices,K+N);*/
  if(verbose_level>=INFO)
    printf("Build (bottom-up) tree done!\n");



  FREE(local_vertices);


  /* tell the system it is a constraint tree, this is usefull for freeing pointers*/
  root->constraint = 1;

  return root;
}


