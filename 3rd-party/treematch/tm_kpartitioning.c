#include "tm_mapping.h"
#include "tm_mt.h"
#include "tm_kpartitioning.h"
#include "k-partitioning.h"
#include <stdlib.h>
#include <stdio.h>
#include "config.h"
#include "ompi_config.h"

#if defined(HAVE_LIBSCOTCH)
#include <scotch.h>
#endif  /* defined(HAVE_LIBSCOTCH) */


#define USE_KL_KPART 0
#define KL_KPART_GREEDY_TRIALS 0

static int verbose_level = ERROR;

#define MAX_TRIALS 10
#define USE_KL_STRATEGY 1


#define TM_MIN(a,b) ((a)<(b)?(a):(b))


OMPI_HIDDEN int  tm_fill_tab(int **,int *,int,int,int,int);
OMPI_HIDDEN void tm_complete_obj_weight(double **,int,int);

static void allocate_vertex(int,int *,com_mat_t *,int,int *,int);
static double eval_cost(int *, com_mat_t *);
static int *kpartition_greedy(int, com_mat_t *,int,int *,int);
static constraint_t *split_constraints (int *,int,int,tm_topology_t *,int, int);
static com_mat_t **split_com_mat(com_mat_t *,int,int,int *);
static int **split_vertices(int *,int,int,int *);
static void free_tab_com_mat(com_mat_t **,int);
static void free_tab_local_vertices(int **,int);
static void free_const_tab(constraint_t *,int);
static void kpartition_build_level_topology(tm_tree_t *,com_mat_t *,int,int,tm_topology_t *,
				     int *,int *,int,double *,double *);

static int greedy_flag = 0;

void tm_set_greedy_flag(int new_val){
  greedy_flag = new_val;
}

int tm_get_greedy_flag(){
  return greedy_flag;
}


#if defined(HAVE_LIBSCOTCH)

SCOTCH_Graph* com_mat_to_scotch_graph(com_mat_t *com_mat, int n){
  double **mat = com_mat->comm;
  SCOTCH_Num vertnbr = n;	// number of vertices
  SCOTCH_Num edgenbr = vertnbr*vertnbr;			// number of edges
  /* adjacency list */
  SCOTCH_Num *verttab = (SCOTCH_Num *)malloc(sizeof(SCOTCH_Num) * (vertnbr+1));
  /* loads of vertices */
  /* SCOTCH_Num *velotab = (SCOTCH_Num *)malloc(sizeof(SCOTCH_Num) * vertnbr); */
  /* id of the neighbors */
  SCOTCH_Num *edgetab = (SCOTCH_Num *)malloc(sizeof(SCOTCH_Num) * edgenbr);
  /* number of bytes exchanged */
  SCOTCH_Num *edlotab = (SCOTCH_Num *)malloc(sizeof(SCOTCH_Num) * edgenbr);
  SCOTCH_Graph *graphptr =  SCOTCH_graphAlloc();

  int edgeNum = 0;
  int i,j;

  /* Building with the communication matrix */
  for(i = 0; i < com_mat->n ; i++) {
    verttab[i] = edgeNum;
    for(j = 0; j < i; j++) {
      if(mat[i][j]){
	edgetab[edgeNum] = j;
	edlotab[edgeNum] = (SCOTCH_Num)mat[i][j];
	edgeNum++;
      }
    }
    /* ensure i!=j. Hence, avoid to test it...*/
    for(j = i+1 ; j < com_mat->n ; j++) {
      if(mat[i][j]){
	edgetab[edgeNum] = j;
	edlotab[edgeNum] = (SCOTCH_Num)mat[i][j];
	edgeNum++;
      }
    }
  }
  

  /* for(i = baseval; i < com_mat->n ; i++) { */
  /*   verttab[i] = edgeNum; */
  /*   /\* velotab[i] = (SCOTCH_Num) ceil(ogr->vertices[i].getVertexLoad() * ratio); *\/ */
  /*   for(j = baseval; j < com_mat->n ; j++) { */
  /*     if((mat[i][j] || mat[j][i]) && (i!=j)){ */
  /* 	edgetab[edgeNum] = j; */
  /* 	edlotab[edgeNum] = (SCOTCH_Num) ((mat[i][j] + mat[j][i])/2); */
  /* 	edgeNum++; */
  /*     } */
  /*   } */
  /* } */

  /* adding the dumb vertices: they have no neighbor*/
  for(i = com_mat->n ; i<vertnbr ; i++) {
    verttab[i] = edgeNum;
  }

  verttab[i] = edgeNum;

  if(tm_get_verbose_level() >=DEBUG){
    printf("Graph converted to Scotch format: edgeNum=%d, edgenbr = %lld, vertnbr = %lld\n",edgeNum, (long long int)edgenbr, (long long int)vertnbr);
  }

  assert(edgeNum <= edgenbr);
  edgenbr = edgeNum;

  SCOTCH_graphInit(graphptr);
  SCOTCH_graphBuild(graphptr, 0, vertnbr, verttab, verttab+1, NULL, NULL, edgenbr, edgetab, edlotab); 

  return graphptr;
}



int  check_partition(SCOTCH_Num *parttab, int k, int n){
  int *count = CALLOC(sizeof(int), k);
  int i;
  for(i=0; i<n; i++){
    count[parttab[i]]++;
  }

  int target= n/k;

  for(i = 0; i<k ; i++){
    if(count[i] != target){
      if(tm_get_verbose_level()>=INFO)
	fprintf(stdout, "Error in partition: %d vertices in partition %d while expecting %d vertices\n",count[i], i, target);
      FREE(count);
      return 0;
    }
  }

   FREE(count);
  return 1;
}


/* n is the number of element in teh graoh with dumlb_vertices
   comm_mat->n is the nulber of processes (i.e. the size of teh graph without dumb veritcies*/
int  *kpartition_scotch(int k, com_mat_t *com_mat, int n, int *constraints, int nb_constraints){
  SCOTCH_Num    partnbr = (SCOTCH_Num) k;
  SCOTCH_Graph* graphptr;
  SCOTCH_Strat  strat;
  SCOTCH_Num    straval;
  SCOTCH_Num   *parttab = (SCOTCH_Num *)MALLOC(sizeof(SCOTCH_Num) * n);
  int          *partition = (int *)MALLOC(sizeof(int) * n);
  int          i, j;
  int          *nb_dumb = (int *)MALLOC(sizeof(int) * k); /*number of dumb vertices per partition */ 
  int          dumb_id, min_nb_dumb = n, sum_dumb = 0, p;
  /* if(SCOTCH_graphCheck(graphptr) == 1){ */
  /*   fprintf(stderr,"Bad scotch graph! Exiting program...\n"); */
  /*   exit(-1); */
  /* } */

  /* printf("Correct scotch graph (%d, %d)!\n", SCOTCH_numSizeof(), sizeof(SCOTCH_Num)); */

  for(i=0;i<n;i++)
    parttab[i] = -1;


  /* put "dumb" vertices in the correct partition if there are any*/
  /*constraints are leaves that can be used */
  if (nb_constraints){
    int end, start = 0;
    for( i = 0 ; i < k ; i ++){
      int max_val = (i+1)* (n/k);
      end = start;
      while( end < nb_constraints){
	if(constraints[end] >= max_val)
	  break;
	end++;
      }
      /* now end - start is the number of constraints for the ith subtree
	 hence the number of dumb vertices in partition i is the differences between the
	 number of leaves of the subtree (n/k) and the number of constraints
      */
      nb_dumb[i] = n/k - (end-start);
      sum_dumb += nb_dumb[i];
      if(nb_dumb[i] < min_nb_dumb){
	min_nb_dumb = nb_dumb[i];
     }
       start=end;
    }

    /* Imagine we have n=12, k=3, nb_dumb[0] = 3, nb_dumb[1] = 2, nb_dumb[2] = 3, hence min_nb_dumb = 2 and sum_dumb = 8 
       So, we have 8 fix vertices and 12-8 = 4 free vertices
       We want scotch to allocate the 6 free vertices such that the whole partition is balanced (4 vertex in each) : 
       1 in parttion 0,   2 in partition 1 and 1 in partition 2.
       To do so we can fill partab as follows: 
       {-1, -1, -1, -1, 0, 0, 0, 1, 1, 2, 2, 2} and call scotch with a n=12 vertices graph with SCOTCH_STRATBALANCE
       dumb_id = n - sum_dumb;
       for(i = 0;i<k;i++){
         for( j = 0; j < nb_dumb[i]; j ++ ){
           parttab[dumb_id] = i;
           dumb_id++;
         } 
       }
       
       A more efficient solution is to fill partab as follows
       {-1, -1, -1, -1, 0, 2, 0, 0, 1, 1, 2, 2} and call Scotch with 
       a p = 6 (n-sum_dumb+ sum_{i}(nb_dumb[i]-min_dumb) vertices graph. 
       Scotch will then only use the 8 fist element of partab
    */

    dumb_id = n - sum_dumb; /* now dumb_id is the number of free vertices*/
    for(i = 0 ; i < k ; i++){
      for( j = 0; j < nb_dumb[i] - min_nb_dumb; j ++ ){
	parttab[dumb_id] = i;
	dumb_id++;
      } 
    }
    p = dumb_id; 
    for(i = 0 ; i < k ; i++){
      for( j = 0 ; j < min_nb_dumb ; j ++ ){
	parttab[dumb_id] = i;
	dumb_id++;
      } 
    }
  }else{
    p=n; /* if no constraint use n vertices */
  }       


  graphptr = com_mat_to_scotch_graph(com_mat, p);
  
  SCOTCH_stratInit (&strat);
  straval = SCOTCH_STRATBALANCE;
  if(k>4)
    straval = SCOTCH_STRATSPEED;
  SCOTCH_stratGraphMapBuild (&strat, straval, partnbr, 0);


  if(tm_get_verbose_level()>=DEBUG){
    printf("Before Scotch (p=%d, n=%d): \n", p, n);
    for(i = 0 ; i < n; i++){
      printf("%d ",(int)parttab[i]);
    }
    printf("\n");
  }

  if(SCOTCH_graphPartFixed(graphptr, partnbr, &strat, parttab) == 0){
    if(tm_get_verbose_level()>=DEBUG){
      printf("After Scotch: \n");
      for(i = 0 ; i < n; i++){
	printf("%d ",(int)parttab[i]);
      }
      printf("\n");
    }
  }else{
    if(tm_get_verbose_level()>=CRITICAL){
      fprintf(stderr,"Scotch Partitionning failed\n");  
    }
    exit(-1);
  }

  if(!check_partition(parttab, partnbr, n)){
    if(tm_get_verbose_level()>=INFO){
      printf("falling from Scotch to greedy partionning\n");
    }
    FREE(partition);
    partition = kpartition_greedy(k, com_mat, n, constraints, nb_constraints);
  }else{
    for(i=0;i<n;i++)
      partition[i] = parttab [i];
  }

  SCOTCH_stratExit (&strat);
  SCOTCH_graphExit(graphptr);
  SCOTCH_memFree(graphptr);
  FREE(parttab);
  FREE(nb_dumb);
  
  return partition;
}

#endif /* defined(HAVE_LIBSCOTCH) */


static void allocate_vertex(int u, int *res, com_mat_t *com_mat, int n, int *size, int max_size)
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
	/* if((n<=16) && (u==8)){ */
	/*   printf("u=%d, i=%d: %f\n",u, i, cost); */
	/* } */
	if (( cost > best_cost)){
	  best_cost = cost;
	  best_part = res[i];
	}
      }
    }
  }
  /* if(n<=16){ */
  /*   printf("size[%d]: %d\n",best_part, size[best_part]); */
  /*   printf("putting(%.2f): %d -> %d\n",best_cost, u, best_part); */
  /* } */

  res[u] = best_part;
  size[best_part]++;
}

static double eval_cost(int *partition, com_mat_t *com_mat)
{
  double cost = 0;
  int i,j;

  for( i = 0 ; i < com_mat->n ; i++ )
    for( j = i+1 ; j < com_mat->n ; j++ )
      if(partition[i] != partition[j])
	cost += com_mat->comm[i][j];

  return cost;
}

static int  *kpartition_greedy(int k, com_mat_t *com_mat, int n, int *constraints, int nb_constraints)
{
  int *partition = NULL, *best_partition=NULL, *size = NULL;
  int i,j,nb_trials;
  int max_size, max_val;
  double cost, best_cost = -1;
  int start, end;
  int dumb_id, nb_dumb;
  int vl = tm_get_verbose_level();


  if(nb_constraints > n){
    if(vl >= ERROR){
      fprintf(stderr,"Error more constraints (%d) than the problem size (%d)!\n",nb_constraints, n);
    }
    return NULL;
  }

  max_size = n/k;

  if(vl >= DEBUG){
    printf("max_size = %d (n=%d,k=%d)\ncom_mat->n-1=%d\n",max_size,n,k,com_mat->n-1);
    printf("nb_constraints = %d\n",nb_constraints);

    if(n<=16){
      printf("Constraints: ");
      tm_print_1D_tab(constraints,nb_constraints);
    }
  }
  /* if(com_mat->n){ */
  /*   printf ("val [n-1][0]= %f\n",com_mat->comm[com_mat->n-1][0]); */
  /* } */


  for( nb_trials = 0 ; nb_trials < MAX_TRIALS ; nb_trials++ ){
    partition = (int *)MALLOC(sizeof(int)*n);
    for ( i = 0 ; i < n ; i ++ )
      partition[i] = -1;

    size = (int *)CALLOC(k,sizeof(int));



    /* put "dumb" vertices in the correct partition if there are any*/
    /*constraints are leaves that can be used */
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
	/* now end - start is the number of constraints for the ith subtree
	   hence the number of dumb vertices is the differences between the
	   number of leaves of the subtree (n/k) and the number of constraints
	*/
	nb_dumb = n/k - (end-start);
	/* if(n<=16){ */
	/*   printf("max_val: %d, nb_dumb=%d, start=%d, end=%d, size=%d\n",max_val, nb_dumb, start, end, n/k); */
	/* } */
	/* dumb vertices are the one with highest indices:
	   put them in the ith partitions*/
	for( j = 0; j < nb_dumb; j ++ ){
	  partition[dumb_id] = i;
	  dumb_id--;
	}
	/* increase the size of the ith partition accordingly*/
	size[i] += nb_dumb;
	start=end;
      }
    }
    /* if(n<=16){ */
    /*   printf("After dumb vertices mapping: ");print_1D_tab(partition,n); */
    /* } */


    /* choose k initial "true" vertices at random and put them in a different partition */
    for ( i = 0 ; i < k ; i ++ ){
      /* if the partition is full of dumb vertices go to next partition*/
      if(size[i] >= max_size)
	continue;
      /* find a vertex not allready partitionned*/
      do{
	/* call the mersenne twister PRNG of tm_mt.c*/
	j =  tm_genrand_int32() % n;
      } while ( partition[j] != -1 );
      /* allocate and update size of partition*/
      partition[j] = i;
      /* if(n<=16){ */
      /* 	printf("random: %d -> %d\n",j,i); */
      /* } */
      size[i]++;
    }

    /* allocate each unaloacted vertices in the partition that maximize the communication*/
    for( i = 0 ;  i < n ; i ++)
      if( partition[i] == -1)
	allocate_vertex(i, partition, com_mat, n, size, max_size);

    cost = eval_cost(partition,com_mat);
    /* if(n<=16){ */
    /*   print_1D_tab(partition,n); */
    /*   printf("cost=%.2f\n",cost); */
    /* } */
    if((cost<best_cost) || (best_cost == -1)){
      best_cost=cost;
      FREE(best_partition);
      best_partition=partition;
    }else
      FREE(partition);

    FREE(size);
  }

  /*print_1D_tab(best_partition,n);
  printf("best_cost=%.2f\n",best_cost);
  */
  return best_partition;
}

int *tm_kpartition(int k, com_mat_t *com_mat, int n, int *constraints, int nb_constraints)
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


#if defined(HAVE_LIBSCOTCH)
  if(!greedy_flag){
    if(verbose_level >= DEBUG)
      printf("Using Scotch\n");
    res = kpartition_scotch(k, com_mat, n, constraints, nb_constraints);
  }else{
    if(verbose_level >= DEBUG)
      printf("Using greedy partitionning\n");
    res = kpartition_greedy(k, com_mat, n, constraints, nb_constraints);
  }
#else  /* defined(HAVE_LIBSCOTCH) */
  if(verbose_level >= DEBUG)
    printf("Using greedy partitionning\n");
  res = kpartition_greedy(k, com_mat, n, constraints, nb_constraints);
#endif  /* defined(HAVE_LIBSCOTCH) */
  return res;
}

static constraint_t *split_constraints (int *constraints, int nb_constraints, int k, tm_topology_t *topology, int depth, int N)
{
  constraint_t *const_tab = NULL;
  int nb_leaves, start, end;
  int i;
  int vl = tm_get_verbose_level();

  const_tab = (constraint_t *)CALLOC(k,sizeof(constraint_t));

  /* nb_leaves is the number of leaves of the current subtree
     this will help to determine where to split constraints and how to shift values
  */
  nb_leaves = tm_compute_nb_leaves_from_level( depth + 1, topology );

/* split the constraints into k sub-constraints
     each sub-contraints 'i' contains constraints of value in [i*nb_leaves,(i+1)*nb_leaves[
   */
  start = 0;

  for( i = 0; i < k; i++ ){
    /*returns the indice in constraints that contains the smallest value not copied
      end is used to compute the number of copied elements (end-size) and is used as the next staring indices*/
    end = tm_fill_tab(&(const_tab[i].constraints), constraints, nb_constraints,start, (i+1) * nb_leaves, i * nb_leaves);
    const_tab[i].length = end-start;
    if(vl>=DEBUG){
      printf("Step %d\n",i);
      printf("\tConstraint: "); tm_print_1D_tab(constraints, nb_constraints);
      printf("\tSub constraint: "); tm_print_1D_tab(const_tab[i].constraints, end-start);
    }

    if(end-start > N/k){
      if(vl >= ERROR){
	fprintf(stderr, "Error in spliting constraint at step %d. N=%d k= %d, length = %d\n", i, N, k, end-start);
      }
      FREE(const_tab);
      return NULL;
    }
    const_tab[i].id = i;
    start = end;
  }

  return const_tab;
}


/* split the com_mat of order n in k partiton according to parmutition table*/
static com_mat_t **split_com_mat(com_mat_t *com_mat, int n, int k, int *partition)
{
  com_mat_t **res = NULL, *sub_com_mat;
  double **sub_mat = NULL;
  int *perm = NULL;
  int cur_part, i, ii, j, jj, m = n/k, s;

  res = (com_mat_t**)MALLOC(k*sizeof(com_mat_t *));


  if(verbose_level >= DEBUG){
    printf("Partition: "); tm_print_1D_tab(partition,n);
    tm_display_tab(com_mat->comm,com_mat->n);
    printf("m=%d,n=%d,k=%d\n",m,n,k);
    printf("perm=%p\n", (void *)perm);
  }

  perm  = (int*)MALLOC(sizeof(int)*m);
  for( cur_part = 0 ; cur_part < k ; cur_part ++ ){

    /* build perm such that submat[i][j] correspond to com_mat[perm[i]][perm[j]] according to the partition*/
    s = 0;
    /* The partition is of size n. n can be larger than the communication matrix order
       as only the input problem are in the communication matrix while n is of the size
       of all the element (including the added one where it is possible to map computation) :
       we can have more compute units than processes*/
    for( j = 0; j < com_mat->n; j ++)
      if ( partition[j] == cur_part )
	perm[s++] = j;

    if(s>m){
      if(verbose_level >= CRITICAL){
	fprintf(stderr,"Partition: "); tm_print_1D_tab(partition,n);
	tm_display_tab(com_mat->comm,com_mat->n);
	fprintf(stderr,"too many elements of the partition for the permuation (s=%d>%d=m). n=%d, k=%d, cur_part= %d\n",s,m,n,k, cur_part);
      }
      exit(-1);
    }
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

    sub_com_mat = (com_mat_t *)MALLOC(sizeof(com_mat_t));
    sub_com_mat -> n = s;
    sub_com_mat -> comm = sub_mat;


    /*  printf("\n\npartition:%d\n",cur_part);display_tab(sub_mat,m);*/

    /* assign the sub_mat to the result*/
    res[cur_part] = sub_com_mat;
  }

 FREE(perm);

  return res;
}

static int **split_vertices( int *vertices, int n, int k, int *partition)
{
  int **res = NULL, *sub_vertices = NULL;
  int m = n/k;
  int i, j, cur_part;

  /*allocate resuts*/
  res = (int**) MALLOC(sizeof(int*) * k);


  if(verbose_level >= DEBUG){
    printf("Partition: ");tm_print_1D_tab(partition,n);
    printf("Vertices id: ");tm_print_1D_tab(vertices,n);
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
      printf("partition %d: ",cur_part);tm_print_1D_tab(sub_vertices,m);
    }
  }
  /*exit(-1);*/
  return res;
}

static void free_tab_com_mat(com_mat_t **mat,int k)
{
  int i,j;
  if( !mat )
    return;

  for ( i = 0 ; i < k ; i ++){
    for ( j = 0 ; j < mat[i]->n ; j ++)
      FREE( mat[i]->comm[j] );
    FREE( mat[i]->comm );
    FREE(mat[i]);

  }
  FREE(mat);
}

static void free_tab_local_vertices(int **mat, int k)
{
  int i; /* m=n/k; */
  if( !mat )
    return;

  for ( i = 0 ; i < k ; i ++){
    FREE( mat[i] );
  }
  FREE(mat);
}


static void free_const_tab(constraint_t *const_tab, int k)
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

#if 0
static void check_com_mat(com_mat_t *com_mat){
  int i,j;

  for( i = 0 ; i < com_mat->n ; i++ )
    for( j = 0 ; j < com_mat->n ; j++ )
      if(com_mat->comm[i][j]<0){
	printf("com_mat->comm[%d][%d]= %f\n",i,j,com_mat->comm[i][j]);
	exit(-1);
      }
}
#endif

static void print_tab(int n){
  for(;n;n--)
    fprintf(stdout,"\t");
}

static void display_partition(int *partition, int *local_vertices, int n, int depth, int k){
  int cur_part, j;
  print_tab(depth);fprintf(stdout,"Partitions at depth=%d\n",depth);
  for( cur_part = 0; cur_part < k ; cur_part ++){
    print_tab(depth); fprintf(stdout,"%d :",cur_part);
    for( j = 0; j < n; j ++){
      if ( partition[j] == cur_part ){
	if(local_vertices[j]!=-1)
	  fprintf(stdout,"%d ",local_vertices[j]);
      }
    }
    fprintf(stdout,"\n");
  }	
}

static void kpartition_build_level_topology(tm_tree_t *cur_node, com_mat_t *com_mat, int N, int depth,
				     tm_topology_t *topology, int *local_vertices,
				     int *constraints, int nb_constraints,
				     double *obj_weight, double *comm_speed)
{
  com_mat_t **tab_com_mat = NULL; /* table of comunication matrix. We will have k of such comunication matrix, one for each subtree */
  int k = topology->arity[depth];
  tm_tree_t **tab_child = NULL;
  int *partition = NULL;
  int **tab_local_vertices = NULL;
  constraint_t *const_tab = NULL;
  int i;
  verbose_level = tm_get_verbose_level();

  /* if we are at the bottom of the tree set cur_node
   and return*/
  if ( depth == topology->nb_levels - 1 ){
    if(verbose_level>=DEBUG)
      printf("id : %d, com_mat= %p\n",local_vertices[0], (void *)com_mat->comm);
    tm_set_node(cur_node,NULL, 0, NULL, local_vertices[0], 0, NULL, depth);
    return;
  }


  if(verbose_level >= DEBUG){
    printf("Partitionning Matrix of size %d (problem size= %d) in %d partitions\n", com_mat->n, N, k);
  }

  /* check_com_mat(com_mat); */

  /* partition the com_matrix in k partitions*/
  partition = tm_kpartition(k, com_mat, N, constraints, nb_constraints);

  if(verbose_level>=INFO)
    display_partition(partition, local_vertices, N, depth, k);

  /* exit(-1); */
  /* split the communication matrix in k parts according to the partition just found above */
  tab_com_mat = split_com_mat( com_mat, N, k, partition);

  /* split the local vertices in k parts according to the partition just found above */
  tab_local_vertices = split_vertices( local_vertices, N, k, partition);

  /* construct a tab of constraints of  size k: one for each partitions*/
  const_tab = split_constraints (constraints, nb_constraints, k, topology, depth, N);

  /* create the table of k nodes of the resulting sub-tree */
  tab_child = (tm_tree_t **) CALLOC (k,sizeof(tm_tree_t*));
  for( i = 0 ; i < k ; i++){
    tab_child[i] = (tm_tree_t *) MALLOC(sizeof(tm_tree_t));
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
  tm_set_node( cur_node, tab_child, k, NULL, cur_node->id, 0, NULL, depth);

  /* free local data*/
  FREE(partition);
  free_tab_com_mat(tab_com_mat,k);
  free_tab_local_vertices(tab_local_vertices,k);
  free_const_tab(const_tab,k);
}


tm_tree_t *tm_kpartition_build_tree_from_topology(tm_topology_t *topology,double **comm,int N, int *constraints, int nb_constraints, double *obj_weight, double *com_speed)
{
  int depth,i, K;
  tm_tree_t *root = NULL;
  int *local_vertices = NULL;
  int nb_cores;
  com_mat_t com_mat;

  verbose_level = tm_get_verbose_level();


  nb_cores=tm_nb_processing_units(topology)*topology->oversub_fact;


  if(verbose_level>=INFO)
    printf("Number of constraints: %d, N=%d, nb_cores = %d, K=%d\n", nb_constraints, N, nb_cores, nb_cores-N);

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
    tm_complete_obj_weight(&obj_weight,N,K);
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
     the value of this array will be used to number the leaves of the tm_tree_t tree
     that start at "root"

     min(N,nb_contraints) is used to tackle the case where there is less processes than constraints

   */

  local_vertices = (int*) MALLOC (sizeof(int) * (K+N));

  for( i = 0 ; i < TM_MIN(N,nb_constraints) ; i++)
    local_vertices[i] = i;
  for( i = TM_MIN(N,nb_constraints) ;i < N + K ; i++)
    local_vertices[i] = -1;

  /* we assume all objects have the same arity*/
  /* assign the root of the tree*/
  root = (tm_tree_t*) MALLOC (sizeof(tm_tree_t));
  root -> id = 0;


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


