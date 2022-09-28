#include <hwloc.h>
#include <hwloc/helper.h>
#include "tm_tree.h"
#include "tm_mapping.h"
#include <ctype.h>
#include "tm_verbose.h"
#include "tm_solution.h"


tm_topology_t* tm_get_local_topo_with_hwloc(void);
static tm_topology_t* hwloc_to_tm(char *filename);
OMPI_HIDDEN int tm_int_cmp_inc(const void* x1,const void* x2);
static void optimize_arity(int **arity, double **cost, int *nb_levels,int n);
static int symetric(hwloc_topology_t topology);
static tm_topology_t * tgt_to_tm(char *filename);
OMPI_HIDDEN void tm_display_arity(tm_topology_t *topology);
OMPI_HIDDEN void tm_display_topology(tm_topology_t *topology);
OMPI_HIDDEN void tm_free_topology(tm_topology_t *topology);
OMPI_HIDDEN tm_topology_t *tm_load_topology(char *arch_filename, tm_file_type_t arch_file_type);
OMPI_HIDDEN void tm_optimize_topology(tm_topology_t **topology);
OMPI_HIDDEN int  tm_topology_add_binding_constraints(char *constraints_filename, tm_topology_t *topology);
static int topo_nb_proc(hwloc_topology_t topology,int N);
static void topology_arity_cpy(tm_topology_t *topology,int **arity,int *nb_levels);
static void topology_constraints_cpy(tm_topology_t *topology,int **constraints,int *nb_constraints);
static void topology_cost_cpy(tm_topology_t *topology,double **cost);
static void topology_numbering_cpy(tm_topology_t *topology,int **numbering,int *nb_nodes);
static double ** topology_to_arch(hwloc_topology_t topology);
static void   build_synthetic_proc_id(tm_topology_t *topology);
tm_topology_t  *tm_build_synthetic_topology(int *arity, double *cost, int nb_levels, int *core_numbering, int nb_core_per_nodes);
OMPI_HIDDEN void tm_set_numbering(tm_numbering_t new_val); /* TM_NUMBERING_LOGICAL or TM_NUMBERING_PHYSICAL */
void   build_synthetic_proc_id(tm_topology_t *topology);


#define LINE_SIZE (1000000)


static tm_numbering_t numbering = TM_NUMBERING_LOGICAL;

void            tm_set_numbering(tm_numbering_t new_val){
  numbering = new_val;
}

tm_numbering_t  tm_get_numbering(){
  return numbering;
}



/* transform a tgt scotch file into a topology file*/
static tm_topology_t * tgt_to_tm(char *filename)
{
  tm_topology_t *topology = NULL;
  FILE *pf = NULL;
  char line[1024];
  char *s = NULL;
  double *cost = NULL;
  int i;



  pf = fopen(filename,"r");
  if(!pf){
    if(tm_get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",filename);
    exit(-1);
  }

  if(tm_get_verbose_level() >= INFO)
    printf("Reading TGT file: %s\n",filename);


  if (NULL == fgets(line,1024,pf)) {
      /* either an error has occurred (and is in an unknown state) or
         we hit EOF and line is empty.  Either way, make line the
         empty string to avoid errors later */
      line[0] = '\0';
  }

  fclose(pf);

  s = strstr(line,"tleaf");
  if(!s){
    if(tm_get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Syntax error! %s is not a tleaf file\n",filename);
    exit(-1);
  }

  s += 5;
  while(isspace(*s))
    s++;

  topology                 = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  topology->nb_constraints = 0;
  topology->oversub_fact   = 1;
  topology->constraints    = NULL;
  topology->nb_levels      = atoi(strtok(s," "))+1;
  topology->arity          = (int*)MALLOC(sizeof(int)*topology->nb_levels);

  cost = (double*)CALLOC(topology->nb_levels,sizeof(double));

  for( i = 0 ; i < topology->nb_levels-1 ; i++ ){
    topology->arity[i] = atoi(strtok(NULL," "));
    cost[i] = atoi(strtok(NULL," "));
  }

  topology->arity[topology->nb_levels-1] = 0;
  /* cost[topology->nb_levels-1]=0; */

  /*aggregate costs*/
  for( i = topology->nb_levels-2 ; i >= 0 ; i-- )
    cost[i] += cost[i+1];

  build_synthetic_proc_id(topology);

  if(tm_get_verbose_level() >= INFO)
    printf("Topology built from %s!\n",filename);

  topology->cost=cost;


  return topology;
}



int tm_nb_processing_units(tm_topology_t *topology)
{
  return topology->nb_proc_units;
}

static inline int topo_nb_proc(hwloc_topology_t topology,int N)
{
  hwloc_obj_t *objs = NULL;
  int nb_proc;

  objs = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*N);
  objs[0] = hwloc_get_next_obj_by_type(topology,HWLOC_OBJ_PU,NULL);
  nb_proc = 1 + hwloc_get_closest_objs(topology,objs[0],objs+1,N-1);
  FREE(objs);
  return nb_proc;
}


static double link_cost(int depth)
{
  /*
    Bertha values
    double tab[5]={21,9,4.5,2.5,0.001};
    double tab[5]={1,1,1,1,1};
    double tab[6]={100000,10000,1000,500,100,10};
  */
  double tab[11] = {1024,512,256,128,64,32,16,8,4,2,1};

  return tab[depth];
  /*
   return 10*log(depth+2);
   return (depth+1);
   return (long int)pow(100,depth);
  */
}

static inline double ** topology_to_arch(hwloc_topology_t topology)
{
  int nb_proc,i,j;
  hwloc_obj_t obj_proc1,obj_proc2,obj_res;
  double **arch = NULL;

  nb_proc = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_PU);
  if (nb_proc < 0) {
    return NULL;
  }
  arch = (double**)malloc(sizeof(double*)*nb_proc);
  if (NULL == arch) {
    return NULL;
  }
  for( i = 0 ; i < nb_proc ; i++ ){
    obj_proc1 = hwloc_get_obj_by_type(topology,HWLOC_OBJ_PU,i);
    arch[obj_proc1->os_index] = (double*)MALLOC(sizeof(double)*nb_proc);
    for( j = 0 ; j < nb_proc ; j++ ){
      obj_proc2 = hwloc_get_obj_by_type(topology,HWLOC_OBJ_PU,j);
      obj_res = hwloc_get_common_ancestor_obj(topology,obj_proc1,obj_proc2);
      /* printf("arch[%d][%d] <- %ld\n",obj_proc1->os_index,obj_proc2->os_index,*((long int*)(obj_res->userdatab))); */
      arch[obj_proc1->os_index][obj_proc2->os_index]=link_cost(obj_res->depth+1);
    }
  }
  return arch;
}

static int symetric(hwloc_topology_t topology)
{
   int depth,i,topodepth = hwloc_topology_get_depth(topology);
   unsigned int arity;
   hwloc_obj_t obj;
   for ( depth = 0; depth < topodepth-1 ; depth++ ) {
    int N = hwloc_get_nbobjs_by_depth(topology, depth);
    obj = hwloc_get_next_obj_by_depth (topology,depth,NULL);
    arity = obj->arity;

    /* printf("Depth=%d, N=%d, Arity:%d\n",depth,N,arity); */
    for (i = 1; i < N; i++ ){
      obj = hwloc_get_next_obj_by_depth (topology,depth,obj);
      if( obj->arity != arity){
	/* printf("[%d]: obj->arity=%d, arity=%d\n",i,obj->arity,arity); */
	return 0;
      }
    }
   }
   return 1;
}

static void build_process_tab_id(tm_topology_t *topology,  hwloc_obj_t *objs, char* filename){
  unsigned int i,j;
  unsigned int nb_nodes = topology->nb_proc_units; 
  int vl = tm_get_verbose_level();
  
  /* Build process id tab */
  if(numbering == TM_NUMBERING_LOGICAL){
    for (i = 0; i < nb_nodes; i++){
      topology->node_id[i] = i;
      topology->node_rank[i] = i;
    }
  }else if(numbering == TM_NUMBERING_PHYSICAL){
    for (i = 0; i < nb_nodes; i++){
      if(objs[i]->os_index > nb_nodes){
	if(vl >= CRITICAL){
	  fprintf(stderr, "Cannot use forced physical numbering!\n\tIndex of PU %d is %d and larger than number of nodes : %d\n",
		  i, objs[i]->os_index, nb_nodes);
	}
	exit(-1);
      }
      for(j = 0; j < i; j++){
	if((unsigned int)topology->node_id[j] == objs[i]->os_index){
	  if(vl >= CRITICAL){
	    fprintf(stderr, "Cannot use forced physical numbering!\n\tDuplicated physical number of some PUs in %s.\n\tPU %d and PU %d have the same physical number: (os_index[%d] = %d) == (os_index[%d] = %d)\n", filename, j, i, j, objs[j]->os_index, i, objs[i]->os_index);
	  }
	  exit(-1);
	}
      }
      topology->node_id[i] = objs[i]->os_index;
      topology->node_rank[objs[i]->os_index] = i;
    }
  }else{
    if(vl >= CRITICAL){
      fprintf(stderr, "Unknown numbering %d\n", (int)numbering);
    }
    exit(-1);
  }
}


static tm_topology_t* hwloc_to_tm(char *filename)
{
  hwloc_topology_t topology;
  tm_topology_t *res = NULL;
  hwloc_obj_t *objs = NULL;
  unsigned topodepth,depth;
  unsigned int nb_nodes;
  double *cost;
  int err, l;
  int vl = tm_get_verbose_level();

  /* Build the topology */
  hwloc_topology_init(&topology);
  err = hwloc_topology_set_xml(topology, filename);
  if(err == -1){
    if(vl >= CRITICAL)
      fprintf(stderr,"Error: %s is a bad xml topology file!\n",filename);
    exit(-1);
  }

#if HWLOC_API_VERSION < 0x20000
  hwloc_topology_ignore_all_keep_structure(topology);
#else
  hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_STRUCTURE);
#endif

  err = hwloc_topology_load(topology);
  if(err == -1){
    if(vl >= CRITICAL)
      fprintf(stderr,"Error: the content of the xml topology file %s is not compatible with the version installed on this machine.\nPlease use compatible versions to generate the file and to use it!\n",filename);
    exit(-1);
  }


  /* Test if symetric */
  if(!symetric(topology)){
    if(vl >= CRITICAL)
      fprintf(stderr,"%s not symetric!\n",filename);
    exit(-1);
  }

  /* work on depth */
  topodepth = hwloc_topology_get_depth(topology);
  
  res                   = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  res->oversub_fact      = 1;
  res->nb_constraints   = 0;
  res->constraints      = NULL;
  res->nb_levels        = topodepth;
  res->nb_nodes         = (size_t*)MALLOC(sizeof(size_t)*res->nb_levels);
  res->arity            = (int*)MALLOC(sizeof(int)*res->nb_levels);

  if(vl >= INFO)
      printf("topodepth = %d\n",topodepth);

  /* Build TreeMatch topology */
  for( depth = 0 ; depth < topodepth ; depth++ ){
    nb_nodes = hwloc_get_nbobjs_by_depth(topology, depth);
    res->nb_nodes[depth] = nb_nodes;

    objs    = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*nb_nodes);
    objs[0] = hwloc_get_next_obj_by_depth(topology, depth, NULL);
    hwloc_get_closest_objs(topology, objs[0], objs+1, nb_nodes-1);
    res->arity[depth] = objs[0]->arity;
    
    if(vl >= DEBUG)
      printf("\n--%d(%d) **%d**:--\n",res->arity[depth],nb_nodes,res->arity[0]);

    
    if (depth == topodepth -1){
      res->nb_constraints = nb_nodes;
      res->nb_proc_units  = nb_nodes;
      res->node_id        = (int*)MALLOC(sizeof(int)*nb_nodes);
      res->node_rank      = (int*)MALLOC(sizeof(int)*nb_nodes);
   
      build_process_tab_id(res, objs, filename);
     
    }
    FREE(objs);


  }

  cost = (double*)CALLOC(res->nb_levels,sizeof(double));
  for(l=0; l<res->nb_levels; l++){
    cost[l] = link_cost(l);
  }
  res->cost = cost;


  /* Destroy topology object. */
  hwloc_topology_destroy(topology);
  if(tm_get_verbose_level() >= INFO)
    printf("\n");



  return res;
}

tm_topology_t* tm_get_local_topology_with_hwloc(void)
{
  hwloc_topology_t topology;
  tm_topology_t *res = NULL;
  hwloc_obj_t *objs = NULL;
  unsigned topodepth,depth;
  int nb_nodes;

  /* Build the topology */
  hwloc_topology_init(&topology);

#if HWLOC_API_VERSION < 0x20000
  hwloc_topology_ignore_all_keep_structure(topology);
#else
  hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_STRUCTURE);
#endif

  hwloc_topology_load(topology);

  /* Test if symetric */
  if(!symetric(topology)){
    if(tm_get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Local toplogy not symetric!\n");
    exit(-1);
  }

  /* work on depth */
  topodepth = hwloc_topology_get_depth(topology);

  res                  = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  res->nb_constraints  = 0;
  res->constraints     = NULL;
  res->nb_levels       = topodepth;
  res->nb_nodes        = (size_t*)MALLOC(sizeof(size_t)*res->nb_levels);
  res->arity           = (int*)MALLOC(sizeof(int)*res->nb_levels);
  res->oversub_fact    = 1; //defaut
  res->cost            = NULL; 

  /* Build TreeMatch topology */
  for( depth = 0 ; depth < topodepth ; depth++ ){
    nb_nodes = hwloc_get_nbobjs_by_depth(topology, depth);
    res->nb_nodes[depth] = nb_nodes;

    objs = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*nb_nodes);
    objs[0] = hwloc_get_next_obj_by_depth(topology,depth,NULL);
    hwloc_get_closest_objs(topology,objs[0],objs+1,nb_nodes-1);
    res->arity[depth] = objs[0]->arity;

    if (depth == topodepth -1){
      res->nb_constraints = nb_nodes;
      res->nb_proc_units  = nb_nodes;
      res->node_id        = (int*)MALLOC(sizeof(int)*nb_nodes);
      res->node_rank      = (int*)MALLOC(sizeof(int)*nb_nodes);
    /* printf("%d:",res->arity[depth]); */

      /* Build process id tab */

      build_process_tab_id(res, objs, "Local node topology");
    }
    FREE(objs);
  }



  /* Destroy HWLOC topology object. */
  hwloc_topology_destroy(topology);

  /* printf("\n"); */
  return res;
}


void tm_free_topology(tm_topology_t *topology)
{
  FREE(topology->node_id);
  FREE(topology->node_rank);
  FREE(topology->constraints);
  FREE(topology->nb_nodes);
  FREE(topology->arity);
  FREE(topology->cost);
  FREE(topology);
}

tm_topology_t *tm_load_topology(char *arch_filename, tm_file_type_t arch_file_type){
  switch(arch_file_type){
  case   TM_FILE_TYPE_TGT:
    return  tgt_to_tm(arch_filename);
  case TM_FILE_TYPE_XML:
    return hwloc_to_tm(arch_filename);
  default:
    if(tm_get_verbose_level() >= ERROR){
      fprintf(stderr,"Error loading topology. Filetype %d unknown\n", arch_file_type);
    }
    exit(-1);
  }
}


void tm_display_topology(tm_topology_t *topology)
{
  int i;
  unsigned long  id;
  for( i = 0 ; i < topology->nb_levels ; i++ ){
    printf("Level %d with arity %d ", i, topology->arity[i]); 
    printf("\n");
  }

  printf("Last level: ");
  for(id = 0; id < topology->nb_nodes[topology->nb_levels-1]/topology->oversub_fact; id++)
    printf("%d ",topology->node_rank[id]);
  printf("\n");


  if(topology->constraints){
    printf("Constraints: ");
    for(i = 0; i < topology->nb_constraints; i++)
      printf("%d ",topology->constraints[i]);
    printf("\n");
  }

  printf("\tnb_levels=%d\n\tnb_constraints=%d\n\toversub_fact=%d\n\tnb proc units=%d\n\n",
	 topology->nb_levels, topology->nb_constraints, topology->oversub_fact, topology->nb_proc_units);

}


void tm_display_arity(tm_topology_t *topology){
  int depth;
  for(depth=0; depth < topology->nb_levels; depth++){
    printf("%d",topology->arity[depth]);
    if(topology->cost) 
      printf("(%lf)",topology->cost[depth]); 
    else 
      printf(":"); 
  }
  printf("\n");
}

int tm_int_cmp_inc(const void* x1,const void* x2)
{
  return *((int *)x1) < *((int *)x2) ? -1 : 1;
}


static int topo_check_constraints(tm_topology_t *topology){
  int n = topology->nb_constraints;
  int i;
  int depth = topology->nb_levels-1;
  for (i=0;i<n;i++){
    if(!tm_in_tab(topology->node_id, topology->nb_nodes[depth], topology->constraints[i])){
      if(tm_get_verbose_level() >= CRITICAL){
	fprintf(stderr,"Error! Incompatible constraint with the topology: rank %d in the constraints is not a valid id of any nodes of the topology.\n",topology->constraints[i]);
      }
      return 0;
    }
  }
  return 1;
}




/* cpy flag tells if we need to copy the array.
   Set to 1 when called from the application level and 0 when called from inside the library*/
static int tm_topology_set_binding_constraints_cpy(int *constraints, int nb_constraints, tm_topology_t *topology, int cpy_flag){

  topology -> nb_constraints = nb_constraints;
  if(cpy_flag){
    topology -> constraints    =  (int*)MALLOC(nb_constraints*sizeof(int));
    memcpy(topology -> constraints, constraints, nb_constraints*sizeof(int));
  }else{
    topology -> constraints    = constraints;
  }

  return topo_check_constraints(topology);
}

int tm_topology_set_binding_constraints(int *constraints, int nb_constraints, tm_topology_t *topology){
  return tm_topology_set_binding_constraints_cpy(constraints, nb_constraints, topology, 1);
}

int  tm_topology_add_binding_constraints(char *constraints_filename, tm_topology_t *topology)
{
  int *tab = NULL;
  FILE *pf = NULL;
  char  line[LINE_SIZE],*l = NULL;
  char *ptr = NULL;
  int i,n;
  unsigned int vl = tm_get_verbose_level();


  if (!(pf = fopen(constraints_filename,"r"))) {
    if(vl >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",constraints_filename);
    exit(-1);
  }

  /* compute the size of the array to store the constraints*/
  n = 0;
  if (NULL == fgets(line, LINE_SIZE, pf)) {
    line[0] = '\0';
  }
  l = line;
  while((ptr=strtok(l," \t"))){
    l = NULL;
    if((ptr[0] != '\n') && ( !isspace(ptr[0])) && (*ptr) && (ptr))
      n++;
  }

  tab = (int*)MALLOC(n*sizeof(int));

  rewind(pf);
  if (NULL == fgets(line, LINE_SIZE, pf)) {
    line[0] = '\0';
  }
  fclose(pf);
  l = line;
  i = 0;
  while((ptr=strtok(l," \t"))){
    l = NULL;
    if((ptr[0] != '\n') && ( !isspace(ptr[0])) && (*ptr) && (ptr)){
      if(i < n)
	tab[i] = atoi(ptr);
      else{
	if(vl >= CRITICAL)
	  fprintf(stderr, "More than %d entries in %s\n", n, constraints_filename);
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

  qsort(tab,n,sizeof(int),tm_int_cmp_inc);

  return tm_topology_set_binding_constraints_cpy(tab, n, topology, 0);
}


static void topology_numbering_cpy(tm_topology_t *topology,int **numbering_loc,int *nb_nodes)
{
  int nb_levels;
  unsigned int vl = tm_get_verbose_level();

  nb_levels = topology->nb_levels;
  *nb_nodes = topology->nb_nodes[nb_levels-1];
  if(vl >= INFO)
    printf("nb_nodes=%d\n",*nb_nodes);
  *numbering_loc = (int*)MALLOC(sizeof(int)*(*nb_nodes));
  memcpy(*numbering_loc,topology->node_id,sizeof(int)*(*nb_nodes));
}

static void topology_arity_cpy(tm_topology_t *topology,int **arity,int *nb_levels)
{
  *nb_levels = topology->nb_levels;
  *arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
  memcpy(*arity,topology->arity,sizeof(int)*(*nb_levels));
}

static void topology_constraints_cpy(tm_topology_t *topology,int **constraints,int *nb_constraints)
{
  *nb_constraints = topology->nb_constraints;
  if(topology->constraints){
    *constraints = (int*)MALLOC(sizeof(int)*(*nb_constraints));
    memcpy(*constraints,topology->constraints,sizeof(int)*(*nb_constraints));
  }else{
    *constraints = NULL;
  }
}

static void topology_cost_cpy(tm_topology_t *topology,double **cost)
{
  *cost = (double*)MALLOC(sizeof(double)*(topology->nb_levels));
  memcpy(*cost,topology->cost,sizeof(double)*(topology->nb_levels));
}

static void optimize_arity(int **arity, double **cost, int *nb_levels,int n)
{
  int a,i;
  int *new_arity = NULL;
  double *new_cost = NULL;

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
    check if the arity of level n devides 3
    If this is the case:
    Add a level
    */
    (*nb_levels)++;
    /* Build a new arity and cost arrays  */
    new_arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
    new_cost  = (double*)MALLOC(sizeof(double)*(*nb_levels));
    /*  Copy the begining if the old arrays */
    for( i = 0 ; i < n ; i++){
      new_arity[i] = (*arity)[i];
      new_cost[i] = (*cost)[i];
    }
    /* set the nth level to arity 3  */
    new_arity[n] = 3;
    /* copy the cost to this level*/
    new_cost[n] = (*cost)[n];;
    /* printf("a=%d\n",a); */
    /* Set the (n+1) level to arity a/3 */
    new_arity[n+1] = a/3;
    /*Dupliacte the cost as it is the same level originally*/
    new_cost[n+1] = (*cost)[n];
    /* Copy the end of the arrays */
    for( i = n+2 ; i < *nb_levels ; i++){
      new_arity[i] = (*arity)[i-1];
      new_cost[i] = (*cost)[i-1];
    }
    FREE(*arity);
    FREE(*cost);
    /* if a/3 =3 then go to the next level */
    if(new_arity[n+1] == 3)
      optimize_arity(&new_arity,&new_cost,nb_levels,n);
    else /* continue to this level (remember we just add a new level */
      optimize_arity(&new_arity,&new_cost,nb_levels,n+1);
    *arity=new_arity;
    *cost=new_cost;
  }else if( (a%2==0) && (a>2) ){/* same as above but for arity == 2 instead of 3 */
    (*nb_levels)++;
    new_arity = (int*)MALLOC(sizeof(int)*(*nb_levels));
    new_cost  = (double*)MALLOC(sizeof(double)*(*nb_levels));
    for( i = 0 ; i < n ; i++ ){
      new_arity[i] = (*arity)[i];
      new_cost[i] = (*cost)[i];
    }
    new_arity[n] = 2;
    new_cost[n] = (*cost)[n];;
    /* printf("a=%d\n",a); */
    new_arity[n+1] = a/2;
    new_cost[n+1] = (*cost)[n];
    for( i = n+2 ; i < *nb_levels ; i++ ){
      new_arity[i] = (*arity)[i-1];
      new_cost[i] = (*cost)[i-1];
    }
   FREE(*arity);
    FREE(*cost);
    if(new_arity[n+1] == 2)
      optimize_arity(&new_arity, &new_cost, nb_levels, n);
    else
      optimize_arity(&new_arity, &new_cost, nb_levels, n+1);
    *arity = new_arity;
    *cost= new_cost;
  }else /* if nothing works go to next level.  */
    optimize_arity(arity, cost, nb_levels,n-1);
}




void tm_optimize_topology(tm_topology_t **topology){
  int *arity = NULL,nb_levels;
  int *numbering_loc = NULL,nb_nodes;
  tm_topology_t *new_topo;
  double *cost;
  unsigned int vl = tm_get_verbose_level();
  int *constraints = NULL, nb_constraints;
  int i;

  if(vl >= DEBUG)
    tm_display_arity(*topology);

  topology_arity_cpy(*topology,&arity,&nb_levels);
  topology_numbering_cpy(*topology,&numbering_loc,&nb_nodes);
  topology_constraints_cpy(*topology,&constraints,&nb_constraints);
  topology_cost_cpy(*topology,&cost);


  optimize_arity(&arity,&cost,&nb_levels,nb_levels-2);
  new_topo = tm_build_synthetic_topology(arity, NULL, nb_levels,numbering_loc,nb_nodes);
  new_topo->cost = cost;
  new_topo->constraints    = constraints;
  new_topo->nb_constraints = nb_constraints;
  new_topo->nb_proc_units  = (*topology)->nb_proc_units;
  new_topo->oversub_fact   = (*topology)->oversub_fact;



  if(vl >= DEBUG){
    if(constraints){
      printf("Constraints: ");
      for(i=0;i<nb_constraints;i++)
	printf("%d - ",constraints[i]);
      printf("\n");
    }

    tm_display_arity(new_topo);
  }
  FREE(arity);
  FREE(numbering_loc);
  tm_free_topology(*topology);

  *topology = new_topo;
  /*  exit(-1); */


}



/*
   Build a synthetic balanced topology

   arity : array of arity of the first nb_level (of size nb_levels)
   cost : array of costs between the levels (of size nb_levels)
   core_numbering: numbering of the core by the system. Array of size nb_core_per_node

   nb_core_per_nodes: number of cores of a given node size of the array core_numbering

   The numbering of the cores is done in round robin fashion after a width traversal of the topology.
   for example:
       {0,1,2,3} becomes 0,1,2,3,4,5,6,7...
   and
       {0,2,1,3} becomes 0,2,1,3,4,6,5,7,...
 */

tm_topology_t  *tm_build_synthetic_topology(int *arity, double *cost, int nb_levels, int *core_numbering, int nb_core_per_nodes)
{
  tm_topology_t *topology = NULL;
  int i,j,n;


  topology                 = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  topology->nb_constraints = 0;
  topology->oversub_fact   = 1;
  topology->constraints    = NULL;
  topology->nb_levels      = nb_levels;
  topology->arity          = (int*)MALLOC(sizeof(int)*topology->nb_levels);
  topology->nb_nodes       = (size_t *)MALLOC(sizeof(size_t)*topology->nb_levels);
  if(cost)
    topology->cost         = (double*)CALLOC(topology->nb_levels,sizeof(double));
  else
    topology->cost         = NULL;

  memcpy(topology->arity, arity, sizeof(int)*nb_levels);
  if(cost)
    memcpy(topology->cost, cost, sizeof(double)*nb_levels);

  n = 1;
  for( i = 0 ; i < topology->nb_levels ; i++ ){
    topology->nb_nodes[i] = n;
    if (i == topology->nb_levels-1){
      topology->node_id        = (int*)MALLOC(sizeof(int)*n);
      topology->node_rank      = (int*)MALLOC(sizeof(int)*n);
      topology->nb_constraints = n;
      topology->nb_proc_units  = n;
      for( j = 0 ; j < n ; j++ ){
	int id = core_numbering[j%nb_core_per_nodes] + (nb_core_per_nodes)*(j/nb_core_per_nodes);
	topology->node_id[j]    = id;
	topology->node_rank[id] = j;
      }
    }
    n *= topology->arity[i];
  }
  if(cost){
    /*aggregate costs*/
    for( i = topology->nb_levels-2 ; i >= 0 ; i-- )
      topology->cost[i] += topology->cost[i+1];
  }

  return topology;
}


static void   build_synthetic_proc_id(tm_topology_t *topology)
{
  int i;
  size_t j,n = 1;

  topology->nb_nodes  = (size_t*) MALLOC(sizeof(size_t)*topology->nb_levels);

  for( i = 0 ; i < topology->nb_levels ; i++ ){
    /* printf("n= %lld, arity := %d\n",n, topology->arity[i]); */
    topology->nb_nodes[i] = n;
   
    if (i == topology->nb_levels-1){
      topology->node_rank      = (int*)MALLOC(sizeof(int)*n);
      topology->node_id        = (int*)MALLOC(sizeof(int)*n);
      if ( !topology->node_id ){
	if(tm_get_verbose_level() >= CRITICAL)
	  fprintf(stderr,"Cannot allocate last level (of size %ld) of the topology\n", (unsigned long int)n);
	exit(-1);
      }
      
      topology->nb_constraints = n;
      topology->nb_proc_units = n;
      
      for( j = 0 ; j < n ; j++ ){
	topology->node_id[j]   = j;
	topology->node_rank[j] = j;
      }
    }

    n *= topology->arity[i];
  }

}



void tm_enable_oversubscribing(tm_topology_t *topology, unsigned int oversub_fact){
{
  int i,j,n;
  int *node_id, *node_rank;

  if(oversub_fact <=1)
    return;

  topology -> nb_levels ++;
  topology -> arity        = (int*)    REALLOC(topology->arity, sizeof(int)*topology->nb_levels);
  topology -> cost         = (double*) REALLOC(topology->cost, sizeof(double)*topology->nb_levels);
  topology -> nb_nodes     = (size_t *)REALLOC(topology->nb_nodes, sizeof(size_t)*topology->nb_levels);
  topology -> oversub_fact = oversub_fact;

  i = topology->nb_levels - 1;
  n = topology->nb_nodes[i-1] * oversub_fact;
  topology->arity[i-1] = oversub_fact;
  topology->cost[i-1] = 0;
  node_id = (int*)MALLOC(sizeof(int)*n);
  node_rank = (int*)MALLOC(sizeof(int)*n);
  topology->nb_nodes[i] = n;

  for( j = 0 ; j < n ; j++ ){
    int id = topology->node_id[j/oversub_fact];
    node_id[j]    = id;
    node_rank[id] = j;
  }
  FREE(topology->node_id);
  FREE(topology->node_rank);
  topology->node_id   = node_id;  
  topology->node_rank = node_rank;  
 }

}
