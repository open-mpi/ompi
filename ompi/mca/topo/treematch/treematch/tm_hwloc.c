#include <hwloc.h>
#include <hwloc/helper.h>
#include "tm_tree.h"
#include "tm_mapping.h"
#include <ctype.h>
#include "tm_verbose.h"


double ** tm_topology_to_arch(tm_topology_t *topology,double *cost);
tm_topology_t * tgt_to_tm(char *filename,double **pcost);
int topo_nb_proc(hwloc_topology_t topology,int N);
double ** topology_to_arch(hwloc_topology_t topology);
int symetric(hwloc_topology_t topology);
tm_topology_t* hwloc_to_tm(char *filename,double **pcost);
tm_topology_t* get_local_topo_with_hwloc(void);




/* transform a tgt scotch file into a topology file*/
tm_topology_t * tgt_to_tm(char *filename, double **pcost)
{
  tm_topology_t *topology = NULL;
  FILE *pf = NULL;
  char line[1024];
  char *s = NULL;
  double *cost = NULL;
  int i;



  pf = fopen(filename,"r");
  if(!pf){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Cannot open %s\n",filename);
    exit(-1);
  }

  if(get_verbose_level() >= INFO)
    printf("Reading TGT file: %s\n",filename);


  fgets(line,1024,pf);

  s = strstr(line,"tleaf");
  if(!s){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Syntax error! %s is not a tleaf file\n",filename);
    exit(-1);
  }

  s += 5;
  while(isspace(*s))
    s++;

  topology = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  topology->nb_levels = atoi(strtok(s," "))+1;
  topology->arity = (int*)MALLOC(sizeof(int)*topology->nb_levels);
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

  *pcost = cost;
  /* FREE(cost); */
  /*
  topology->arity[0]=nb_proc;
  topology->nb_levels=decompose((int)ceil((1.0*nb_obj)/nb_proc),1,topology->arity);
  printf("levels=%d\n",topology->nb_levels);
  */
  if(get_verbose_level() >= INFO)
    printf("Topology built from %s!\n",filename);


  return topology;
}

int topo_nb_proc(hwloc_topology_t topology,int N)
{
  hwloc_obj_t *objs = NULL;
  int nb_proc;

  objs = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*N);
  objs[0] = hwloc_get_next_obj_by_type(topology,HWLOC_OBJ_PU,NULL);
  nb_proc = 1 + hwloc_get_closest_objs(topology,objs[0],objs+1,N-1);
  FREE(objs);
  return nb_proc;
}


double ** topology_to_arch(hwloc_topology_t topology)
{
  int nb_proc,i,j;
  hwloc_obj_t obj_proc1,obj_proc2,obj_res;
  double **arch = NULL;

  nb_proc = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_PU);
  arch = (double**)MALLOC(sizeof(double*)*nb_proc);
  for( i = 0 ; i < nb_proc ; i++ ){
    obj_proc1 = hwloc_get_obj_by_type(topology,HWLOC_OBJ_PU,i);
    arch[obj_proc1->os_index] = (double*)MALLOC(sizeof(double)*nb_proc);
    for( j = 0 ; j < nb_proc ; j++ ){
      obj_proc2 = hwloc_get_obj_by_type(topology,HWLOC_OBJ_PU,j);
      obj_res = hwloc_get_common_ancestor_obj(topology,obj_proc1,obj_proc2);
      /* printf("arch[%d][%d] <- %ld\n",obj_proc1->os_index,obj_proc2->os_index,*((long int*)(obj_res->userdatab))); */
      arch[obj_proc1->os_index][obj_proc2->os_index]=speed(obj_res->depth+1);
    }
  }
  return arch;
}

int symetric(hwloc_topology_t topology)
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

tm_topology_t* hwloc_to_tm(char *filename,double **pcost)
{
  hwloc_topology_t topology;
  tm_topology_t *res = NULL;
  hwloc_obj_t *objs = NULL;
  unsigned topodepth,depth;
  int nb_nodes,i;
  double *cost;
  int err;

  /* Build the topology */
  hwloc_topology_init(&topology);
  err = hwloc_topology_set_xml(topology,filename);
  if(err == -1){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Error: %s is a bad xml topology file!\n",filename);
    exit(-1);
  }

  hwloc_topology_ignore_all_keep_structure(topology);
  hwloc_topology_load(topology);


  /* Test if symetric */
  if(!symetric(topology)){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"%s not symetric!\n",filename);
    exit(-1);
  }

  /* work on depth */
  topodepth = hwloc_topology_get_depth(topology);

  res = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  res->nb_levels = topodepth;
  res->node_id = (int**)MALLOC(sizeof(int*)*res->nb_levels);
  res->nb_nodes = (int*)MALLOC(sizeof(int)*res->nb_levels);
  res->arity = (int*)MALLOC(sizeof(int)*res->nb_levels);

  if(get_verbose_level() >= INFO)
      printf("topodepth = %d\n",topodepth);

  /* Build TreeMatch topology */
  for( depth = 0 ; depth < topodepth ; depth++ ){
    nb_nodes = hwloc_get_nbobjs_by_depth(topology, depth);
    res->nb_nodes[depth] = nb_nodes;
    res->node_id[depth] = (int*)MALLOC(sizeof(int)*nb_nodes);

    objs = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*nb_nodes);
    objs[0] = hwloc_get_next_obj_by_depth(topology,depth,NULL);
    hwloc_get_closest_objs(topology,objs[0],objs+1,nb_nodes-1);
    res->arity[depth] = objs[0]->arity;

    if(get_verbose_level() >= INFO)
      printf("%d(%d):",res->arity[depth],nb_nodes);

    /* Build process id tab */
    for (i = 0; i < nb_nodes; i++){
      res->node_id[depth][i] = objs[i]->os_index;
      /* if(depth==topodepth-1) */
    }
    FREE(objs);
  }

  cost = (double*)CALLOC(res->nb_levels,sizeof(double));
  for(i=0; i<res->nb_levels; i++){
    cost[i] = speed(i);
  }

  *pcost = cost;


  /* Destroy topology object. */
  hwloc_topology_destroy(topology);
  if(get_verbose_level() >= INFO)
    printf("\n");
  return res;
}

tm_topology_t* get_local_topo_with_hwloc(void)
{
  hwloc_topology_t topology;
  tm_topology_t *res = NULL;
  hwloc_obj_t *objs = NULL;
  unsigned topodepth,depth;
  int nb_nodes,i;

  /* Build the topology */
  hwloc_topology_init(&topology);
  hwloc_topology_ignore_all_keep_structure(topology);
  hwloc_topology_load(topology);

  /* Test if symetric */
  if(!symetric(topology)){
    if(get_verbose_level() >= CRITICAL)
      fprintf(stderr,"Local toplogy not symetric!\n");
    exit(-1);
  }

  /* work on depth */
  topodepth = hwloc_topology_get_depth(topology);

  res = (tm_topology_t*)MALLOC(sizeof(tm_topology_t));
  res->nb_levels = topodepth;
  res->node_id = (int**)MALLOC(sizeof(int*)*res->nb_levels);
  res->nb_nodes = (int*)MALLOC(sizeof(int)*res->nb_levels);
  res->arity = (int*)MALLOC(sizeof(int)*res->nb_levels);

  /* Build TreeMatch topology */
  for( depth = 0 ; depth < topodepth ; depth++ ){
    nb_nodes = hwloc_get_nbobjs_by_depth(topology, depth);
    res->nb_nodes[depth] = nb_nodes;
    res->node_id[depth] = (int*)MALLOC(sizeof(int)*nb_nodes);

    objs = (hwloc_obj_t*)MALLOC(sizeof(hwloc_obj_t)*nb_nodes);
    objs[0] = hwloc_get_next_obj_by_depth(topology,depth,NULL);
    hwloc_get_closest_objs(topology,objs[0],objs+1,nb_nodes-1);
    res->arity[depth] = objs[0]->arity;

    /* printf("%d:",res->arity[depth]); */

    /* Build process id tab */
    for (i = 0; i < nb_nodes; i++){
      res->node_id[depth][i] = objs[i]->os_index;
      /* if(depth==topodepth-1) */
    }
    FREE(objs);
  }

  /* Destroy HWLOC topology object. */
  hwloc_topology_destroy(topology);

  /* printf("\n"); */
  return res;
}

