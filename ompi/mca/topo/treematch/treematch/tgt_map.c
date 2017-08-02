#include <stdio.h>
#include <string.h>
#include <ctype.h>
//#include "tm_hwloc.h"
#include "tm_tree.h"
#include "tm_mapping.h"
#include "tm_timings.h"



int  main(int argc, char**argv){;
  tree_t *comm_tree=NULL;
  double **comm,**arch;
  tm_topology_t *topology;
  int nb_processes,nb_cores;
  int *sol,*k;
  if(argc<3){
    fprintf(stderr,"Usage: %s <Architecture tgt> <communication partern file>\n",argv[0]);
    return -1;
  }

  topology=tgt_to_tm(argv[1],&arch);
   optimize_topology(&topology);
  nb_processes=build_comm(argv[2],&comm);
  sol=(int*)MALLOC(sizeof(int)*nb_processes);

  nb_cores=nb_processing_units(topology);
  k=(int*)MALLOC(sizeof(int)*nb_cores);
  // TreeMatchMapping(nb_processes,nb_cores,comm,sol);

  if(nb_processes>nb_cores){
    fprintf(stderr,"Error: to many processes (%d)  for this topology (%d nodes)\n",nb_processes,nb_cores);
    exit(-1);
  }
  TIC;
  comm_tree=build_tree_from_topology(topology,comm,nb_processes,NULL,NULL);
  map_topology_simple(topology,comm_tree,sol,k);
  double duration=TOC;
  printf("mapping duration: %f\n",duration);
  printf("TreeMatch: ");
  print_sol_inv(nb_processes,sol,comm,arch);
  //print_1D_tab(k,nb_cores);
//  display_other_heuristics(topology,nb_processes,comm,arch);

  //display_tab(arch,nb_cores);

  FREE_topology(topology);
  //FREE_tree(comm_tree);
  FREE(sol);
  FREE(comm);
  FREE(arch);



  return 0;
}
