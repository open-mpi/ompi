#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "tm_hwloc.h"
#include "tm_tree.h"
#include "tm_mapping.h"
#include "tm_timings.h"



int  main(int argc, char**argv){;
  tm_topology_t *topology;
  int nb_cores;
  double **arch;
  if(argc<2){
    fprintf(stderr,"Usage: %s <Architecture tgt>\n",argv[0]);
    return -1;
  }

  topology=tgt_to_tm(argv[1],&arch);
  nb_cores=nb_nodes(topology);

  display_tab(arch,nb_cores);

  FREE_topology(topology);
  FREE(arch);



  return 0;
}
