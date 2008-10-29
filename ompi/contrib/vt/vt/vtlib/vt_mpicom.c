/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_trc.h"
#include "vt_mpicom.h"
#include "vt_error.h"

#include <stdlib.h>
/* 
 *-----------------------------------------------------------------------------
 *
 * Communicator management
 *
 *-----------------------------------------------------------------------------
 */

#define VT_MAX_COMM    50
#define VT_MAX_WIN     50
#define VT_MAX_WINACC  50

struct VTWorld
{
  MPI_Group group;
  int size;
  int size_grpv;
  int* ranks;
};

struct VTWorld world;

/* -- rank translation -- */

int vt_rank_to_pe(int rank, MPI_Comm comm)
{
  MPI_Group group;
  int global_rank;
  int inter;

  PMPI_Comm_test_inter(comm, &inter);
  if ( inter )
    PMPI_Comm_remote_group(comm, &group);
  else
    PMPI_Comm_group(comm, &group);

  PMPI_Group_translate_ranks(group, 1, &rank, world.group, &global_rank);
  PMPI_Group_free(&group);
  return global_rank;
}

/* -- communicator handling -- */

struct VTComm
{
  MPI_Comm        comm;
  MPI_Group	 group;
  uint32_t          cid;
};

static int currcid = 2; /* 0/1 reserved for MPI_COMM_WORLD/MPI_COMM_SELF */
static int last_comm = 0;
static int* ranks;
static struct VTComm comms[VT_MAX_COMM];
static unsigned char* grpv;

int vt_comm_get_cid()
{
  return ++currcid;
}

static int comm_initialized = 0;

void vt_comm_init()
{
  int i;

  if ( !comm_initialized )
  {
    comm_initialized = 1;
    PMPI_Comm_group(MPI_COMM_WORLD, &world.group);
    PMPI_Group_size(world.group, &world.size);
    world.size_grpv = world.size / 8 + (world.size % 8 ? 1 : 0);

    world.ranks  = (int*)calloc(world.size, sizeof(int));
    for (i = 0; i < world.size; i++)
      world.ranks[i] = i;  

    ranks  = (int*)calloc(world.size, sizeof(int));
    grpv = (unsigned char*)calloc(world.size_grpv, sizeof(unsigned char));
  }
}

void vt_comm_finalize()
{
  PMPI_Group_free(&world.group);

  free(world.ranks);
  free(ranks);
  free(grpv);
}

void vt_group_to_bitvector(MPI_Group group)
{
  int i;
  /* translate ranks */
  PMPI_Group_translate_ranks(world.group, world.size, world.ranks, group, ranks);
  
  /* initialize grpv */
  for (i = 0; i < world.size_grpv; i++)
    grpv[i] = 0;

  /* which process in MPI_COMM_WORLD is member of comm */
  for (i = 0; i < world.size; i++)
    if (ranks[i] != MPI_UNDEFINED)
      grpv[i / 8] |= (1 << (i % 8));
}

void vt_comm_create(MPI_Comm comm)
{
  int i;
  MPI_Group group;
  
  if (last_comm >= VT_MAX_COMM) 
    vt_error_msg("Too many communicators");

  /* ask for group of comm */
  PMPI_Comm_group(comm, &group);

  /* check if group already exists w/o communicator */
  if ((i = vt_group_search( group ) != -1) &&
      (comms[i].comm == MPI_COMM_NULL))
    { 
      /* just set communicator to comm */
      comms[i].comm = comm;
    }  
  else
    {
      /* create group entry in grpv */
      vt_group_to_bitvector( group );
  
      /* register mpi communicator definition */
      vt_def_mpi_comm(currcid, world.size_grpv, grpv);

      /* enter comm in comms[] arrray */
      comms[last_comm].comm  = comm;
      comms[last_comm].group = group;
      comms[last_comm].cid   = currcid++;
      last_comm++;
    }
  
  /* clean up */
  PMPI_Group_free(&group);
}

void vt_comm_free(MPI_Comm comm)
{
  if (last_comm == 1 && comms[0].comm == comm)
    {
      last_comm = 0;
    }
  else if (last_comm > 1)
    {
      int i = 0;
      
      while(i < last_comm && comms[i].comm != comm)
        i++;
      
      if (i < last_comm--)
        comms[i] = comms[last_comm];
      else
        vt_error_msg("vt_comm_free1: Cannot find communicator");
    }
  else
      vt_error_msg("vt_comm_free2: Cannot find communicator");
}

uint32_t vt_comm_id(MPI_Comm comm)
{
  int i = 0;

  while(i < last_comm && comms[i].comm != comm)
    i++;
  
  if (i <= last_comm)
    return comms[i].cid;
  else 
    {
      vt_error_msg("vt_comm_id: Cannot find communicator");  
      return (uint32_t)-1;
    }
}

/* 
 *-----------------------------------------------------------------------------
 *
 * Group management
 *
 *-----------------------------------------------------------------------------
 */

void vt_group_create(MPI_Group group)
{
  if (last_comm >= VT_MAX_COMM) 
    vt_error_msg("Too many communicators");

  /* check if group already exists w/ communicator */
  if (vt_group_search( group ) == -1)
    {
      /* create group entry in grpv */
      vt_group_to_bitvector( group );
  
      /* register mpi communicator definition */
      vt_def_mpi_comm(currcid, world.size_grpv, grpv);

      /* enter comm in comms[] arrray */
      comms[last_comm].comm  = MPI_COMM_NULL;
      comms[last_comm].group = group;
      comms[last_comm].cid   = currcid++;
      last_comm++;  
    }
}

void vt_group_free(MPI_Group group)
{
   int pos = vt_group_search(group);
   if ( pos != -1 )
     {
       comms[pos].group = MPI_GROUP_EMPTY;
       comms[pos].cid   = 0;
     }
}

uint32_t vt_group_id(MPI_Group group)
{
  int i = 0;
  
  while ((i < last_comm) && (comms[i].group != group))
    i++;
      
  if (i != last_comm)
    return comms[i].cid;  
  else
    {
      vt_error_msg("Cannot find group");  
      return (uint32_t)-1; 
    }
}

int vt_group_search(MPI_Group group)
{
  int i = 0;
  
  while ((i < last_comm) && (comms[i].group != group))
    i++;
      
  if (i != last_comm)
    return i;  
  else
    return -1; 
}
