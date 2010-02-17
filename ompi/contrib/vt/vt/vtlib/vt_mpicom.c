/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <stdlib.h>

#include "vt_trc.h"
#include "vt_mpicom.h"
#include "vt_error.h"

#define VT_MAX_COMM 50
#define VT_MAX_WIN 100

struct VTWorld
{
  MPI_Group   group;
  VT_MPI_INT  size;
  VT_MPI_INT  size_grpv;
  VT_MPI_INT* ranks;
};

/* -- communicator handling -- */

struct VTComm
{
  MPI_Comm    comm;
  MPI_Group   group;
  uint32_t    cid;
};

/* -- window handling -- */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
struct VTWin
{
  MPI_Win     win;
  MPI_Comm    comm;
  uint32_t    gid;
  uint32_t    wid;
};
#endif /* HAVE_MPI2_1SIDED */

struct VTWorld world;

static uint32_t currcid = 0; /* 0/1 reserved for MPI_COMM_WORLD/MPI_COMM_SELF */
static uint32_t last_comm = 0;
static VT_MPI_INT* ranks;
static struct VTComm comms[VT_MAX_COMM];
static uint8_t* grpv;

static uint8_t comm_initialized = 0;

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
static uint32_t currwid = 0;
static uint32_t free_win = (uint32_t)-1;
static uint32_t last_win = 0;
static struct VTWin wins[VT_MAX_WIN];
#endif /* HAVE_MPI2_1SIDED */


static uint32_t group_search(MPI_Group group)
{
  uint32_t i = 0;
  
  while ((i < last_comm) && (comms[i].group != group))
    i++;
      
  if (i != last_comm)
    return i;  
  else
    return (uint32_t)-1; 
}

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
static uint32_t win_search(MPI_Win win)
{
  uint32_t i = 0;
  
  free_win = (uint32_t)-1;
  while ((i < last_win)&& (wins[i].win != win))
    {
      if( (free_win == (uint32_t)-1) && (wins[i].win == MPI_WIN_NULL) &&
          (wins[i].comm == MPI_COMM_NULL) )
        free_win = i;
      i++;
    }

  if (i != last_win)
    return i;
  else
    return (uint32_t)-1;
}
#endif /*HAVE_MPI2_1SIDED */

/* 
 *-----------------------------------------------------------------------------
 *
 * Communicator management
 *
 *-----------------------------------------------------------------------------
 */

/* -- rank translation -- */

uint32_t vt_rank_to_pe(VT_MPI_INT rank, MPI_Comm comm)
{
  MPI_Group group;
  VT_MPI_INT global_rank;
  VT_MPI_INT inter;

  PMPI_Comm_test_inter(comm, &inter);
  if ( inter )
    PMPI_Comm_remote_group(comm, &group);
  else
    PMPI_Comm_group(comm, &group);

  PMPI_Group_translate_ranks(group, 1, &rank, world.group, &global_rank);
  PMPI_Group_free(&group);
  return (uint32_t)global_rank;
}

void vt_comm_init()
{
  VT_MPI_INT i;

  if ( !comm_initialized )
  {
    comm_initialized = 1;
    PMPI_Comm_group(MPI_COMM_WORLD, &world.group);
    PMPI_Group_size(world.group, &world.size);
    world.size_grpv = world.size / 8 + (world.size % 8 ? 1 : 0);

    world.ranks  = (VT_MPI_INT*)calloc(world.size, sizeof(VT_MPI_INT));
    for (i = 0; i < world.size; i++)
      world.ranks[i] = i;  

    ranks  = (VT_MPI_INT*)calloc(world.size, sizeof(VT_MPI_INT));
    grpv = (uint8_t*)calloc(world.size_grpv, sizeof(uint8_t));

    vt_comm_create( MPI_COMM_WORLD );
    vt_comm_create( MPI_COMM_SELF );
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
  VT_MPI_INT i;

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
  if ((i = group_search( group ) != (uint32_t)-1) &&
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
      uint32_t i = 0;
      
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
  uint32_t i = 0;

  while(i < last_comm && comms[i].comm != comm)
    i++;
  
  if (i != last_comm)
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
  if (group_search( group ) == (uint32_t)-1)
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
  /* The follow code to "free" the comm/group entry is unusable.  A thorough
     check of the usage of active communicators is required to verify that
     none still require it, then the entire comm/group entry should be deleted
     so that it can be subsequently re-used. */

#if 0
  uint32_t pos = group_search(group);
  if ( pos != (uint32_t)-1 )
    {
      comms[pos].group = MPI_GROUP_EMPTY;
      comms[pos].cid   = 0;
    }
  else
    {
      vt_error_msg("Cannot find group");
    }
#endif
}

uint32_t vt_group_id(MPI_Group group)
{
  uint32_t pos = group_search(group);
  
  if ( pos != (uint32_t)-1 )
    {
      return comms[pos].cid;  
    }
  else
    {
      vt_error_msg("Cannot find group");  
      return (uint32_t)-1; 
    }
}

/* 
 *-----------------------------------------------------------------------------
 *
 * Window management
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

void vt_win_create( MPI_Win win, MPI_Comm comm )
{
  MPI_Group group;

  /* check if window already exists */
  if (win_search( win ) == (uint32_t)-1)
    {
      PMPI_Win_get_group(win, &group); 

      /* enter win in wins[] arrray */

      if( free_win != (uint32_t)-1 )
        {
          wins[free_win].win  = win;
          wins[free_win].comm = comm;
          wins[free_win].gid  = vt_group_id(group);
          wins[free_win].wid  = currwid++;
        }
      else if ( last_win < VT_MAX_WIN ) 
        {
          wins[last_win].win  = win;
          wins[last_win].comm = comm;
          wins[last_win].gid  = vt_group_id(group);
          wins[last_win].wid  = currwid++;
          last_win++;
        }
      else 
        vt_error_msg("Too many windows");
    }
}

void vt_win_free( MPI_Win win )
{
  uint32_t pos = win_search(win);
  if ( pos != (uint32_t)-1 )
    {
      wins[pos].win  = MPI_WIN_NULL;
      wins[pos].comm = MPI_COMM_NULL;
      wins[pos].gid  = 0;
      wins[pos].wid  = 0;
    }
  else
    {
      vt_error_msg("Cannot find window");  
    }
}

void vt_win_id( MPI_Win win, MPI_Comm* comm, uint32_t* gid, uint32_t* wid )
{
  uint32_t pos = win_search(win);
  
  if ( pos != (uint32_t)-1 )
    {
      *comm = wins[pos].comm;
      *gid  = wins[pos].gid;
      *wid  = wins[pos].wid; 
    }
  else
    {
      vt_error_msg("Cannot find window");  
    }
}

void vt_win_set_gid( MPI_Win win, uint32_t gid )
{
  uint32_t pos = win_search(win);

  if ( pos != (uint32_t)-1 )
    {
      wins[pos].gid = gid;
    }
  else
    {
      vt_error_msg("Cannot find window");
    }
}

#endif /* HAVE_MPI2_1SIDED */
