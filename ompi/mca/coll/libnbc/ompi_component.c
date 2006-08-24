#include <stdio.h>
#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi_config.h"
#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#include "ompi_component.h"

#include "nbc.h"

int mca_coll_libnbc_allgather_intra(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iallgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_allgatherv_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void * rbuf, int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iallgatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;

}

int mca_coll_libnbc_allreduce_intra(void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iallreduce(sbuf, rbuf, count, dtype, op, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
}

int mca_coll_libnbc_alltoall_intra(void *sbuf, int scount,
                                 struct ompi_datatype_t *sdtype, 
                                 void *rbuf, int rcount, 
                                 struct ompi_datatype_t *rdtype,
                                 struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Ialltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
}

int mca_coll_libnbc_alltoallv_intra(void *sbuf, int *scounts, int *sdisps,
                               struct ompi_datatype_t *sdtype,
                               void *rbuf, int *rcounts, int *rdisps,
                               struct ompi_datatype_t *rdtype, 
                               struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Ialltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_alltoallw_intra(void *sbuf, int *scounts, int *sdisps,
                                  struct ompi_datatype_t **sdtypes, 
                                  void *rbuf, int *rcounts, int *rdisps,
                                  struct ompi_datatype_t **rdtypes, 
                                  struct ompi_communicator_t *comm)
{

  /* not implemented in libnbc yet ...
  NBC_Handle handle;
  int res;

  res = NBC_Iallgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  */

  return 0;
}

int mca_coll_libnbc_barrier_intra(struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  /*printf("calling barrier ...\n");*/
  res = NBC_Ibarrier(comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
}

int mca_coll_libnbc_bcast_intra(void *buff, int count,
                              struct ompi_datatype_t *datatype, int root,
                              struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Ibcast(buff, count, datatype, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;

}

int mca_coll_libnbc_exscan_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op, 
                               struct ompi_communicator_t *comm)
{
  /* not implemented yet ...
  NBC_Handle handle;
  int res;

  res = NBC_Iallgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  */
  return 0;
  
}

int mca_coll_libnbc_gather_intra(void *sbuf, int scount, 
                               struct ompi_datatype_t *sdtype, 
                               void *rbuf, int rcount, 
                               struct ompi_datatype_t *rdtype, 
                               int root, struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Igather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
}

int mca_coll_libnbc_gatherv_intra(void *sbuf, int scount, 
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int *rcounts, int *disps,
                                struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Igatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Ireduce(sbuf, rbuf, count, dtype, op, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_reduce_scatter_intra(void *sbuf, void *rbuf, int *rcounts,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Ireduce_scatter(sbuf, rbuf, rcounts, dtype, op, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_scan_intra(void *sbuf, void *rbuf, int count,
                             struct ompi_datatype_t *dtype, 
                             struct ompi_op_t *op, 
                             struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iscan(sbuf, rbuf, count, dtype, op, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_scatter_intra(void *sbuf, int scount, 
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int rcount, 
                                struct ompi_datatype_t *rdtype,
                                int root, 
                                struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iscatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}

int mca_coll_libnbc_scatterv_intra(void *sbuf, int *scounts,
                                 int *disps, struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount,
                                 struct ompi_datatype_t *rdtype, int root,
                                 struct ompi_communicator_t *comm)
{
  NBC_Handle handle;
  int res;

  res = NBC_Iscatterv(sbuf, scounts, disps, sdtype, rbuf, rcount, rdtype, root, comm, &handle);
  if(res != NBC_OK) return res;
  res = NBC_Wait(&handle);
  return res;
  
}




















/*
 * Public string showing the coll ompi_libnbc component version number
 */
const char *mca_coll_libnbc_component_version_string =
  "Open MPI libnbc collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int mca_coll_libnbc_priority_param = -1;

/*
 * Local function
 */
static int libnbc_open(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_1_0_0_t mca_coll_libnbc_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itlibnbc */

    {
        /* Indicate that we are a coll v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_COLL_BASE_VERSION_1_0_0,

        /* Component name and version */

        "libnbc",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,

        /* Component open and close functions */

        libnbc_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */
        
        true
    },

    /* Initialization / querying functions */

    mca_coll_libnbc_init_query,
    mca_coll_libnbc_comm_query,
    NULL,
};


static int libnbc_open(void)
{
    /* We'll always be picked if there's only one process in the
       communicator */
    
    mca_coll_libnbc_priority_param = 
        mca_base_param_register_int("coll", "libnbc", "priority", NULL, 1);

    return OMPI_SUCCESS;
}


/*
 * Module
 */
static const mca_coll_base_module_1_0_0_t module = {

  /* Initialization / finalization functions */

  mca_coll_libnbc_module_init,
  mca_coll_libnbc_module_finalize,

  /* Collective function pointers */

  mca_coll_libnbc_allgather_intra,
  mca_coll_libnbc_allgatherv_intra,
  mca_coll_libnbc_allreduce_intra, 
  mca_coll_libnbc_alltoall_intra,
  mca_coll_libnbc_alltoallv_intra,
  NULL, /* not implemented yet - mca_coll_libnbc_alltoallw_intra, */
  mca_coll_libnbc_barrier_intra, 
  mca_coll_libnbc_bcast_intra, 
  NULL, /* not implemented yet - mca_coll_libnbc_exscan_intra, */
  mca_coll_libnbc_gather_intra,
  mca_coll_libnbc_gatherv_intra,
  mca_coll_libnbc_reduce_intra, 
  mca_coll_libnbc_reduce_scatter_intra,
  mca_coll_libnbc_scan_intra,
  mca_coll_libnbc_scatter_intra,
  mca_coll_libnbc_scatterv_intra
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_libnbc_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    /* Nothing to do */
  
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_libnbc_comm_query(struct ompi_communicator_t *comm, int *priority,
                         struct mca_coll_base_comm_t **data)
{
  /* We only work on intracommunicators */

  if (!OMPI_COMM_IS_INTER(comm)) {
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_libnbc_priority_param, priority)) {
      return NULL;
    }
    /* printf("returning prio: %i\n", *priority); */

    return &module;
  }

    return NULL;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t* mca_coll_libnbc_module_init(struct ompi_communicator_t *comm)
{

  comm->c_coll_selected_data = (void*)NBC_Init_comm(comm);
  if(NULL == comm->c_coll_selected_data) return NULL;
  /* printf("communicator initialized comminfo: %lu:)\n", (unsigned long)comm->c_coll_selected_data); */

  return &module;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_libnbc_module_finalize(struct ompi_communicator_t *comm)
{
  return OMPI_SUCCESS;
}

