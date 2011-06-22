/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_FILTER_COMMON_H_
#define _VT_FILTER_COMMON_H_

#include "vt_inttypes.h"

#ifdef VT_MPI
# include "vt_defs.h" // to get VT_MPI_INT
# include "mpi.h"
#endif // VT_MPI

//
// FilterCommonC class
//
class FilterCommonC
{
public:

  // contructor
  FilterCommonC();

  // destructor
  virtual ~FilterCommonC();

protected:

  // prepare progress
  bool prepareProgress( const uint64_t& maxBytes );

  // update progress
  bool updateProgress( const uint64_t& bytes );

  // finish progress
  bool finishProgress( void );

#ifdef VT_MPI
  // get number and communicator of worker ranks
  void getWorkerComm( const uint32_t& maxWorkers );

  // communicator of worker ranks
  MPI_Comm   m_workerComm;

  // number of worker ranks
  VT_MPI_INT m_numWorkerRanks;
#endif // VT_MPI

private:

  //
  // data structure for progress
  //
  struct ProgressS
  {
    uint64_t         curBytes;     // current bytes read
    uint64_t         maxBytes;     // max. bytes readable

#ifdef VT_MPI
    // The following variables are significant for the MPI-parallel version of
    // vtfilter to get the progresses of all worker ranks.

    static const VT_MPI_INT msgTag = 500; // message tag

    uint64_t*        rankCurBytes; // current bytes read per rank

    MPI_Request      sendRequest;  // sender request handle

    uint64_t*        recvBuffers;  // receive buffers
    MPI_Request*     recvRequests; // persistent receive request handles
    MPI_Status*      recvStatuses; // receive statuses
    VT_MPI_INT*      recvIndices;  // indices of completed receive operations
#endif // VT_MPI

  };

  // storage of progress information
  ProgressS m_progress;

};

#endif // _VT_FILTER_COMMON_H_
