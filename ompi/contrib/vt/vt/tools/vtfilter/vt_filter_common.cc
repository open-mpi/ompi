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

#include "vt_filter.h"
#include "vt_filter_common.h"

#include <iostream>

#include <assert.h>
#include <stdio.h>


//////////////////// class FilterCommonC ////////////////////

// public methods
//

FilterCommonC::FilterCommonC()
{
#ifdef VT_MPI
  m_workerComm = MPI_COMM_WORLD;
  m_numWorkerRanks = NumRanks;
#endif // VT_MPI
}

FilterCommonC::~FilterCommonC()
{
#ifdef VT_MPI
  if( m_workerComm != MPI_COMM_WORLD && m_workerComm != MPI_COMM_NULL )
    MPI_Comm_free( &m_workerComm );
#endif // VT_MPI
}

// protected methods
//

bool
FilterCommonC::prepareProgress( const uint64_t& maxBytes )
{
  bool error = false;

  uint64_t max_bytes = maxBytes;

#ifdef VT_MPI
  if( m_numWorkerRanks > 1 )
  {
    // reduce max. bytes to rank 0
    //
    uint64_t sum_max_bytes;
    MPI_Reduce( &max_bytes, &sum_max_bytes, 1, MPI_LONG_LONG_INT, MPI_SUM, 0,
                m_workerComm );
    MASTER max_bytes = sum_max_bytes;
  }
#endif // VT_MPI

  // initalize variables of data structure ProgressS
  //

  m_progress.curBytes = 0;
  m_progress.maxBytes = max_bytes;

#ifdef VT_MPI
  // initialize MPI related variables of data structure ProgressS
  //
  if( m_numWorkerRanks > 1 )
  {
    MASTER
    {
      // allocate memory for some arrays
      //
      m_progress.rankCurBytes = new uint64_t[m_numWorkerRanks];
      assert( m_progress.rankCurBytes );
      m_progress.recvBuffers = new uint64_t[m_numWorkerRanks-1];
      assert( m_progress.recvBuffers );
      m_progress.recvRequests = new MPI_Request[m_numWorkerRanks-1];
      assert( m_progress.recvRequests );
      m_progress.recvStatuses = new MPI_Status[m_numWorkerRanks-1];
      assert( m_progress.recvStatuses );
      m_progress.recvIndices = new VT_MPI_INT[m_numWorkerRanks-1];
      assert( m_progress.recvIndices );

      // initialize arrays
      //
      for( int i = 0; i < m_numWorkerRanks; i++ )
      {
        // initialize array of current bytes read
        m_progress.rankCurBytes[i] = 0;

        if( i > 0 )
        {
          // create persistent request handle
          MPI_Recv_init( &(m_progress.recvBuffers[i-1]), 1, MPI_LONG_LONG_INT,
                         i, m_progress.msgTag, m_workerComm,
                         &(m_progress.recvRequests[i-1]) );

          // start persistent communication
          MPI_Start( &(m_progress.recvRequests[i-1]) );
        }
      }
    }
    else // SLAVE
    {
      // initialize request handle for sending progress to rank 0
      m_progress.sendRequest = MPI_REQUEST_NULL;
    }

    // block until all worker ranks have reached this point to avoid that the
    // progress does a big jump at beginning
    MPI_Barrier( m_workerComm );
  }
#endif // VT_MPI

  return !error;
}

bool
FilterCommonC::updateProgress( const uint64_t& bytes )
{
  bool error = false;

#if defined(HAVE_OMP) && HAVE_OMP
# pragma omp critical (progress)
  {
#endif // HAVE_OMP

  // add bytes to current bytes read
  m_progress.curBytes += bytes;

#ifdef VT_MPI
  if( m_numWorkerRanks > 1 )
  {
    MASTER
    {
      // add bytes to current bytes read of rank 0
      m_progress.rankCurBytes[0] += bytes;

#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp master
      {
#endif // HAVE_OMP

      // get current bytes read from all worker ranks
      //

      VT_MPI_INT out_count;

      // rank 0 is finished? (called from finishProgress())
      if( bytes == 0 )
      {
        // yes, wait for one or more updates from worker ranks
        MPI_Waitsome( m_numWorkerRanks - 1, m_progress.recvRequests, &out_count,
                      m_progress.recvIndices, m_progress.recvStatuses );
      }
      else
      {
        // no, test for one or more updates from worker ranks
        MPI_Testsome( m_numWorkerRanks - 1, m_progress.recvRequests, &out_count,
                      m_progress.recvIndices, m_progress.recvStatuses );
      }

      if( out_count != MPI_UNDEFINED )
      {
        int index;
        int i;

        for( i = 0; i < out_count; i++ )
        {
          index = m_progress.recvIndices[i];

          // worker rank (index+1) is finished?
          if( m_progress.recvBuffers[index] != (uint64_t)-1 )
          {
            // no, update rank's current bytes read and restart persistent
            // communication
            //
            m_progress.rankCurBytes[index+1] = m_progress.recvBuffers[index];
            MPI_Start( &(m_progress.recvRequests[m_progress.recvIndices[i]]) );
          }
        }

        // recompute sum of current bytes read
        //
        m_progress.curBytes = 0;
        for( i = 0; i < m_numWorkerRanks; i++ )
          m_progress.curBytes += m_progress.rankCurBytes[i];
      }

#if defined(HAVE_OMP) && HAVE_OMP
      } // omp master
#endif // HAVE_OMP
    }
    else // SLAVE
    {
#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp master
      {
#endif // HAVE_OMP

      // send current bytes read to rank 0
      //

      VT_MPI_INT do_send = 1;
      MPI_Status status;

      // send only if it's the first send or the request handle isn't
      // currently in use
      //
      if( m_progress.sendRequest != MPI_REQUEST_NULL )
        MPI_Test( &(m_progress.sendRequest), &do_send, &status );

      if( do_send )
      {
        MPI_Issend( &(m_progress.curBytes), 1, MPI_LONG_LONG_INT, 0,
                    m_progress.msgTag, m_workerComm, &m_progress.sendRequest );
      }

#if defined(HAVE_OMP) && HAVE_OMP
      } // omp master
#endif // HAVE_OMP
    }
  }
#endif // VT_MPI

  MASTER
  {
    // show progress
    //

    double progress =
      100.0 * (double)m_progress.curBytes / (double)m_progress.maxBytes;

    printf( " %7.2f %%\r", progress );
    fflush( stdout );
  }

#if defined(HAVE_OMP) && HAVE_OMP
  } // omp critical
#endif // HAVE_OMP

  return !error;
}

bool
FilterCommonC::finishProgress()
{
  bool error = false;

#ifdef VT_MPI
  if( m_numWorkerRanks > 1 )
  {
    MASTER
    {
      // update progress until all worker ranks are
      // finished / all bytes are read
      //
      while( !error && m_progress.curBytes < m_progress.maxBytes )
      {
        error = !updateProgress( 0 );
      }
    }
    else // SLAVE
    {
      MPI_Status status;
      MPI_Wait( &(m_progress.sendRequest), &status );

      // send last current bytes read to rank 0
      MPI_Send( &(m_progress.curBytes), 1, MPI_LONG_LONG_INT, 0,
                m_progress.msgTag, m_workerComm );

      // send marker (-1) to rank 0 which indicates that this worker rank is
      // finished
      //
      m_progress.curBytes = (uint64_t)-1;
      MPI_Send( &(m_progress.curBytes), 1, MPI_LONG_LONG_INT, 0,
                m_progress.msgTag, m_workerComm );
    }
  }
#endif // VT_MPI

  MASTER
  {
    // show final progress
    printf( " %7.2f %%  done\n", 100.0 );
  }

#ifdef VT_MPI
  if( m_numWorkerRanks > 1 )
  {
    MASTER
    {
      // free memory of some arrays
      //

      // ensure that all requests are inactive before freeing memory
      MPI_Waitall( m_numWorkerRanks - 1, m_progress.recvRequests,
                   m_progress.recvStatuses );

      // free memory
      //
      delete [] m_progress.rankCurBytes;
      delete [] m_progress.recvBuffers;
      delete [] m_progress.recvRequests;
      delete [] m_progress.recvStatuses;
      delete [] m_progress.recvIndices;
    }
  }
#endif // VT_MPI

  return !error;
}

#ifdef VT_MPI

void
FilterCommonC::getWorkerComm( const uint32_t& maxWorkers )
{
  if( NumRanks == 1 )
    return;

  MASTER
  {
    // check for MPI tasks which will be out of work
    //
    if( (uint32_t)NumRanks > maxWorkers )
    {
      m_numWorkerRanks = maxWorkers;

      std::cerr << ExeName << ": Warning: Too many MPI tasks in use. "
                << NumRanks - m_numWorkerRanks << " of " << NumRanks
                << " tasks will be out of work." << std::endl;
    }
  }

  // broadcast number of worker ranks
  MPI_Bcast( &m_numWorkerRanks, 1, MPI_INT, 0, MPI_COMM_WORLD );

  // create communicator for worker ranks
  //
  if( m_numWorkerRanks != NumRanks )
  {
    // is my rank needed?
    if( MyRank < m_numWorkerRanks )
    {
      // yes, put my rank into the worker communicator
      MPI_Comm_split( MPI_COMM_WORLD, 4711, MyRank, &m_workerComm );
    }
    else
    {
      // no, the worker communicator will be MPI_COMM_NULL
      MPI_Comm_split( MPI_COMM_WORLD, MPI_UNDEFINED, MyRank, &m_workerComm );
    }
  }
}

#endif // VT_MPI
