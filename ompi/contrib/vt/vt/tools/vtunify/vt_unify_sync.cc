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

#include "vt_unify.h"
#include "vt_unify_sync.h"

#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(HAVE_MKL) && HAVE_MKL
# include "mkl.h"
# define CLAPACK_INT    MKL_INT
# define CLAPACK_DOUBLE double
# define dgemm_ dgemm
#elif defined(HAVE_ACML) && HAVE_ACML
# include "acml.h"
# define CLAPACK_INT    int
# define CLAPACK_DOUBLE double
# define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
                _beta, _c, _ldc) \
    dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, _beta, \
           _c, _ldc, 1, 1)
# define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
                _info) \
    dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
           _info, 1)
#elif defined(HAVE_ESSL) && HAVE_ESSL
# include "essl.h"
# define CLAPACK_INT    _ESVINT
# define CLAPACK_DOUBLE double
# define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
                _beta, _c, _ldc) \
    esvdgemm(_transa, _transb, *(_m), *(_n), *(_k), *(_alpha), (void*)(_a), \
             *(_lda), (void*)(_b), *(_ldb), *(_beta), (void*)(_c), *(_ldc))
# define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
                _info) \
    esvdgels(_trans, *(_m), *(_n), *(_nrhs), (void*)(_a), *(_lda), \
             (void*)(_b), *(_ldb), _work, *(_lwork), *(_info))
#elif defined(HAVE_SUNPERF) && HAVE_SUNPERF
# include "sunperf.h"
# define CLAPACK_INT    int
# define CLAPACK_DOUBLE double
/* since Sun Studio 12 the prototype was extended */
  #if __SUNPRO_CC >= 0x590
#   define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
		  _beta, _c, _ldc)					\
      dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, _beta, \
	     _c, _ldc, 1, 1)
#   define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
		  _info)						\
      dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
	     _info, 1)    
  #endif
#else // CLAPACK
  extern "C" {
# include "f2c.h"
# include "clapack.h"
# include "blaswrap.h"
  int dgemm_(char*, char*, integer*, integer*, integer*, doublereal*,
	     doublereal*, integer*, doublereal*, integer*, doublereal*,
	     doublereal*, integer*);
} // extern "C"
# define CLAPACK_INT    integer
# define CLAPACK_DOUBLE doublereal
#endif // CLAPACK

Synchronization * theSynchronization; // instance of class Synchronization

//////////////////// class Synchronization ////////////////////

// public methods
//

Synchronization::Synchronization()
{
   uint32_t i, j;

   m_uProcNum = 0;

   for( i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      assert( g_vecUnifyCtls[i]->p_vec_sync_phases &&
	      g_vecUnifyCtls[i]->p_vec_sync_phases->size() > 0 );

      if( g_vecUnifyCtls[i]->p_vec_sync_times )
      {
	 assert( g_vecUnifyCtls[i]->p_vec_sync_pairs );
      }
      if( g_vecUnifyCtls[i]->p_vec_sync_pairs )
      {
	 assert( g_vecUnifyCtls[i]->p_vec_sync_times );
      }

      // continue if current process doesn't have sync. information
      if( !g_vecUnifyCtls[i]->p_vec_sync_times &&
	  !g_vecUnifyCtls[i]->p_vec_sync_pairs )
      {
	 continue;
      }

      assert(g_vecUnifyCtls[i]->p_vec_sync_times->size() ==
	     g_vecUnifyCtls[i]->p_vec_sync_pairs->size() );

      // set number of sync. phases, if first process
      if( i == 0 )
      {
	 m_uRoundMax = g_vecUnifyCtls[i]->p_vec_sync_phases->size();
      }
      // check whether number of sync. phases is equal to first process
      else
      {
	 assert( g_vecUnifyCtls[i]->p_vec_sync_phases->size() == m_uRoundMax );
      }
      
      for( j = 0; j < g_vecUnifyCtls[i]->p_vec_sync_times->size(); j++ )
      {
	 // add timestamps to the global map
	 std::pair<uint32_t, uint32_t> key =
	    (*(g_vecUnifyCtls[i]->p_vec_sync_pairs))[j];
	 SyncTime_struct sync_time =
	    (*(g_vecUnifyCtls[i]->p_vec_sync_times))[j];

	 std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>,
	    SyncTime_struct*>*>::iterator iter_map_timestamps =
	    m_mapSyncPhasemapProcessIdsTimestamps.find(sync_time.phase_idx);
			  
	 if( iter_map_timestamps == m_mapSyncPhasemapProcessIdsTimestamps.end() )
	 {
	    iter_map_timestamps =
	       m_mapSyncPhasemapProcessIdsTimestamps.insert(
		  std::make_pair( sync_time.phase_idx, new std::map<std::pair<uint32_t, uint32_t>,
				  SyncTime_struct*>() ) ).first;
	 }			 

	 // update timestamps

	 std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*>::iterator 
	    iter_timestamps = (iter_map_timestamps->second)->find(key);

	 if( iter_timestamps != (iter_map_timestamps->second)->end() )
	 {
	    delete(iter_timestamps->second);
	    (iter_map_timestamps->second)->erase(iter_timestamps);
	 }

	 iter_timestamps = (iter_map_timestamps->second)->insert(
			      std::make_pair( key, new SyncTime_struct(sync_time) ) ).first;

	 // set number of processes
	 m_uProcNum = VT_MAX(m_uProcNum, key.second + 1);
      }
   }

   m_vecRoundNum.assign( g_vecUnifyCtls.size(), 0 );
}

Synchronization::~Synchronization()
{
   std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>,
      SyncTime_struct*>*>::iterator iter_phase_time_stamps;
    
   // clean up timestamp map
   for( iter_phase_time_stamps = m_mapSyncPhasemapProcessIdsTimestamps.begin();
	iter_phase_time_stamps != m_mapSyncPhasemapProcessIdsTimestamps.end();
	++iter_phase_time_stamps )
   {
      std::map<std::pair<uint32_t, uint32_t>,
	 SyncTime_struct*>::iterator iter_time_stamps;

      // delete timestamps
      for( iter_time_stamps  = (iter_phase_time_stamps->second)->begin() ;
	   iter_time_stamps != (iter_phase_time_stamps->second)->end() ;
	   ++iter_time_stamps )
      {
	 if( iter_time_stamps->second != NULL )
	    delete(iter_time_stamps->second);
      }
      (iter_phase_time_stamps->second)->clear();
      delete(iter_phase_time_stamps->second);
   }

   // clean up timer paramaters map
   std::map<uint32_t, std::vector<SyncParam_struct*>*>::iterator iter_param;
   
   for( iter_param  = m_mapIdxvecSyncParam.begin();
	iter_param != m_mapIdxvecSyncParam.end();
	++iter_param )
   {
      // delete timer parameter
      for( uint32_t i = 0; i < (iter_param->second)->size(); i++ )
      {
	 if( ( *(iter_param->second))[i] != NULL )
	    delete( ( *(iter_param->second))[i] );
      }
      (iter_param->second)->clear();
      delete(iter_param->second);
   }
   
   m_mapSyncPhasemapProcessIdsTimestamps.clear();
   m_mapIdxvecSyncParam.clear();
   m_mapIdxMinStartTime.clear();
   m_vecSyncPreCorrection.clear();
}

bool Synchronization::run()
{
  VPrint( 1, "Initializing enhanced time synchronization\n" );

  bool error = false;

#ifdef VT_MPI
  if( g_iMPISize > 1 )
     VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );
#endif // VT_MPI

  PVPrint( 2, " Calculating timer parameters\n" );

  std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*>
     first_time_stamps, last_time_stamps;

  first_time_stamps =
     *((m_mapSyncPhasemapProcessIdsTimestamps.find(0))->second);

  last_time_stamps  =
     *((m_mapSyncPhasemapProcessIdsTimestamps.find(m_uRoundMax-1))->second);
  
  // presynchronization step 
  error = !calcSync(0, first_time_stamps, last_time_stamps);

  // calculate global time for each phase 
  for( uint32_t i = 1; i < m_uRoundMax && !error; i++ )
  {
     first_time_stamps =
	*((m_mapSyncPhasemapProcessIdsTimestamps.find(i-1))->second);
     last_time_stamps  =
	*((m_mapSyncPhasemapProcessIdsTimestamps.find(i))->second);
     error = !calcSync(i, first_time_stamps, last_time_stamps);
  }

  return !error;
}

void
Synchronization::setMinStartTimeForStreamId( uint32_t streamId, uint64_t minStartTime )
{
   m_mapIdxMinStartTime.insert( std::make_pair( streamId, minStartTime ) );
}

uint64_t
Synchronization::getMinStartTimeForStreamId( uint32_t streamId )
{
  std::map<uint32_t, uint64_t>::iterator it =
     m_mapIdxMinStartTime.find( streamId );
  assert( it != m_mapIdxMinStartTime.end() );

  return it->second;
}

bool
Synchronization::updateSyncParam( uint32_t procId )
{
  bool error = false;

  std::map<uint32_t, uint32_t>::iterator it = 
    g_mapStreamIdUnifyCtlIdx.find( procId );
  assert( it != g_mapStreamIdUnifyCtlIdx.end() );
  assert( it->second < g_vecUnifyCtls.size() );
   
  // get the synchronization reference process id (map stream id) for procId
  std::vector<SyncPhase_struct> * p_vec_sync_phases = g_vecUnifyCtls[it->second]->p_vec_sync_phases;
  SyncPhase_struct phase = (*p_vec_sync_phases)[m_vecRoundNum[it->second]];

  if( m_vecRoundNum[it->second] >= 1 && m_vecRoundNum[it->second] < m_uRoundMax - 1 )
  {
     std::map<uint32_t, std::vector<SyncParam_struct*>*>::iterator iter =
	m_mapIdxvecSyncParam.find( phase.mapid );
     
     g_vecUnifyCtls[it->second]->sync_offset = (*(iter->second))[m_vecRoundNum[it->second]+1]->offset;
     g_vecUnifyCtls[it->second]->sync_drift  = (*(iter->second))[m_vecRoundNum[it->second]+1]->drift;
  }

  m_vecRoundNum[it->second]++;
   
  return !error;
}

// private methods
//

bool
Synchronization::calcSync( uint32_t round, std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*> &first_time_stamps, 
			   std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*> &last_time_stamps )
{
  bool error = false;

  std::cout.precision(20);
  CLAPACK_DOUBLE alpha, beta;
  CLAPACK_INT k,l,m,n;
  CLAPACK_INT lda,ldb,ldc,lwork,nrhs;
  CLAPACK_INT info;
  CLAPACK_DOUBLE* p_work;
  uint32_t id1, id2;
  char trans[2] = "N";
  
  k=0; l=0; m=0, n=0;

  if( m_vecSyncPreCorrection.size() == 0 ) 
     m_vecSyncPreCorrection.assign( m_uProcNum, 1.0 );

  // system R*X2=S (drift) 
  int dim_x_1 = 4*first_time_stamps.size();
  int dim_y_1 = m_uProcNum;
  
  double *p_dmatrix_r = (double *) calloc( dim_x_1 * dim_y_1, sizeof(double) );
  double *p_dmatrix_s = (double *) calloc( dim_x_1, sizeof(double) );
  
  memset( (void*) p_dmatrix_r, 0, dim_x_1 * dim_y_1 * sizeof(double) );
  memset( (void*) p_dmatrix_s, 0, dim_x_1 * sizeof(double) );


  // system G*X1=D D=D+H*X2(offset)
  int dim_x_2 = 3*first_time_stamps.size();
  int dim_y_2 = m_uProcNum;

  double *p_dmatrix_g = (double *) calloc( dim_x_2 * dim_y_2, sizeof(double) );
  double *p_dmatrix_h = (double *) calloc( dim_x_2 * dim_y_2, sizeof(double) );
  double *p_dmatrix_d = (double *) calloc( dim_x_2, sizeof(double) );

  memset( (void*) p_dmatrix_g, 0, dim_x_2 * dim_y_2 * sizeof(double) );
  memset( (void*) p_dmatrix_h, 0, dim_x_2 * dim_y_2 * sizeof(double) );
  memset( (void*) p_dmatrix_d, 0, dim_x_2 * sizeof(double) );


  // solution matrizes

  double *p_dmatrix_x  = (double *) calloc( dim_y_1+dim_y_2, sizeof(double) );
  double *p_dmatrix_x1 = (double *) calloc( dim_y_2, sizeof(double) );
  double *p_dmatrix_x2 = (double *) calloc( dim_y_1, sizeof(double) );

  memset( (void*) p_dmatrix_x , 0, ( dim_y_1 + dim_y_2 ) * sizeof(double) );
  memset( (void*) p_dmatrix_x1, 0, dim_y_2 * sizeof(double) );
  memset( (void*) p_dmatrix_x2, 0, dim_y_1 * sizeof(double) );

#if (defined (__DEBUG))
  int dim_x_3 = 12*first_time_stamps.size();
  int dim_y_3 = 2 * m_uProcNum;

  double *p_dmatrix_a = (double *) calloc( dim_x_3*dim_y_3 , sizeof(double) );
  double *p_dmatrix_y = (double *) calloc( dim_x_3, sizeof(double) );
  memset( (void*) p_dmatrix_a, 0, dim_x_3*dim_y_3*sizeof(double) );
  memset( (void*) p_dmatrix_y, 0, dim_x_3*sizeof(double) );
#endif

  // offset vector

  int64_t *p_imatrix_offset = (int64_t*) calloc( dim_y_2, sizeof(int64_t) );

  memset( (void*) p_imatrix_offset, 0, dim_y_2 * sizeof(int64_t) );

  std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*>::iterator iter;
  std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*>::iterator iter2;

  // insert synchronization timestamps into the matrices

  for( iter = first_time_stamps.begin(); iter != first_time_stamps.end(); ++iter )
  {
     id1 = (iter->first).first;
     id2 = (iter->first).second;

     SyncTime_struct * p_data = (iter->second);
     SyncTime_struct * p_data2;
     iter2 = last_time_stamps.find(iter->first);

     assert( iter2 != last_time_stamps.end() );

     p_data2=iter2->second;

#if (defined (__DEBUG))	    
	    
     p_dmatrix_a[ m + id1*dim_x_3 ] = 1.0;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data->t[0];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3 ] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data->t[3];
     m++;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data->t[1];
     m++;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data->t[2];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3] = (double) -1;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = -(double)p_data->t[0];
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data->t[1];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) -1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data->t[3];
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = -(double)p_data->t[2];
     m++;

     p_dmatrix_a[ m + id1*dim_x_3 ] = 1.0;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data2->t[0];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3 ] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data2->t[3];
     m++;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data2->t[1];
     m++;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data2->t[2];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3] = (double) -1;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = -(double)p_data2->t[0];
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = (double)p_data2->t[1];
     m++;
     p_dmatrix_a[ m + id1*dim_x_3] = (double) 1;
     p_dmatrix_a[ m + id2*dim_x_3] = (double) -1;
     p_dmatrix_a[ m + (m_uProcNum + id1)*dim_x_3 ] = (double)p_data2->t[3];
     p_dmatrix_a[ m + (m_uProcNum + id2)*dim_x_3 ] = -(double)p_data2->t[2];
     m++;
#endif
     
     double delta_offset = (double)( (int64_t)( (p_data2->t[2] - p_data->t[2]) + 
						(p_data2->t[1] - p_data->t[1]) ) -
				     (int64_t)( (p_data2->t[0] - p_data->t[0]) + 
						(p_data2->t[3] - p_data->t[3]) ) ) / 2;
     
#if (defined(__DEBUG))
     std::cout << " Delta - Offset " << id1 << " " << id2 << " :"<< delta_offset << std::endl;
#endif

     // equation (1) (k-r)
     p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[0] - p_data->t[0] ) * 
					   m_vecSyncPreCorrection[id1] );
     p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[1] ) * 
					   m_vecSyncPreCorrection[id2] * (double)(-1.0) );
     p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[0] - p_data->t[0] ) - 
						     (int64_t) ( p_data2->t[1] - p_data->t[1] ) )
					   + delta_offset );
     k++;

     // equation (2) (k-s)
     
     p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[3] - p_data->t[0] ) *
					   m_vecSyncPreCorrection[id1] );
     p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[2] - p_data->t[1] ) * 
					   m_vecSyncPreCorrection[id2] * (double)(-1.0) );
     p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[3] - p_data->t[0] ) - 
						     (int64_t) ( p_data2->t[2] - p_data->t[1] ) )
					   + delta_offset );
     
     k++;
     
     // equation (3) (l-r)
     
     p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[0] - p_data->t[3] ) * 
					   m_vecSyncPreCorrection[id1] );
     p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[2] ) * 
					   m_vecSyncPreCorrection[id2] * (double)(-1.0) );
     p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[0] - p_data->t[3] ) - 
						     (int64_t) ( p_data2->t[1] - p_data->t[2] ) )
					   + delta_offset );
     
     k++;
     
     // equation (4) (l-s)
     
     p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[3] - p_data->t[3] ) * 
					   m_vecSyncPreCorrection[id1] );
     p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[1] ) *
					   m_vecSyncPreCorrection[id2] * (double)(-1.0) );
     p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[3] - p_data->t[3]) - 
						     (int64_t) ( p_data2->t[1] - p_data->t[1]) )
					   + delta_offset );
     
     k++;
     
  }
  
  // ***** drift calculation *****
  
  // solve system of linear equations R*X2=S

  m    = dim_x_1;
  n    = dim_y_1; 
  nrhs = 1;
  lda  = dim_x_1;
  ldb  = dim_x_1;
  lwork= 2*m*n;
  p_work = new double[lwork];

  dgels_( trans, &m, &n, &nrhs, p_dmatrix_r, &lda, p_dmatrix_s, &ldb, 
	  p_work, &lwork, &info );

  if ( info < 0 )
  {
     std::cerr << " Error: Lapack routine dgels returns error in round "
	       << round << std::endl;
     return error;
  }

  delete [] p_work;

  // normalization of drift vector

  double sum = 0.0;
  for( uint32_t i = 0; i < m_uProcNum; i++ )
      sum += p_dmatrix_s[i] * m_vecSyncPreCorrection[i];
  
  double normalize = 0.0;
  normalize = 1 - sum / m_uProcNum;

  for( uint32_t i = 0; i < m_uProcNum; i++ )
     p_dmatrix_x2 [i] = p_dmatrix_s[i] * m_vecSyncPreCorrection[i] + normalize;	    

  if ( round == 0 )
  { 
     m_vecSyncPreCorrection.clear();
     for ( uint32_t i = 0; i < m_uProcNum; i++ )
	m_vecSyncPreCorrection.push_back( p_dmatrix_x2[i] );
  }

  // ***** offset calculation *****

  if( round > 0)
  {
     for( iter = first_time_stamps.begin(); iter != first_time_stamps.end(); ++iter )
     {
	id1 = (iter->first).first;
	id2 = (iter->first).second;

	// insert data into the matrix

	SyncTime_struct *p_data = (iter->second);

/*	    
	double offset = ( (double) ( p_data->t[1] + p_data->t[2]) - 
			  (double) ( p_data->t[3] + p_data->t[0]) )/2.0;
*/
	int64_t offset = ( (int64_t)( p_data->t[1] - p_data->t[0] ) +
			   (int64_t)( p_data->t[2] - p_data->t[3] ) )/2;
	    
	// equation offset (1)
	  
	p_dmatrix_g [ l + id1*dim_x_2 ] = -1.0;	
	p_dmatrix_g [ l + id2*dim_x_2 ] = 1.0;
	p_dmatrix_d [ l ]               = (double)( (int64_t)p_data->t[1] - 
						    (int64_t)p_data->t[0] - 
						    offset );  
	p_dmatrix_h [ l + id1*dim_x_2 ] =  (double)p_data->t[0];
	p_dmatrix_h [ l + id2*dim_x_2 ] = -(double)p_data->t[1];
	
	l++;
	
	// equation offset (2)
	
	p_dmatrix_g [ l + id1*dim_x_2 ] = 1.0;	
	p_dmatrix_g [ l + id2*dim_x_2 ] = -1.0;
	p_dmatrix_d [ l ]               = (double)( (int64_t)p_data->t[3] - 
						    (int64_t)p_data->t[2] + 
						    offset );
	p_dmatrix_h [ l + id1*dim_x_2 ] = -(double)p_data->t[3];
	p_dmatrix_h [ l + id2*dim_x_2 ] =  (double)p_data->t[2];
	
	l++; 
	
	// equation offset (3)
	
	p_dmatrix_g [ l + id1*dim_x_2 ] = 2.0;
	p_dmatrix_g [ l + id2*dim_x_2 ] = -2.0;
	p_dmatrix_h [ l + id1*dim_x_2 ] = -(double)(p_data->t[0]+p_data->t[3]);
	p_dmatrix_h [ l + id2*dim_x_2 ] = (double)(p_data->t[1]+p_data->t[2]);
	
	l++;
     }
  }
  
  // system G*X1=D D=D+H*X2
  // solve equation D=D+H*X2
  m = dim_x_2;
  n = 1;
  k = dim_y_2;
  alpha = 1.0;
  lda = dim_x_2;
  ldb = dim_y_2;
  beta = 1.0;
  ldc = dim_x_2;
  
  //  print(p_dmatrix_d, dim_x_2,1, "matrix d: ");
  //  print(p_dmatrix_h, dim_x_2,dim_y_2, "matrix h: ");

  dgemm_( trans, trans, &m, &n, &k, &alpha, p_dmatrix_h, &lda, p_dmatrix_x2, &ldb,
	  &beta, p_dmatrix_d, &ldc );
 
  //  print(p_dmatrix_d, dim_x_2, 1, "matrix d: ");
  //  print(p_dmatrix_g, dim_x_2,dim_y_2, "matrix g: ");

  // solve linear system G*X1=D

  m    = dim_x_2;
  n    = dim_y_2; 
  nrhs = 1;
  lda  = dim_x_2;
  ldb  = dim_x_2;
  lwork= 2*m*n;
  p_work = new double[lwork];

  dgels_( trans, &m, &n, &nrhs, p_dmatrix_g, &lda, p_dmatrix_d, &ldb, 
	  p_work, &lwork, &info );


  //  print(p_dmatrix_d, dim_x_2, 1, "matrix d:");

  if ( info < 0 )
    {
      std::cerr << " Error: Lapack routine dgels returns error in round " << round << std::endl;
      return error;
    }

  delete [] p_work;
   
  double min = p_dmatrix_d[0];
  for( uint32_t i=1; i< m_uProcNum; i++)
    {
      min = VT_MIN( min, p_dmatrix_d[i] );
    }
  
  for( uint32_t i=0; i< m_uProcNum; i++)
    {
      p_dmatrix_x1 [ i ] = p_dmatrix_d [ i ] - min;
    }

  for( uint32_t i=0; i< m_uProcNum; i++)
    {
      p_dmatrix_x [ i ] = p_dmatrix_x1 [ i ];
      p_dmatrix_x [ m_uProcNum + i ] = p_dmatrix_x2[ i ];
    }

  int64_t offset_corr = 0;

  if( round == 1 )
    {
      // determine the best offset correction to garantuee always time > 0
      for( uint32_t i=0; i< m_uProcNum; i++)
	{
	  std::map<uint32_t, uint32_t>::iterator it = 
	    g_mapStreamIdUnifyCtlIdx.find(i+1);
	  assert( it != g_mapStreamIdUnifyCtlIdx.end() );
	  assert( it->second < g_vecUnifyCtls.size() );
	  
	  // get the synchronization reference process id for streamid
	  std::vector<SyncPhase_struct> * p_vec_sync_phases = g_vecUnifyCtls[it->second]->p_vec_sync_phases;
	  SyncPhase_struct act_phase = ((*p_vec_sync_phases)[round-1]);	  
	  
	  uint32_t mapid = act_phase.mapid;

	  // offset + offset_corr + drift * starttime = 0
	  int64_t corr = (int64_t)( -p_dmatrix_x[ mapid-1 ] ) - (int64_t)( p_dmatrix_x[ mapid - 1 + m_uProcNum ] * (double)( m_mapIdxMinStartTime.find(mapid) )->second );

	  if ( i==0 ) offset_corr = corr;
	    
	  if ( corr > offset_corr ) offset_corr = corr;
	  
	}

      // correct offset, to garantuee time > 0 over all synchronization processes
      for( uint32_t i = 0; i < m_uProcNum; i++)
	{
	  p_imatrix_offset[ i ] = (int64_t)p_dmatrix_x[ i ] + offset_corr;
	  p_dmatrix_x[ i ] = (double) p_imatrix_offset[ i ];
	}
    }

#if (defined (__DEBUG))
  m = dim_x_3;
  n = 1;
  k = dim_y_3;
  alpha = 1.0;
  lda = dim_x_3;
  ldb = dim_y_3;
  beta = 0.0;
  ldc = dim_x_3;

  print( p_dmatrix_a, dim_x_3,dim_y_3,"matrix a:" );

  dgemm_( "N", "N", &m, &n, &k, &alpha, p_dmatrix_a, &lda, p_dmatrix_x, &ldb,
	  &beta, p_dmatrix_y, &ldc );

  print( p_dmatrix_x, 2*dim_y_2,1, "matrix x:" );
  print( p_dmatrix_y, dim_x_3,1, "matrix y:" );
  
#endif 

  if( round > 1)
  {
     // determine an approximate duration time for the synchronization phase
     // might be inaccurate, because of overhead in the synchronization phase
     // while using only the measurement data for approximation
     
     for( uint32_t i = 0; i < m_uProcNum; i++ )
     { 
	std::map<uint32_t, uint32_t>::iterator it = 
	   g_mapStreamIdUnifyCtlIdx.find(i+1);
	assert( it != g_mapStreamIdUnifyCtlIdx.end() );
	assert( it->second < g_vecUnifyCtls.size() );
	
	// get the synchronization reference process id for streamid
	std::vector<SyncPhase_struct> * p_vec_sync_phases = g_vecUnifyCtls[it->second]->p_vec_sync_phases;
	SyncPhase_struct last_phase = ((*p_vec_sync_phases)[round-1]);
	
	uint32_t mapid = last_phase.mapid;

	// determine offset correction for a monotone increasing time function
	std::map<uint32_t, std::vector<SyncParam_struct*>*>::iterator iter_param =
	   m_mapIdxvecSyncParam.find(mapid);

	SyncParam_struct * p_param = (*(iter_param->second))[round-1];

	uint64_t time1 = (uint64_t)( p_param->offset + (int64_t)( p_param->drift * (double) last_phase.time ) );
	uint64_t time2 = time1 + (uint64_t)( (double) last_phase.duration * 
					     ( p_param->drift + p_dmatrix_x[ m_uProcNum+i ] ) / 2.0 );

	// offset + offsetcorr + drift_act( lastphase->time + duration ) = time2
	offset_corr =  ( time2 - (int64_t)p_dmatrix_x[ i ] - 
			 (int64_t)( (double)( last_phase.time + last_phase.duration ) * 
				    p_dmatrix_x[ m_uProcNum + i ] ) );	  

	p_imatrix_offset[ i ] = (int64_t) p_dmatrix_x[ i ] + offset_corr;
     }
  }

  // insert correction parameters for time synchronization into global map
  for( uint32_t i=0; i < m_uProcNum; i++ )
  {
     SyncParam_struct *data = 
	new SyncParam_struct( p_imatrix_offset[ i ],
			      p_dmatrix_x[ i + m_uProcNum ] );
      
     std::map<uint32_t, std::vector<SyncParam_struct*>*>:: iterator it_param =
	m_mapIdxvecSyncParam.find(i+1);
      
     if( it_param == m_mapIdxvecSyncParam.end() )
     {
	it_param = m_mapIdxvecSyncParam.insert(std::make_pair( i+1, new std::vector<SyncParam_struct*>() )).first;
     }
     (it_param->second)->push_back( data );  
  }

  if( round == 1 )
  { 
     // initial update of synchronization parameters overall processes
     for( uint32_t i=0; i < g_vecUnifyCtls.size(); i++ )
     {
	// get the synchronization reference process id for i
	std::vector<SyncPhase_struct> * p_vec_sync_phases = g_vecUnifyCtls[i]->p_vec_sync_phases;
	SyncPhase_struct phase = (*p_vec_sync_phases)[0];
	uint32_t mapid = phase.mapid;

	std::map<uint32_t, std::vector<SyncParam_struct*>*>:: iterator iter =
	   m_mapIdxvecSyncParam.find(mapid);
	g_vecUnifyCtls[i]->sync_offset = (*(iter->second))[round]->offset;
	g_vecUnifyCtls[i]->sync_drift = (*(iter->second))[round]->drift;
     }
  }

  // remove matrices
  free(p_dmatrix_r); 
  free(p_dmatrix_s);
  free(p_dmatrix_g);
  free(p_dmatrix_h);
  free(p_dmatrix_d);
  free(p_dmatrix_x);
  free(p_dmatrix_x1);
  free(p_dmatrix_x2);
  free(p_imatrix_offset);
  
  return !error;
}

void Synchronization::print(double *a, int m, int n, char* info)
{
  std::cout << info << std::endl;
  for ( int i= 0; i < m; i++ )
    {
      for ( int j = 0; j < n; j++)
	std::cout << a[ i + j*m ] << " ";
      std::cout << std::endl;
    }
}
