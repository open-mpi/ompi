// -*- c++ -*-
// 
// $HEADER$
//

#ifndef MPIPP_H
#define MPIPP_H

// 
// Let's ensure that we're really in C++, and some errant programmer
// hasn't included <mpicxx.h> just "for completeness"
//

#if defined(__cplusplus) || defined(c_plusplus) 

#include "ompi_config.h"
#include <mpi.h>

// we include all this here so that we escape the silly namespacing issues
#include <map>
#include <utility>

#include <stdarg.h>

// forward declare so that we can still do inlining
struct ompi_mutex_t;

//JGS: this is used for implementing user functions for MPI::Op
extern "C" void
op_intercept(void *invec, void *outvec, int *len, MPI_Datatype *datatype);

//JGS: this is used as the MPI_Handler_function for
// the mpi_errhandler in ERRORS_THROW_EXCEPTIONS
extern "C" void
throw_excptn_fctn(MPI_Comm* comm, int* errcode, ...);

extern "C" void
errhandler_intercept(MPI_Comm * mpi_comm, int * err, ...);


//used for attr intercept functions
enum CommType { eIntracomm, eIntercomm, eCartcomm, eGraphcomm};

extern "C" int
copy_attr_intercept(MPI_Comm oldcomm, int keyval, 
		    void *extra_state, void *attribute_val_in, 
		    void *attribute_val_out, int *flag);

extern "C" int
delete_attr_intercept(MPI_Comm comm, int keyval, 
		      void *attribute_val, void *extra_state);


#if 0 /* OMPI_ENABLE_MPI_PROFILING */
#include "mpi/cxx/pmpicxx.h"
#endif

namespace MPI {

#if ! OMPI_HAVE_CXX_EXCEPTION_SUPPORT
	extern int mpi_errno;
#endif

  class Comm_Null;
  class Comm;
  class Intracomm;
  class Intercomm;
  class Graphcomm;
  class Cartcomm;
  class Datatype;
  class Errhandler;
  class Group;
  class Op;
  class Request;
  class Status;
  class Info;
  class Win;
  class File;

  typedef MPI_Aint Aint;
  typedef MPI_Offset Offset;

#include "mpi/cxx/constants.h"
#include "mpi/cxx/functions.h"
#include "mpi/cxx/datatype.h"

  typedef void User_function(const void* invec, void* inoutvec, int len,
			     const Datatype& datatype);

#include "mpi/cxx/exception.h"
#include "mpi/cxx/op.h"
#include "mpi/cxx/status.h"
#include "mpi/cxx/request.h"   //includes class Prequest
#include "mpi/cxx/group.h" 
#include "mpi/cxx/comm.h"
#include "mpi/cxx/errhandler.h"
#include "mpi/cxx/intracomm.h"
#include "mpi/cxx/topology.h"  //includes Cartcomm and Graphcomm
#include "mpi/cxx/intercomm.h"
#include "mpi/cxx/info.h"
#include "mpi/cxx/win.h"
#include "mpi/cxx/file.h"

}

#if 0 /* OMPI_ENABLE_MPI_PROFILING */
#include "mpi/cxx/pop_inln.h"
#include "mpi/cxx/pgroup_inln.h"
#include "mpi/cxx/pstatus_inln.h"
#include "mpi/cxx/prequest_inln.h"
#endif

//
// These are the "real" functions, whether prototyping is enabled
// or not. These functions are assigned to either the MPI::XXX class
// or the PMPI::XXX class based on the value of the macro MPI
// which is set in mpi2cxx_config.h.
// If prototyping is enabled, there is a top layer that calls these
// PMPI functions, and this top layer is in the XXX.cc files.
//

#include "mpi/cxx/datatype_inln.h"
#include "mpi/cxx/functions_inln.h"
#include "mpi/cxx/request_inln.h"
#include "mpi/cxx/comm_inln.h"
#include "mpi/cxx/intracomm_inln.h"
#include "mpi/cxx/topology_inln.h"
#include "mpi/cxx/intercomm_inln.h"
#include "mpi/cxx/group_inln.h"
#include "mpi/cxx/op_inln.h"
#include "mpi/cxx/errhandler_inln.h"
#include "mpi/cxx/status_inln.h"
#include "mpi/cxx/info_inln.h"
#include "mpi/cxx/win_inln.h"
#include "mpi/cxx/file_inln.h"

#endif // #if defined(__cplusplus) || defined(c_plusplus) 
#endif // #ifndef MPIPP_H_
