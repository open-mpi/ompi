// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//


#include "mpicxx.h"
#include <stdio.h>

extern "C" 
void ompi_mpi_cxx_throw_excptn_fctn(MPI_Comm *, int *errcode, ...)
{
#if OMPI_HAVE_CXX_EXCEPTION_SUPPORT
    throw(MPI::Exception(*errcode));
#else
  // Ick.  This is really ugly, but necesary if someone uses a C compiler
  // and -lmpi++ (which can legally happen in the LAM MPI implementation,
  // and probably in MPICH and others who include -lmpi++ by default in their
  // wrapper compilers)
  fprintf(stderr, "MPI 2 C++ exception throwing is disabled, MPI::mpi_errno has the error code\n"); 
  MPI::mpi_errno = *errcode;
#endif  
}

MPI::Comm::mpi_comm_map_t MPI::Comm::mpi_comm_map;
MPI::Comm::mpi_err_map_t MPI::Comm::mpi_err_map;
MPI::Comm::key_fn_map_t MPI::Comm::key_fn_map;

ompi_mutex_t *MPI::Comm::mpi_comm_map_mutex = NULL;
ompi_mutex_t *MPI::Comm::mpi_err_map_mutex = NULL;
ompi_mutex_t *MPI::Comm::key_fn_map_mutex = NULL;

extern "C"
void ompi_mpi_cxx_errhandler_intercept(MPI_Comm *mpi_comm, int *err, ...)
{
  MPI::Comm* comm = MPI::Comm::mpi_err_map[*mpi_comm];
  if (comm && comm->my_errhandler) {
    va_list ap;
    va_start(ap, err);
    comm->my_errhandler->handler_fn(*comm, err, ap);
    va_end(ap);
  }
}

MPI::Op* MPI::Intracomm::current_op;

extern "C" void
ompi_mpi_cxx_op_intercept(void *invec, void *outvec, int *len, 
                          MPI_Datatype *datatype)
{
  MPI::Op* op = MPI::Intracomm::current_op;
  MPI::Datatype thedata = *datatype;
  ((MPI::User_function*)op->op_user_function)(invec, outvec, *len, thedata);
  //JGS the above cast is a bit of a hack, I'll explain:
  //  the type for the PMPI::Op::op_user_function is PMPI::User_function
  //  but what it really stores is the user's MPI::User_function supplied when
  //  the user did an Op::Init. We need to cast the function pointer back to
  //  the MPI::User_function. The reason the PMPI::Op::op_user_function was
  //  not declared a MPI::User_function instead of a PMPI::User_function is
  //  that without namespaces we cannot do forward declarations.
  //  Anyway, without the cast the code breaks on HP LAM with the aCC compiler.
}

extern "C" int
ompi_mpi_cxx_copy_attr_intercept(MPI_Comm oldcomm, int keyval, 
                                 void *extra_state, void *attribute_val_in, 
                                 void *attribute_val_out, int *flag)
{
  int ret = 0;
  MPI::Comm::key_pair_t* copy_and_delete = 
    MPI::Comm::key_fn_map[keyval];
  MPI::Comm::Copy_attr_function* copy_fn;
  copy_fn = copy_and_delete->first;

  MPI::Comm::comm_pair_t *comm_type = 
    MPI::Comm::mpi_comm_map[oldcomm];
  
  // Just in case...

  if (comm_type == 0)
    return MPI::ERR_OTHER;

  MPI::Intracomm intracomm;
  MPI::Intercomm intercomm;
  MPI::Graphcomm graphcomm;
  MPI::Cartcomm cartcomm;
  
  int thetype = (int)comm_type->second;
  bool bflag = (bool)*flag; 

  switch (thetype) {
  case eIntracomm:
    intracomm = MPI::Intracomm(*comm_type->first);
    ret = copy_fn(intracomm, keyval, extra_state,
		  attribute_val_in, attribute_val_out, bflag);
    break;
  case eIntercomm:
    intercomm = MPI::Intercomm(*comm_type->first);
    ret = copy_fn(intercomm, keyval, extra_state,
		  attribute_val_in, attribute_val_out, bflag);
    break;
  case eGraphcomm:
    graphcomm = MPI::Graphcomm(*comm_type->first);
    ret = copy_fn(graphcomm, keyval, extra_state,
		  attribute_val_in, attribute_val_out, bflag);
    break;
  case eCartcomm:
    cartcomm = MPI::Cartcomm(*comm_type->first);
    ret = copy_fn(cartcomm, keyval, extra_state,
		  attribute_val_in, attribute_val_out, bflag);
    break;
  }

  *flag = (int)bflag;
  return ret;
}

extern "C" int
ompi_mpi_cxx_delete_attr_intercept(MPI_Comm comm, int keyval, 
                                   void *attribute_val, void *extra_state)
{
  int ret = 0;

  MPI::Comm::key_pair_t *copy_and_delete = 
    MPI::Comm::key_fn_map[keyval];

  MPI::Comm::Delete_attr_function* delete_fn;  
  delete_fn = copy_and_delete->second;

  MPI::Comm::comm_pair_t *comm_type = 
    MPI::Comm::mpi_comm_map[comm];

  // Just in case...

  if (comm_type == 0)
    return MPI::ERR_OTHER;

  MPI::Intracomm intracomm;
  MPI::Intercomm intercomm;
  MPI::Graphcomm graphcomm;
  MPI::Cartcomm cartcomm;
  
  int thetype = (long)(comm_type->second);

  if (delete_fn > (MPI::Comm::Delete_attr_function*) 100) {
    switch (thetype) {
    case eIntracomm:
      intracomm = MPI::Intracomm(*comm_type->first);
      ret = delete_fn(intracomm, keyval, attribute_val, extra_state);
      break;
    case eIntercomm:
      intercomm = MPI::Intercomm(*comm_type->first);
      ret = delete_fn(intercomm, keyval, attribute_val, extra_state);
      break;
    case eGraphcomm:
      graphcomm = MPI::Graphcomm(*comm_type->first);
      ret = delete_fn(graphcomm, keyval, attribute_val, extra_state);
      break;
    case eCartcomm:
      cartcomm = MPI::Cartcomm(*comm_type->first);
      ret = delete_fn(cartcomm, keyval, attribute_val, extra_state);
      break;
    }
  } else 
    ret = MPI::ERR_OTHER;
  return ret; 
}

