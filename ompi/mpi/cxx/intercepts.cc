// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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
    /* Portland compiler raises a warning if va_start is not used in a
     * variable argument function */
    va_list ap;
    va_start(ap, errcode);
    va_end(ap);
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

opal_mutex_t *MPI::Comm::mpi_comm_map_mutex = NULL;
opal_mutex_t *MPI::Comm::mpi_err_map_mutex = NULL;
opal_mutex_t *MPI::Comm::key_fn_map_mutex = NULL;

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

// This is a bit weird; bear with me.  The user-supplied function for
// MPI::Op contains a C++ object reference.  So it must be called from
// a C++-compiled function.  However, libmpi does not contain any C++
// code because there are portability and bootstrapping issues
// involved if someone tries to make a 100% C application link against
// a libmpi that contains C++ code.  At a minimum, the user will have
// to use the C++ compiler to link.  LA-MPI has shown that users don't
// want to do this (there are other problems, but this one is easy to
// cite).
//
// Hence, there are two problems when trying to invoke the user's
// callback funcion from an MPI::Op:
//
// 1. The MPI_Datatype that the C library has must be converted to an
// (MPI::Datatype)
// 2. The C++ callback function must then be called with a
// (MPI::Datatype&)
//
// Some relevant facts for the discussion:
//
// - The main engine for invoking Op callback functions is in libmpi
// (i.e., in C code).
//
// - The C++ bindings are a thin layer on top of the C bindings.
//
// - The C++ bindings are a separate library from the C bindings
// (libmpi_cxx.la).
//
// - As a direct result, the mpiCC wrapper compiler must generate a
// link order thus: "... -lmpi_cxx -lmpi ...", meaning that we cannot
// have a direct function call from the libmpi to libmpi_cxx.  We can
// only do it by function pointer.
//
// So the problem remains -- how to invoke a C++ MPI::Op callback
// function (which only occurrs for user-defined datatypes, BTW) from
// within the C Op callback engine in libmpi?
//
// It is easy to cache a function pointer to the
// ompi_mpi_cxx_op_intercept() function on the MPI_Op (that is located
// in the libmpi_cxx library, and is therefore compiled with a C++
// compiler).  But the normal C callback MPI_User_function type
// signature is (void*, void*, int*, MPI_Datatype*) -- so if
// ompi_mpi_cxx_op_intercept() is invoked with these arguments, it has
// no way to deduce what the user-specified callback function is that
// is associated with the MPI::Op.
//
// One can easily imagine a scenario of caching the callback pointer
// of the current MPI::Op in a global variable somewhere, and when
// ompi_mpi_cxx_op_intercept() is invoked, simply use that global
// variable.  This is unfortunately not thread safe.
//
// So what we do is as follows:
//
// 1. The C++ dispatch function ompi_mpi_cxx_op_intercept() is *not*
// of type (MPI_User_function*).  More specifically, it takes an
// additional argument: a function pointer.  its signature is (void*,
// void*, int*, MPI_Datatype*, MPI_Op*, MPI::User_function*).  This
// last argument is the function pointer of the user callback function
// to be invoked.
//
// The careful reader will notice that it is impossible for the C Op
// dispatch code in libmpi to call this function properly because the
// last argument is of a type that is not defined in libmpi (i.e.,
// it's only in libmpi_cxx).  Keep reading -- this is explained below.
//
// 2. When the MPI::Op is created (in MPI::Op::Init()), we call the
// back-end C MPI_Op_create() function as normal (just like the F77
// bindings, in fact), and pass it the ompi_mpi_cxx_op_intercept()
// function (casting it to (MPI_User_function*) -- it's a function
// pointer, so its size is guaranteed to be the same, even if the
// signature of the real function is different).  
//
// 3. The function pointer to ompi_mpi_cxx_op_intercept() will be
// cached in the MPI_Op in op->o_func[0].cxx_intercept_fn.  
//
// Recall that MPI_Op is implemented to have an array of function
// pointers so that optimized versions of reduction operations can be
// invoked based on the corresponding datatype.  But when an MPI_Op
// represents a user-defined function operation, there is only one
// function, so it is always stored in function pointer array index 0.
//
// 4. When MPI_Op_create() returns, the C++ MPI::Op::Init function
// manually sets OMPI_OP_FLAGS_CXX_FUNC flag on the resulting MPI_Op
// (again, very similar to the F77 MPI_OP_CREATE wrapper).  It also
// caches the user's C++ callback function in op->o_func[1].c_fn
// (recall that the array of function pointers is actually a union of
// multiple different function pointer types -- it doesn't matter
// which type the user's callback function pointer is stored in; since
// all the types in the union are function pointers, it's guaranteed
// to be large enough to hold what we need.  
//
// Note that we don't have a member of the union for the C++ callback
// function because its signature includes a (MPI::Datatype&), which
// we can't put in the C library libmpi.
//
// 5. When the user invokes an function that uses the MPI::Op (or,
// more specifically, when the Op dispatch engine in ompi/op/op.c [in
// libmpi] tries to dispatch off to it), it will see the
// OMPI_OP_FLAGS_CXX_FUNC flag and know to use the
// op->o_func[0].cxx_intercept_fn and also pass as the 4th argument,
// op->o_func[1].c_fn.
//
// 6. ompi_mpi_cxx_op_intercept() is therefore invoked and receives
// both the (MPI_Datatype*) (which is easy to convert to
// (MPI::Datatype&)) and a pointer to the user's C++ callback function
// (albiet cast as the wrong type).  So it casts the callback function
// pointer to (MPI::User_function*) and invokes it.
//
// Wasn't that simple?
//
extern "C" void
ompi_mpi_cxx_op_intercept(void *invec, void *outvec, int *len, 
                          MPI_Datatype *datatype, MPI_User_function *c_fn)
{
    MPI::Datatype cxx_datatype = *datatype;
    MPI::User_function *cxx_callback = (MPI::User_function*) c_fn;
    cxx_callback(invec, outvec, *len, cxx_datatype);
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
  bool bflag = OPAL_INT_TO_BOOL(*flag); 

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

// For similar reasons as above, we need to intercept calls for the 3
// generalized request callbacks (convert arguments to C++ types and
// invoke the C++ callback signature).

extern "C" int
ompi_mpi_cxx_grequest_query_fn_intercept(void *state, MPI_Status *status)
{
    struct MPI::Grequest_intercept_t *data = 
        (struct MPI::Grequest_intercept_t *) state;

    MPI::Status s(*status);
    int ret = data->git_cxx_query_fn(data->git_extra, s);
    *status = s;
    return ret;
}

extern "C" int
ompi_mpi_cxx_grequest_free_fn_intercept(void *state)
{
    struct MPI::Grequest_intercept_t *data = 
        (struct MPI::Grequest_intercept_t *) state;
    int ret = data->git_cxx_free_fn(data->git_extra);
    // Delete the struct that was "new"ed in MPI::Grequest::Start()
    delete data;
    return ret;
}

extern "C" int
ompi_mpi_cxx_grequest_cancel_fn_intercept(void *state, int cancelled)
{
    struct MPI::Grequest_intercept_t *data = 
        (struct MPI::Grequest_intercept_t *) state;
    return data->git_cxx_cancel_fn(data->git_extra, (bool) cancelled);
}
