// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

#if 0 /* OMPI_ENABLE_MPI_PROFILING */

inline
MPI::Op::Op() { }
  
inline
MPI::Op::Op(const MPI::Op& o) : pmpi_op(o.pmpi_op) { }
  
inline
MPI::Op::Op(const MPI_Op& o) : pmpi_op(o) { }

inline
MPI::Op::~Op() { }

inline
MPI::Op& MPI::Op::operator=(const MPI::Op& op) {
  pmpi_op = op.pmpi_op; return *this;
}

// comparison
inline bool
MPI::Op::operator== (const MPI::Op &a) {
  return (bool)(pmpi_op == a.pmpi_op);
}

inline bool
MPI::Op::operator!= (const MPI::Op &a) {
  return (bool)!(*this == a);
}

// inter-language operability
inline MPI::Op&
MPI::Op::operator= (const MPI_Op &i) { pmpi_op = i; return *this; }

inline
MPI::Op::operator MPI_Op () const { return pmpi_op; }

//inline
//MPI::Op::operator MPI_Op* () { return pmpi_op; }


#else  // ============= NO PROFILING ===================================

// construction
inline
MPI::Op::Op() : mpi_op(MPI_OP_NULL) { }

inline
MPI::Op::Op(const MPI_Op &i) : mpi_op(i) { }

inline
MPI::Op::Op(const MPI::Op& op)
  : op_user_function(op.op_user_function), mpi_op(op.mpi_op) { }

inline 
MPI::Op::~Op() 
{ 
#if 0
  mpi_op = MPI_OP_NULL;
  op_user_function = 0;
#endif
}  

inline MPI::Op&
MPI::Op::operator=(const MPI::Op& op) {
  mpi_op = op.mpi_op;
  op_user_function = op.op_user_function;
  return *this;
}

// comparison
inline bool
MPI::Op::operator== (const MPI::Op &a) { return (bool)(mpi_op == a.mpi_op); }

inline bool
MPI::Op::operator!= (const MPI::Op &a) { return (bool)!(*this == a); }

// inter-language operability
inline MPI::Op&
MPI::Op::operator= (const MPI_Op &i) { mpi_op = i; return *this; }

inline
MPI::Op::operator MPI_Op () const { return mpi_op; }

//inline
//MPI::Op::operator MPI_Op* () { return &mpi_op; }

#endif

inline void
MPI::Op::Init(MPI::User_function *func, bool commute)
{
  (void)MPI_Op_create(ompi_mpi_cxx_op_intercept , (int) commute, &mpi_op);
  op_user_function = (User_function*)func;
}


inline void
MPI::Op::Free()
{
  (void)MPI_Op_free(&mpi_op);
}
