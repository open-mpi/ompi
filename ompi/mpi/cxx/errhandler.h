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

class Errhandler {
public:

#if 0 /* OMPI_ENABLE_MPI_PROFILING */

  // construction / destruction
  inline Errhandler() { }

  inline virtual ~Errhandler() { }

  inline Errhandler(const MPI_Errhandler &i)
    : pmpi_errhandler(i) { }

 // copy / assignment
  inline Errhandler(const Errhandler& e)
    : pmpi_errhandler(e.pmpi_errhandler) { }

  inline Errhandler(const PMPI::Errhandler& e)
    : pmpi_errhandler(e) { }

  inline Errhandler& operator=(const Errhandler& e) {
    pmpi_errhandler = e.pmpi_errhandler; return *this; }

  // comparison
  inline bool operator==(const Errhandler &a) {
    return (bool)(pmpi_errhandler == a.pmpi_errhandler); }
  
  inline bool operator!=(const Errhandler &a) {
    return (bool)!(*this == a); }

  // inter-language operability
  inline Errhandler& operator= (const MPI_Errhandler &i) {
    pmpi_errhandler = i; return *this; }
 
  inline operator MPI_Errhandler() const { return pmpi_errhandler; }
 
  //  inline operator MPI_Errhandler*() { return pmpi_errhandler; }
  
  inline operator const PMPI::Errhandler&() const { return pmpi_errhandler; }

#else

  // construction / destruction
  inline Errhandler()
    : mpi_errhandler(MPI_ERRHANDLER_NULL) {}

  inline virtual ~Errhandler() { }

  inline Errhandler(const MPI_Errhandler &i)
    : mpi_errhandler(i) {}

 // copy / assignment
  inline Errhandler(const Errhandler& e)
    : handler_fn(e.handler_fn), mpi_errhandler(e.mpi_errhandler) { }

  inline Errhandler& operator=(const Errhandler& e)
  {
    mpi_errhandler = e.mpi_errhandler;
    handler_fn = e.handler_fn;
    return *this;
  }

  // comparison
  inline bool operator==(const Errhandler &a) {
    return (bool)(mpi_errhandler == a.mpi_errhandler); }
  
  inline bool operator!=(const Errhandler &a) {
    return (bool)!(*this == a); }

  // inter-language operability
  inline Errhandler& operator= (const MPI_Errhandler &i) {
    mpi_errhandler = i; return *this; }
 
  inline operator MPI_Errhandler() const { return mpi_errhandler; }
 
  //  inline operator MPI_Errhandler*() { return &mpi_errhandler; }
  
#endif

  //
  // Errhandler access functions
  //
  
  virtual void Free();

#if !0 /* OMPI_ENABLE_MPI_PROFILING */
  Comm::Errhandler_fn* handler_fn;
#endif

protected:
#if 0 /* OMPI_ENABLE_MPI_PROFILING */
  PMPI::Errhandler pmpi_errhandler;
#else
  MPI_Errhandler mpi_errhandler;
#endif


public:
  // took out the friend decls
  //private:

  //this is for ERRORS_THROW_EXCEPTIONS
  //this is called from MPI::Real_init
  inline void init() const {
#if ! 0 /* OMPI_ENABLE_MPI_PROFILING */
    // $%%@#%# AIX/POE 2.3.0.0 makes us put in this cast here
    (void)MPI_Errhandler_create((MPI_Handler_function*) &ompi_mpi_cxx_throw_excptn_fctn,
				(MPI_Errhandler *) &mpi_errhandler); 
#else
    pmpi_errhandler.init();
#endif
  }

  //this is for ERRORS_THROW_EXCEPTIONS
  //this is called from MPI::Finalize
  inline void free() const {
#if ! 0 /* OMPI_ENABLE_MPI_PROFILING */
    (void)MPI_Errhandler_free((MPI_Errhandler *) &mpi_errhandler); 
#else
    pmpi_errhandler.free();
#endif
  }
};
