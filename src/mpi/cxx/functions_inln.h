// -*- c++ -*-
// 
// $HEADER$
//

#include <string.h>

//
// Point-to-Point Communication
//

inline void 
MPI::Attach_buffer(void* buffer, int size)
{
  (void)MPI_Buffer_attach(buffer, size);
}

inline int 
MPI::Detach_buffer(void*& buffer)
{
  int size;
  (void)MPI_Buffer_detach(&buffer, &size);
  return size;
}

//
// Process Topologies
//

inline void
MPI::Compute_dims(int nnodes, int ndims, int dims[])
{
  (void)MPI_Dims_create(nnodes, ndims, dims);
}


//
// Environmental Inquiry
//

inline void 
MPI::Get_processor_name(char* name, int& resultlen)
{
  (void)MPI_Get_processor_name(name, &resultlen);
}

inline void
MPI::Get_error_string(int errorcode, char* string, int& resultlen)
{
  (void)MPI_Error_string(errorcode, string, &resultlen);
}

inline int 
MPI::Get_error_class(int errorcode) 
{
  int errorclass;
  (void)MPI_Error_class(errorcode, &errorclass);
  return errorclass;
}

inline double 
MPI::Wtime()
{
  return (MPI_Wtime());
}

inline double 
MPI::Wtick()
{
  return (MPI_Wtick());
}

inline void
MPI::Real_init()
{
  // This is here even though ERRORS_THROW_EXCEPTIONS is a const
  // function; there's no way around this.  :-(
  MPI::ERRORS_THROW_EXCEPTIONS.init();
}


inline void
MPI::Init(int& argc, char**& argv)
{
  (void)MPI_Init(&argc, &argv);
  Real_init();
}

inline void
MPI::Init()
{
  (void)MPI_Init(0, 0);
  Real_init();
}

inline void
MPI::Finalize()
{
  // Prevent a memory leak by calling this hidden "free" function here
  // (even though ERRORS_THROW_EXCEPTIONS is a const object)
  MPI::ERRORS_THROW_EXCEPTIONS.free();
  (void)MPI_Finalize();
}

inline bool
MPI::Is_initialized()
{
  int t;
  (void)MPI_Initialized(&t);
  return (bool) t;
}


//
// External Interfaces
//

inline int
MPI::Init_thread(int required)
{
  int provided;
  (void) MPI_Init_thread(0, NULL, required, &provided);
  Real_init();
  return provided;
}


inline int
MPI::Init_thread(int& argc, char**& argv, int required)
{
  int provided;
  (void) MPI_Init_thread(&argc, &argv, required, &provided);
  Real_init();
  return provided;
}


inline bool
MPI::Is_thread_main()
{
  int flag;
  (void) MPI_Is_thread_main(&flag);
  return ((flag == 1) ? true : false);
}


inline int
MPI::Query_thread()
{
  int provided;
  (void) MPI_Query_thread(&provided);
  return provided;
}


//
// Miscellany
//


inline void*
MPI::Alloc_mem(MPI::Aint size, const MPI::Info& info) 
{
  void* baseptr;
  (void) MPI_Alloc_mem(size, info, &baseptr);
  return baseptr;
}


inline void
MPI::Free_mem(void* base)
{
  (void) MPI_Free_mem(base);
}


//
// Process Creation
//


inline void
MPI::Close_port(const char* port_name) 
{
  (void) MPI_Close_port((char *) port_name);
}


inline void
MPI::Lookup_name(const char * service_name, 
			const MPI::Info& info,
			char* port_name)
{
  (void) MPI_Lookup_name((char *) service_name, info, port_name);
}


inline void
MPI::Open_port(const MPI::Info& info, char* port_name)
{
  (void) MPI_Open_port(info, port_name);
}


inline void
MPI::Publish_name(const char* service_name, 
			 const MPI::Info& info,
			 const char* port_name)
{
  (void) MPI_Publish_name((char *) service_name, info,
			 (char *) port_name);
}


inline void
MPI::Unpublish_name(const char* service_name, 
			   const MPI::Info& info,
			   const char* port_name)
{
  (void)MPI_Unpublish_name((char *) service_name, info,
                         (char *) port_name);
}



//
// Profiling
//

inline void
MPI::Pcontrol(const int level, ...)
{
  va_list ap;
  va_start(ap, level);
 
  (void)MPI_Pcontrol(level, ap);
  va_end(ap);
}


inline void
MPI::Get_version(int& version, int& subversion)
{
  (void)MPI_Get_version(&version, &subversion);
}


inline MPI::Aint
MPI::Get_address(void* location)
{
  MPI::Aint ret;
  MPI_Get_address(location, &ret);
  return ret;
}
