// -*- c++ -*-
// 
// $HEADER$
//

//
// Point-to-Point Communication
//

inline int
MPI::Status::Get_count(const MPI::Datatype& datatype) const
{
  int count;
  //(MPI_Status*) is to cast away the const
  (void)MPI_Get_count((MPI_Status*)&mpi_status, datatype, &count);
  return count;
}

inline bool
MPI::Status::Is_cancelled() const
{
  int t;
  (void)MPI_Test_cancelled((MPI_Status*)&mpi_status, &t);
  return (bool) t;
}

inline int
MPI::Status::Get_elements(const MPI::Datatype& datatype) const
{
  int count;
  (void)MPI_Get_elements((MPI_Status*)&mpi_status, datatype, &count);
  return count;
}

//
// Status Access
//
inline int
MPI::Status::Get_source() const
{
  int source;
  source = mpi_status.MPI_SOURCE;
  return source;
}

inline void
MPI::Status::Set_source(int source)
{
  mpi_status.MPI_SOURCE = source;
}

inline int
MPI::Status::Get_tag() const
{
  int tag;
  tag = mpi_status.MPI_TAG;
  return tag;
}

inline void
MPI::Status::Set_tag(int tag)
{
  mpi_status.MPI_TAG = tag;
}

inline int
MPI::Status::Get_error() const
{
  int error;
  error = mpi_status.MPI_ERROR;
  return error;
}

inline void
MPI::Status::Set_error(int error)
{
  mpi_status.MPI_ERROR = error;
}
