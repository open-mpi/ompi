// -*- c++ -*-
//
// $HEADER$
//

inline MPI::Intercomm
MPI::Intercomm::Dup() const
{
  MPI_Comm newcomm;
  (void)MPI_Comm_dup(mpi_comm, &newcomm);
  return newcomm;
}

inline MPI::Intercomm&
MPI::Intercomm::Clone() const
{
  MPI_Comm newcomm;
  (void)MPI_Comm_dup(mpi_comm, &newcomm);
  MPI::Intercomm* dup = new MPI::Intercomm(newcomm);
  return *dup;
}

inline int
MPI::Intercomm::Get_remote_size() const 
{
  int size;
  (void)MPI_Comm_remote_size(mpi_comm, &size);
  return size;
}

inline MPI::Group
MPI::Intercomm::Get_remote_group() const 
{
  MPI_Group group;
  (void)MPI_Comm_remote_group(mpi_comm, &group);
  return group;
}

inline MPI::Intracomm
MPI::Intercomm::Merge(bool high)
{
  MPI_Comm newcomm;
  (void)MPI_Intercomm_merge(mpi_comm, (int)high, &newcomm);
  return newcomm;
}


//
// Extended Collective Operations
//

inline MPI::Intercomm
MPI::Intercomm::Create(const Group& group) const
{
  MPI_Comm newcomm;
  (void) MPI_Comm_create(mpi_comm, (MPI_Group) group, &newcomm);
  return newcomm;
}

inline MPI::Intercomm
MPI::Intercomm::Split(int color, int key) const
{
  MPI_Comm newcomm;
  (void) MPI_Comm_split(mpi_comm, color, key, &newcomm);
  return newcomm;
}
