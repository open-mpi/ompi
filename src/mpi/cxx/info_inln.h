// -*- c++ -*-
// 
// $HEADER$
//


inline MPI::Info 
MPI::Info::Create()
{
  MPI_Info newinfo;
  (void) MPI_Info_create(&newinfo);
  return newinfo;
}

inline void
MPI::Info::Delete(const char* key)
{
   (void)MPI_Info_delete(mpi_info, (char*)key);
}

inline MPI::Info 
MPI::Info::Dup() const
{
  MPI_Info newinfo;
  (void)MPI_Info_dup(mpi_info, &newinfo);
  return newinfo;
}

inline void
MPI::Info::Free()
{
  (void) MPI_Info_free(&mpi_info);
}

inline bool
MPI::Info::Get(const char* key, int valuelen, char* value) const
{
  int flag;
  (void)MPI_Info_get(mpi_info, (char*)key, valuelen, value, &flag);
  return (bool) flag;
}

inline int
MPI::Info::Get_nkeys() const
{
  int nkeys;
  MPI_Info_get_nkeys(mpi_info, &nkeys);
  return nkeys;
}

inline void
MPI::Info::Get_nthkey(int n, char* key) const
{
  (void) MPI_Info_get_nthkey(mpi_info, n, key);
}

inline bool 
MPI::Info::Get_valuelen(const char* key, int& valuelen) const
{
  int flag;
  (void) MPI_Info_get_valuelen(mpi_info, (char*)key, &valuelen, &flag);
  return (bool) flag;
}

inline void
MPI::Info::Set(const char* key, const char* value)
{
  (void) MPI_Info_set(mpi_info, (char*)key, (char*)value);
}
