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

//
// Groups, Contexts, and Communicators
//

inline int
MPI::Group::Get_size() const
{
  int size;
  (void)MPI_Group_size(mpi_group, &size);
  return size;
}

inline int
MPI::Group::Get_rank() const 
{
  int rank;
  (void)MPI_Group_rank(mpi_group, &rank);
  return rank;
}

inline void
MPI::Group::Translate_ranks (const MPI::Group& group1, int n,
				    const int ranks1[], 
				    const MPI::Group& group2, int ranks2[])
{
  (void)MPI_Group_translate_ranks(group1, n, (int*)ranks1, group2, (int*)ranks2);
}

inline int
MPI::Group::Compare(const MPI::Group& group1, const MPI::Group& group2)
{
  int result;
  (void)MPI_Group_compare(group1, group2, &result);
  return result;
}

inline MPI::Group
MPI::Group::Union(const MPI::Group &group1, const MPI::Group &group2)
{
  MPI_Group newgroup;
  (void)MPI_Group_union(group1, group2, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Intersect(const MPI::Group &group1, const MPI::Group &group2)
{
  MPI_Group newgroup;
  (void)MPI_Group_intersection( group1,  group2, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Difference(const MPI::Group &group1, const MPI::Group &group2)
{
  MPI_Group newgroup;  
  (void)MPI_Group_difference(group1, group2, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Incl(int n, const int ranks[]) const
{
  MPI_Group newgroup;
  (void)MPI_Group_incl(mpi_group, n, (int*)ranks, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Excl(int n, const int ranks[]) const
{
  MPI_Group newgroup;
  (void)MPI_Group_excl(mpi_group, n, (int*)ranks, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Range_incl(int n, const int ranges[][3]) const
{
  MPI_Group newgroup;
  (void)MPI_Group_range_incl(mpi_group, n, (int(*)[3])ranges, &newgroup);
  return newgroup;
}

inline MPI::Group
MPI::Group::Range_excl(int n, const int ranges[][3]) const
{
  MPI_Group newgroup;
  (void)MPI_Group_range_excl(mpi_group, n, (int(*)[3])ranges, &newgroup);
  return newgroup;
}

inline void
MPI::Group::Free()
{
  (void)MPI_Group_free(&mpi_group);
}
