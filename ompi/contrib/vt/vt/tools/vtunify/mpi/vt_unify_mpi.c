/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_unify_mpi.h"

#include "mpi.h"

#include <assert.h>

#define MAX_TYPES 12

/* array of MPI datatypes */
static MPI_Datatype typev[MAX_TYPES] =
   { MPI_CHAR,
     MPI_INT,
     MPI_LONG_LONG_INT,
     MPI_DOUBLE,
     MPI_UNSIGNED,
     MPI_UNSIGNED_SHORT,
     MPI_LONG_LONG_INT, /* MPI_UNSIGNED_LONG_LONG */
     MPI_PACKED };
static int typen = 8;

static MPI_Comm get_mpi_comm( VTUnify_MPI_Comm ucomm )
{
   assert( ucomm == 0 );
   return MPI_COMM_WORLD;
}

static VTUnify_MPI_Datatype add_mpi_type( MPI_Datatype type )
{
   VTUnify_MPI_Datatype utype;
   assert( typen < MAX_TYPES );
   utype = typen++;
   typev[utype] = type;
   return utype;
}

static MPI_Datatype get_mpi_type( VTUnify_MPI_Datatype utype )
{
   assert( utype >= 0 && utype < MAX_TYPES );
   return typev[utype];
}

/*** MPI wrapper functions ***/

/* MPI_Abort */

VT_MPI_INT VTUnify_MPI_Abort( VTUnify_MPI_Comm ucomm, VT_MPI_INT errorcode )
{
   VT_MPI_INT error;
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Abort( comm, errorcode );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Address */

VT_MPI_INT VTUnify_MPI_Address( void * location, VTUnify_MPI_Aint * address )
{
   VT_MPI_INT error;

   error = MPI_Address( location, (MPI_Aint*)address );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Barrier */

VT_MPI_INT VTUnify_MPI_Barrier( VTUnify_MPI_Comm ucomm )
{
  VT_MPI_INT error;
  MPI_Comm comm = get_mpi_comm( ucomm );

  error = MPI_Barrier( comm );

  return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Bcast */

VT_MPI_INT VTUnify_MPI_Bcast( void * buffer, VT_MPI_INT count,
                              VTUnify_MPI_Datatype utype, VT_MPI_INT root,
                              VTUnify_MPI_Comm ucomm )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Bcast( (buffer == VTUnify_MPI_BOTTOM) ?
                      MPI_BOTTOM : buffer,
                      count, type, root, comm );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Comm_size */

VT_MPI_INT VTUnify_MPI_Comm_size( VTUnify_MPI_Comm ucomm, VT_MPI_INT * size )
{
   VT_MPI_INT error;
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Comm_size( comm, size );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Comm_rank */

VT_MPI_INT VTUnify_MPI_Comm_rank( VTUnify_MPI_Comm ucomm, VT_MPI_INT * rank )
{
   VT_MPI_INT error;
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Comm_rank( comm, rank );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Finalize */

VT_MPI_INT VTUnify_MPI_Finalize()
{
   VT_MPI_INT error;

   error = MPI_Finalize();

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Init */

VT_MPI_INT VTUnify_MPI_Init( VT_MPI_INT * argc, char *** argv )
{
   VT_MPI_INT error;

   error = MPI_Init( argc, argv );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Pack */

VT_MPI_INT VTUnify_MPI_Pack( void * inbuf, VT_MPI_INT incount,
                             VTUnify_MPI_Datatype utype, void * outbuf,
                             VT_MPI_INT outsize, VT_MPI_INT * position,
                             VTUnify_MPI_Comm ucomm )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Pack( inbuf, incount, type, outbuf, outsize, position, comm );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Pack_size */

VT_MPI_INT VTUnify_MPI_Pack_size( VT_MPI_INT incount,
                                  VTUnify_MPI_Datatype utype,
                                  VTUnify_MPI_Comm ucomm, VT_MPI_INT * size )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Pack_size( incount, type, comm, size );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Recv */

VT_MPI_INT VTUnify_MPI_Recv( void * buf, VT_MPI_INT count,
                             VTUnify_MPI_Datatype utype, VT_MPI_INT source,
                             VT_MPI_INT tag, VTUnify_MPI_Comm ucomm,
                             VTUnify_MPI_Status * ustatus )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );
   MPI_Status status;

   error = MPI_Recv( buf, count, type, source, tag, comm, &status );

   if( ustatus != VTUnify_MPI_STATUS_IGNORE )
   {
      ustatus->source = status.MPI_SOURCE;
      ustatus->tag = status.MPI_TAG;
   }

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Send */

VT_MPI_INT VTUnify_MPI_Send( void * buf, VT_MPI_INT count,
                             VTUnify_MPI_Datatype utype, VT_MPI_INT dest,
                             VT_MPI_INT tag, VTUnify_MPI_Comm ucomm )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Send( buf, count, type, dest, tag, comm );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Type_commit */

VT_MPI_INT VTUnify_MPI_Type_commit( VTUnify_MPI_Datatype * utype )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( *utype );

   error = MPI_Type_commit( &type );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Type_free */

VT_MPI_INT VTUnify_MPI_Type_free( VTUnify_MPI_Datatype * utype )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( *utype );

   error = MPI_Type_free( &type );

   return (error == MPI_SUCCESS) ? 1 : 0;
}

/* MPI_Type_struct */

VT_MPI_INT VTUnify_MPI_Type_struct( VT_MPI_INT count,
                                    VT_MPI_INT * array_of_blocklengths,
                                    VTUnify_MPI_Aint * array_of_udisplacements,
                                    VTUnify_MPI_Datatype * array_of_utypes,
                                    VTUnify_MPI_Datatype * newutype )
{
   VT_MPI_INT error, i;
   MPI_Aint * array_of_displacements;
   MPI_Datatype * array_of_types;
   MPI_Datatype newtype;

   array_of_displacements = (MPI_Aint*)malloc( count * sizeof( MPI_Aint ));
   assert( array_of_displacements );
   for( i = 0; i < count; i++ )
      array_of_displacements[i] = (MPI_Aint)array_of_udisplacements[i];

   array_of_types = (MPI_Datatype*)malloc( count * sizeof( MPI_Datatype ));
   assert( array_of_types );
   for( i = 0; i < count; i++ )
      array_of_types[i] = get_mpi_type( array_of_utypes[i] );

   error = MPI_Type_struct( count, array_of_blocklengths,
                            array_of_displacements, array_of_types,
                            &newtype );

   free( array_of_displacements );
   free( array_of_types );

   if( error == MPI_SUCCESS )
   {
      *newutype = add_mpi_type( newtype );
      return 1;
   }
   else
   {
      return 0;
   }
}

/* MPI_Unpack */

VT_MPI_INT VTUnify_MPI_Unpack( void * inbuf, VT_MPI_INT insize,
                               VT_MPI_INT * position,
                               void * outbuf, VT_MPI_INT outcount,
                               VTUnify_MPI_Datatype utype,
                               VTUnify_MPI_Comm ucomm )
{
   VT_MPI_INT error;
   MPI_Datatype type = get_mpi_type( utype );
   MPI_Comm comm = get_mpi_comm( ucomm );

   error = MPI_Unpack( inbuf, insize, position, outbuf, outcount, type, comm );

   return (error == MPI_SUCCESS) ? 1 : 0;
}
