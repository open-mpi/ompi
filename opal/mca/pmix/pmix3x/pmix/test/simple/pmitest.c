/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pmi.h"

static const char * PMI_Err_str(int error)
{
    static char str[100];
    switch (error)
    {
    case PMI_SUCCESS:
	return "PMI_SUCCESS";
    case PMI_FAIL:
	return "PMI_FAIL";
    case PMI_ERR_INVALID_ARG:
	return "PMI_ERR_INVALID_ARG";
    case PMI_ERR_INVALID_KEY:
	return "PMI_ERR_INVALID_KEY";
    case PMI_ERR_INVALID_KEY_LENGTH:
	return "PMI_ERR_INVALID_KEY_LENGTH";
    case PMI_ERR_INVALID_VAL:
	return "PMI_ERR_INVALID_VAL";
    case PMI_ERR_INVALID_VAL_LENGTH:
	return "PMI_ERR_INVALID_VAL_LENGTH";
    case PMI_ERR_INVALID_LENGTH:
	return "PMI_ERR_INVALID_LENGTH";
    case PMI_ERR_INIT:
	return "PMI_ERR_INIT";
    case PMI_ERR_NOMEM:
	return "PMI_ERR_NOMEM";
    }
    sprintf(str, "PMI_ERR_UNKNOWN: %d", error);
    return str;
}

#define PRINT_ERROR(error, fcname) if (error != PMI_SUCCESS) printf("%s failed: %s\n", fcname, PMI_Err_str(error)); else printf("%s unexpectedly succeeded\n", fcname); fflush(stdout);

int main( int argc, char * argv[] )
{
    int rc, spawned, size, rank, name_max, id_maxlen, key_maxlen, val_maxlen;
    char *kvsname, *id, *domain_id, *key, *val;

    rc = PMI_Init( &spawned );
    if ( rc != PMI_SUCCESS )
    {
       printf( "PMI_Init failed with rc = %s\n", PMI_Err_str(rc) );
       return -1 ;
   }
   else
   {
       printf( "PMI_Init returned spawned = %d\n", spawned );
   }

   rc = PMI_Get_size( &size );
   if ( rc == PMI_SUCCESS )
   {
       rc = PMI_Get_rank( &rank );
       if ( rc == PMI_SUCCESS )
           printf( "size = %d, rank = %d\n", size, rank );
       else
           printf( "PMI_Get_Rank failed with rc = %s\n", PMI_Err_str(rc) );
   }
   else
       printf( "PMI_Get_size failed with rc = %s\n", PMI_Err_str(rc) );

   rc = PMI_KVS_Get_name_length_max( &name_max );
   if ( rc != PMI_SUCCESS )
   {
       printf( "PMI_KVS_Get_name_length_max failed with rc = %s\n", PMI_Err_str(rc) );
       return -1;
   }
   else
       printf( "PMI_KVS_Get_name_length_max got %d\n", name_max );

   kvsname = (char *) malloc( name_max );
   rc = PMI_KVS_Get_my_name( kvsname, name_max );
   if ( rc != PMI_SUCCESS )
   {
       printf( "PMI_KVS_Get_my_name failed with rc = %s\n", PMI_Err_str(rc) );
       return -1;
   }
   else
       printf( "PMI_KVS_Get_my_name got %s\n", kvsname );

   rc = PMI_Get_id_length_max( &id_maxlen );
   if ( rc != PMI_SUCCESS )
   {
       printf("PMI_Get_id_length_max failed with rc = %s\n", PMI_Err_str(rc) );
       return -1;
   }
   else
       printf("PMI_Get_id_length_max got %d\n", id_maxlen);
   id = (char *) malloc( id_maxlen );
   rc = PMI_Get_id( id, id_maxlen );
   if ( rc != PMI_SUCCESS )
   {
       printf("PMI_Get_id failed with rc = %s\n", PMI_Err_str(rc));
   }
   else
       printf( "PMI_Get_id got %s\n", id );
   domain_id = (char *) malloc( id_maxlen );
   rc = PMI_Get_kvs_domain_id( domain_id, id_maxlen );
   if ( rc != PMI_SUCCESS )
   {
       printf("PMI_Get_kvs_domain_id failed with rc = %s\n", PMI_Err_str(rc));
   }
   else
       printf( "PMI_Get_kvs_domain_id got %s\n", domain_id );

   rc = PMI_KVS_Get_key_length_max( &key_maxlen );
   if (rc != PMI_SUCCESS )
   {
       printf("PMI_KVS_Get_key_length_max failed with rc = %s\n", PMI_Err_str(rc));
       return -1;
   }
   else
       printf( "PMI_Get_key_maxlen got %d\n", key_maxlen );
   key = (char *) malloc( key_maxlen );
   rc = PMI_KVS_Get_value_length_max( &val_maxlen );
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_KVS_Get_value_length_max failed with rc = %s\n", PMI_Err_str(rc));
       return -1;
   }
   else
       printf( "PMI_Get_val_maxlen got %d\n", val_maxlen );
   val = (char *) malloc( val_maxlen );

   sprintf(key, "test_key_%d", rank);
   sprintf(val, "test_value_%d", rank);

   rc = PMI_KVS_Put( kvsname, key, val );
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_KVS_Put failed with rc = %s\n", PMI_Err_str(rc));
   }
   rc = PMI_KVS_Commit( kvsname );
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_KVS_Commit failed with rc = %s\n", PMI_Err_str(rc));
   }
   rc = PMI_Barrier();
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_Barrier failed with rc = %s\n", PMI_Err_str(rc));
   }

   sprintf(key, "test_key_%d", (rank + 1) % size);
   rc = PMI_KVS_Get( kvsname, key, val, val_maxlen );
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_KVS_Get(%s) failed with rc = %s\n", key, PMI_Err_str(rc));
   }
   else
       printf("PMI_KVS_Get(%s) returned %s\n", key, val);

    /* Test awkward character string put and get */
   if (rank == 0)
   {
       sprintf(key, "foo");
       sprintf(val, "foo=bar baz=bif name=\"Buzz Bee\" clink=~!@#$\\;':<>,. clank=a b c");

       rc = PMI_KVS_Put( kvsname, key, val );
       if (rc != PMI_SUCCESS)
       {
           printf("PMI_KVS_Put failed with rc = %s\n", PMI_Err_str(rc));
       }
       rc = PMI_KVS_Commit( kvsname );
       if (rc != PMI_SUCCESS)
       {
           printf("PMI_KVS_Commit failed with rc = %s\n", PMI_Err_str(rc));
       }
   }

   rc = PMI_Barrier();
   if (rc != PMI_SUCCESS)
   {
       printf("PMI_Barrier failed with rc = %s\n", PMI_Err_str(rc));
   }

   if (rank == size - 1)
   {
       sprintf(key, "foo");
       rc = PMI_KVS_Get( kvsname, key, val, val_maxlen );
       if (rc != PMI_SUCCESS)
       {
           printf("PMI_KVS_Get(%s) failed with rc = %s\n", key, PMI_Err_str(rc));
       }
       else
           printf("PMI_KVS_Get(%s) returned %s\n", key, val);
   }

   if ( rank == (size - 1) )
   {
       key[0] = '\0';
       val[0] = '\0';
       rc = PMI_KVS_Iter_first(kvsname, key, key_maxlen, val, val_maxlen);
       if (rc == PMI_SUCCESS)
       {
           while (key[0] != '\0')
           {
              printf("PMI_KVS_Iter got key=%s val=%s\n",key,val);
              rc = PMI_KVS_Iter_next(kvsname, key, key_maxlen, val, val_maxlen);
              if (rc != PMI_SUCCESS)
              {
                  printf("PMK_KVS_Iter_next failed with rc = %s\n", PMI_Err_str(rc));
                  break;
              }
          }
      }
      else
      {
       printf("PMI_KVS_Iter_first failed with rc = %s\n", PMI_Err_str(rc));
   }
}

    /* error testing */
if (rank != 0)
{
	printf("PMI error testing:\n");
	strcpy(key, "test_key");
	strcpy(val, "test_val");
	rc = PMI_KVS_Put("baloney", key, val);
	PRINT_ERROR(rc, "PMI_KVS_Put(baloney, key, val)");
	rc = PMI_KVS_Put(NULL, key, val);
	PRINT_ERROR(rc, "PMI_KVS_Put(NULL, key, val)");
	rc = PMI_KVS_Put(kvsname, NULL, val);
	PRINT_ERROR(rc, "PMI_KVS_Put(kvsname, NULL, val)");
	rc = PMI_KVS_Put(kvsname, key, NULL);
	PRINT_ERROR(rc, "PMI_KVS_Put(kvsname, key, NULL)");
	rc = PMI_KVS_Get("baloney", key, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Get(baloney, key, val, val_maxlen)");
	rc = PMI_KVS_Get(NULL, key, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Get(NULL, key, val, val_maxlen)");
	rc = PMI_KVS_Get(kvsname, NULL, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Get(kvsname, NULL, val, val_maxlen)");
	rc = PMI_KVS_Get(kvsname, key, NULL, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Get(kvsname, key, NULL, val_maxlen)");
	rc = PMI_KVS_Get(kvsname, key, val, -1);
	PRINT_ERROR(rc, "PMI_KVS_Get(kvsname, key, val, -1)");
	rc = PMI_KVS_Commit(NULL);
	PRINT_ERROR(rc, "PMI_KVS_Commit(NULL)");
	rc = PMI_KVS_Commit("baloney");
	PRINT_ERROR(rc, "PMI_KVS_Commit(baloney)");
	rc = PMI_KVS_Get_my_name(NULL, name_max);
	PRINT_ERROR(rc, "PMI_KVS_Get_my_name(NULL, name_max)");
	rc = PMI_KVS_Get_my_name(kvsname, -1);
	PRINT_ERROR(rc, "PMI_KVS_Get_my_name(kvsname, -1)");
	rc = PMI_Get_id(NULL, id_maxlen);
	PRINT_ERROR(rc, "PMI_Get_id(NULL, id_maxlen)");
	rc = PMI_Get_id(id, -1);
	PRINT_ERROR(rc, "PMI_Get_id(id, -1)");
	rc = PMI_Get_kvs_domain_id(NULL, id_maxlen);
	PRINT_ERROR(rc, "PMI_Get_domain_id(NULL, id_maxlen)");
	rc = PMI_Get_kvs_domain_id(domain_id, -1);
	PRINT_ERROR(rc, "PMI_Get_domain_id(domain_id, -1)");
	rc = PMI_Init(NULL);
	PRINT_ERROR(rc, "PMI_Init(NULL)");
	rc = PMI_Get_rank(NULL);
	PRINT_ERROR(rc, "PMI_Get_rank(NULL)");
	rc = PMI_Get_size(NULL);
	PRINT_ERROR(rc, "PMI_Get_size(NULL)");
	rc = PMI_KVS_Get_name_length_max(NULL);
	PRINT_ERROR(rc, "PMI_Get_name_length_max(NULL)");
	rc = PMI_Get_id_length_max(NULL);
	PRINT_ERROR(rc, "PMI_Get_id_length_max(NULL)");
	rc = PMI_KVS_Get_key_length_max(NULL);
	PRINT_ERROR(rc, "PMI_Get_key_length_max(NULL)");
	rc = PMI_KVS_Get_value_length_max(NULL);
	PRINT_ERROR(rc, "PMI_Get_value_length_max(NULL)");
	rc = PMI_KVS_Iter_first("baloney", key, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(baloney, key, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_first(NULL, key, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(NULL, key, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_first(kvsname, NULL, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(kvsname, NULL, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_first(kvsname, key, -1, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(kvsname, key, -1, val, val_maxlen)");
	rc = PMI_KVS_Iter_first(kvsname, key, key_maxlen, NULL, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(kvsname, key, key_maxlen, NULL, val_maxlen)");
	rc = PMI_KVS_Iter_first(kvsname, key, key_maxlen, val, -1);
	PRINT_ERROR(rc, "PMI_KVS_Iter_first(kvsname, key, key_maxlen, val, -1)");
	rc = PMI_KVS_Iter_next("baloney", key, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(baloney, key, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_next(NULL, key, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(NULL, key, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_next(kvsname, NULL, key_maxlen, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(kvsname, NULL, key_maxlen, val, val_maxlen)");
	rc = PMI_KVS_Iter_next(kvsname, key, -1, val, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(kvsname, key, -1, val, val_maxlen)");
	rc = PMI_KVS_Iter_next(kvsname, key, key_maxlen, NULL, val_maxlen);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(kvsname, key, key_maxlen, NULL, val_maxlen)");
	rc = PMI_KVS_Iter_next(kvsname, key, key_maxlen, val, -1);
	PRINT_ERROR(rc, "PMI_KVS_Iter_next(kvsname, key, key_maxlen, val, -1)");
}

rc = PMI_Finalize( );
return 0;
}
