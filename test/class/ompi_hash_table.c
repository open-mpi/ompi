/*
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <string.h>
#include "support.h"
#include "class/ompi_object.h"
#include "class/ompi_hash_table.h"

static FILE *error_out=NULL;

char *num_keys[] = {
    "1234", "1234",
    "5678", "5678",
    "12450", "12450",
    "45623", "45623",
    NULL
};


char *str_keys[] = {
    "foo", "bar",
    "2", "this cow jumped over the moon",
    "this is another key", "this is another value",
    "key key", "value value",
    NULL
};


char *perm_keys[] = {
    "abcdef", "abcdef",
    "bcdefa", "bcdefa",
    "cdefab", "cdefab",
    "defabc", "defabc",
    "efabcd", "efabcd",
    "fabcde", "fabcde",
    "badcfe", "badcfe",
    "badcef", "badcef",
    "abdcfe", "abdcfe",
    "bcdaef", "bcdaef",
    NULL
};

static void validate_table(ompi_hash_table_t *table, char *keys[], int is_numeric_keys)
{
    int         j;
    const char  *val;
    
    for ( j = 0; keys[j]; j += 2)
    {
        if ( 1 == is_numeric_keys )
            val = ompi_hash_table_get_value_uint32(table, atoi(keys[j]));
        else
            val = ompi_hash_table_get_value_ptr(table, keys[j], strlen(keys[j]));
        test_verify_str(keys[j+1], val);
    }
    test_verify_int(j/2, ompi_hash_table_get_size(table));
}

static void test_htable(ompi_hash_table_t *table)
{
    int j;
    fprintf(error_out, "\nTesting integer keys...\n");
    for ( j = 0; num_keys[j]; j += 2)
    {
        ompi_hash_table_set_value_uint32(table, atoi(num_keys[j]), num_keys[j+1]);
    }
    validate_table(table, num_keys, 1);
    
    /* remove all values for next test */
    ompi_hash_table_remove_all(table);
    test_verify_int(0, ompi_hash_table_get_size(table));
    
    fprintf(error_out, "\nTesting string keys...\n");
    for ( j = 0; str_keys[j]; j += 2)
    {
        ompi_hash_table_set_value_ptr(table, str_keys[j], strlen(str_keys[j]), str_keys[j+1]);
    }
    validate_table(table, str_keys, 0);
    
    /* remove all values for next test */
    ompi_hash_table_remove_all(table);
    test_verify_int(0, ompi_hash_table_get_size(table));
    
    fprintf(error_out, "\nTesting collision resolution...\n");
    /* All of the string keys in keys array should
        have the same hash value. */
    for ( j = 0; perm_keys[j]; j += 2)
    {
        ompi_hash_table_set_value_ptr(table, perm_keys[j], strlen(perm_keys[j]), perm_keys[j+1]);
    }

    validate_table(table, perm_keys, 0);
    
    /* remove all values for next test */
    ompi_hash_table_remove_all(table);
    test_verify_int(0, ompi_hash_table_get_size(table));
    
    fprintf(error_out, "\n\n");
}


static void test_dynamic(void)
{
    ompi_hash_table_t     *table;
    
    table = OBJ_NEW(ompi_hash_table_t);
    if ( NULL == table )
    {
        fprintf(error_out, "Error: Unable to create hash table.\n");
        exit(-1);
    }
    fprintf(error_out, "Testing with dynamically created table...\n");
    ompi_hash_table_init(table, 4);
    test_htable(table);
    
    OBJ_RELEASE(table);
}


static void test_static(void)
{
    ompi_hash_table_t     table;
    
    OBJ_CONSTRUCT(&table, ompi_hash_table_t);
    ompi_hash_table_init(&table, 128);

    fprintf(error_out, "Testing with statically created table...\n");
    test_htable(&table);

    OBJ_DESTRUCT(&table);
}


int main(int argc, char **argv)
{
    /* local variables */
    test_init("ompi_hash_table_t");

#ifdef STANDALONE
    error_out = stderr;
#else
    error_out = fopen( "./ompi_hash_table_test_out.txt", "w" );
    if( error_out == NULL ) error_out = stderr;
#endif
    
    test_dynamic();
    test_static();
#ifndef STANDALONE
    fclose( error_out );
#endif
    
    return test_finalize();
}
