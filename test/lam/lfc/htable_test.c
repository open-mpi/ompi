/*
 * $HEADER$
 */

#include <stdint.h>
#include "support.h"
#include "lam/lfc/object.h"
#include "lam/lfc/hash_table.h"

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

void validate_table(lam_fast_hash_t *table, char *keys[],
                    int is_numeric_keys)
{
    int         j;
    const char  *val;
    
    for ( j = 0; keys[j]; j += 2)
    {
        if ( 1 == is_numeric_keys )
            val = lam_fh_get_value_for_ikey(table, atoi(keys[j]));
        else
            val = lam_fh_get_value_for_skey(table, keys[j]);
        
        test_verify(keys[j+1], val);
    }
    test_verify_int(j/2, lam_fh_count(table));
}

void test_htable(lam_fast_hash_t *table)
{
    char        *skey1 = "foo", *skey2 = "2";
    int         ikey1 = 2, j;
    
    printf("\nTesting integer keys...\n");
    for ( j = 0; num_keys[j]; j += 2)
    {
        lam_fh_set_value_for_ikey(table, num_keys[j+1], 
                                  atoi(num_keys[j]));
        
    }
    validate_table(table, num_keys, 1);
    
    /* remove all values for next test */
    lam_fh_remove_all(table);
    test_verify_int(0, lam_fh_count(table));
    
    printf("\nTesting string keys...\n");
    for ( j = 0; str_keys[j]; j += 2)
    {
        lam_fh_set_value_for_skey(table, str_keys[j+1], 
                                  str_keys[j]);
        
    }
    validate_table(table, str_keys, 0);
    
    /* remove all values for next test */
    lam_fh_remove_all(table);
    test_verify_int(0, lam_fh_count(table));
    
    
    printf("\nTesting collision resolution...\n");
    /* All of the string keys in keys array should
        have the same hash value. */
    for ( j = 0; perm_keys[j]; j += 2)
    {
        lam_fh_set_value_for_skey(table, perm_keys[j+1], perm_keys[j]);
    }

    validate_table(table, perm_keys, 0);
    
    /* remove all values for next test */
    lam_fh_remove_all(table);
    test_verify_int(0, lam_fh_count(table));
    
    printf("\n\n");
}


void test_dynamic()
{
    lam_fast_hash_t     *table;
    
    table = OBJ_NEW(lam_fast_hash_t);
    if ( NULL == table )
    {
        printf("Error: Unable to create hash table.\n");
        exit(-1);
    }
    printf("Testing with dynamically created table...\n");
    test_htable(table);
    
    OBJ_RELEASE(table);
}


void test_static()
{
    lam_fast_hash_t     table;
    
    OBJ_CONSTRUCT(&table, lam_fast_hash_t);

    printf("Testing with statically created table...\n");
    test_htable(&table);

    OBJ_DESTRUCT(&table);
}


int main(int argc, char **argv)
{
    /* local variables */
    test_init("Hash Table");
    
    test_dynamic();
    test_static();
    
    test_finalize();
    return 0;
}
