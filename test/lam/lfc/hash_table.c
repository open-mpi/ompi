/*
 * $HEADER$
 */

#include <stdint.h>
#include <string.h>
#include "support.h"
#include "lam/lfc/lam_object.h"
#include "lam/lfc/lam_hash_table.h"

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

static void validate_table(lam_hash_table_t *table, char *keys[], int is_numeric_keys)
{
    int         j;
    const char  *val;
    
    for ( j = 0; keys[j]; j += 2)
    {
        if ( 1 == is_numeric_keys )
            val = lam_hash_table_get_value_uint32(table, atoi(keys[j]));
        else
            val = lam_hash_table_get_value_ptr(table, keys[j], strlen(keys[j]));
        test_verify(keys[j+1], val);
    }
    test_verify_int(j/2, lam_hash_table_get_size(table));
}

static void test_htable(lam_hash_table_t *table)
{
    int j;
    printf("\nTesting integer keys...\n");
    for ( j = 0; num_keys[j]; j += 2)
    {
        lam_hash_table_set_value_uint32(table, atoi(num_keys[j]), num_keys[j+1]);
    }
    validate_table(table, num_keys, 1);
    
    /* remove all values for next test */
    lam_hash_table_remove_all(table);
    test_verify_int(0, lam_hash_table_get_size(table));
    
    printf("\nTesting string keys...\n");
    for ( j = 0; str_keys[j]; j += 2)
    {
        lam_hash_table_set_value_ptr(table, str_keys[j], strlen(str_keys[j]), str_keys[j+1]);
    }
    validate_table(table, str_keys, 0);
    
    /* remove all values for next test */
    lam_hash_table_remove_all(table);
    test_verify_int(0, lam_hash_table_get_size(table));
    
    printf("\nTesting collision resolution...\n");
    /* All of the string keys in keys array should
        have the same hash value. */
    for ( j = 0; perm_keys[j]; j += 2)
    {
        lam_hash_table_set_value_ptr(table, perm_keys[j], strlen(perm_keys[j]), perm_keys[j+1]);
    }

    validate_table(table, perm_keys, 0);
    
    /* remove all values for next test */
    lam_hash_table_remove_all(table);
    test_verify_int(0, lam_hash_table_get_size(table));
    
    printf("\n\n");
}


static void test_dynamic(void)
{
    lam_hash_table_t     *table;
    
    table = OBJ_NEW(lam_hash_table_t);
    if ( NULL == table )
    {
        printf("Error: Unable to create hash table.\n");
        exit(-1);
    }
    printf("Testing with dynamically created table...\n");
    lam_hash_table_init(table, 4);
    test_htable(table);
    
    OBJ_RELEASE(table);
}


static void test_static(void)
{
    lam_hash_table_t     table;
    
    OBJ_CONSTRUCT(&table, lam_hash_table_t);
    lam_hash_table_init(&table, 128);

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
