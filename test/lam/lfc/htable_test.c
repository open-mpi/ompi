/*
 * $HEADER$
 */

#include <stdint.h>
#include "support.h"
#include "lam/lfc/object.h"
#include "lam/lfc/hash_table.h"

char *keys[] = {
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

void test_htable(lam_fast_hash_t *table)
{
    char        *skey1 = "foo";
    int         ikey1 = 2, j;
    
    printf("Testing simple adds...\n");
    printf("Adding (key = %s, value = bar)\n", skey1);
    lam_fh_set_value_for_skey(table, "bar", skey1);
    
    printf("Adding (key = 2, value = this cow jumped over the moon)\n");
    lam_fh_set_value_for_ikey(table, "this cow jumped over the moon", ikey1);
    
    printf("Table contains %d items\n", lam_fh_count(table));
    
    printf("Value for key %s is %s\n", skey1,
           lam_fh_get_value_for_skey(table, skey1));
    
    printf("Value for key %d is %s\n", ikey1,
           lam_fh_get_value_for_ikey(table, ikey1));
    
    lam_fh_remove_all(table);
    printf("\nRemoved all table items (size = %d)...\n",
           lam_fh_count(table));

    printf("\nTesting collision resolution...\n");
    for ( j = 0; keys[j]; j += 2)
    {
        lam_fh_set_value_for_skey(table, keys[j+1], keys[j]);
    }

    printf("Table contents: \n");
    for ( j = 0; keys[j]; j += 2)
    {
        printf("value for key %s is %s\n",
               keys[j], lam_fh_get_value_for_skey(table, keys[j]));
    }

    printf("\n\n");
}


void test_dynamic()
{
    lam_fast_hash_t     *table;
    
    table = OBJ_CREATE(lam_fast_hash_t, &lam_fast_hash_cls);
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
    
    STATIC_INIT(table, &lam_fast_hash_cls);

    printf("Testing with statically created table...\n");
    test_htable(&table);

    STATIC_DESTROY(table);
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
