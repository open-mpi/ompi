/*
 * $HEADER$
 */

#include "support.h"
#include "lam/lfc/list.h"

int main(int argc, char **argv)
{
    /* local variables */
    lam_list_t list;
    lam_list_type_t list_type,list_type_out;
    size_t list_size;

    test_init("List");

    /* initialize list */
    lam_list_init(&list);

    /* check length of list */
    list_size=lam_list_get_size(&list);
    if( 0 == list_size ) {
        test_success();
    } else {
        test_failure(" lam_list_get_size");
    }

    /* check list type */
    list_type=2;
    lam_list_set_type(&list,list_type);
    list_type_out=0;
    list_type_out=lam_list_get_type(&list);
    if( list_type_out == list_type ) {
        test_success();
    } else {
        test_failure(" lam_list_set/get_type");
    }


    test_finalize();
    return 0;
}
