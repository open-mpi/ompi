#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "rte/universe/registry.h"

ompi_registry_core *registry;
ompi_keytable *keytable;

int main(int argc, char *argv)
{
    ompi_keytable *nextkey;
    ompi_registry_core *nextreg;

    keytable = (ompi_keytable *)malloc(sizeof(ompi_keytable));
    keytable->token = strdup("universe");
    keytable->key = 1;
    keytable->next = NULL;

    nextkey = (ompi_keytable *)malloc(sizeof(ompi_keytable));
    nextkey->token = strdup("commworld-1");
    nextkey->key = 1;
    keytable->next = nextkey;
    nextkey->next = NULL;

    registry = (ompi_registry_core *)malloc(sizeof(ompi_registry_core));
    registry->primary_key = ompi_getkey("universe");
    registry->keys = (ompi_keylist *)malloc(sizeof(ompi_keylist));
    registry->keys->key = ompi_getkey("commworld-1");
    registry->keys->next = NULL;
    registry->object_size = 100;
    registry->object = (uint8_t *)malloc(100);
    registry->subscriber = (ompi_subscribe_list *)malloc(sizeof(ompi_subscribe_list));
    registry->subscriber->id = 1;
    registry->subscriber->action = OMPI_REGISTRY_NOTIFY_MODIFICATION | OMPI_REGISTRY_NOTIFY_DELETE;

    printf("universe key = %d\n", registry->primary_key);
}

int ompi_getkey(char *token)
{
    ompi_keytable *ptr;

    ptr = keytable;
    while ((ptr != NULL) && (0 != strcmp(token, ptr->token))) {
	ptr = ptr->next;
    }
    if (NULL == ptr) {
	return(-1);
    }
    return(ptr->key);
}
