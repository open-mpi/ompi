#include "orte_config.h"
#include <stdbool.h>

#include "orte/include/orte_constants.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/runtime/runtime.h"

static void notify_callback(orte_gpr_notify_data_t *data, void *cbdata);

static void notify_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
	size_t i, j, k;
	orte_gpr_value_t **values = (orte_gpr_value_t**)(data->values)->addr;

    printf("Callback received - with %lu values\n", (unsigned long)data->cnt);
	for (i = 0, k=0; k < data->cnt &&
                     i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            orte_gpr_value_t *value = values[i];
            k++;
            printf("\tData for value %lu - with %lu keyvals\n",
                (unsigned long)i, (unsigned long)value->cnt);
            for (j = 0; j < value->cnt; j++) {
                orte_gpr_keyval_t *keyval = value->keyvals[j];
                    printf("\t\tKey number: %lu\tkey = %s\n", (unsigned long)j, keyval->key);
			
                switch (keyval->type) {
                    case ORTE_NODE_STATE:
                    printf("\t\tval = %d\n", keyval->value.node_state);
                    break;

                    case ORTE_STRING:
                        printf("\t\tval = %s\n", keyval->value.strptr);
                        break;

                    case ORTE_INT32:
                        printf("\t\tval = %d\n", keyval->value.i32);
                        break;
			
                    case ORTE_INT16:
                        printf("\t\tval = %d\n", keyval->value.i16);
                        break;
			
                    default:
                        printf("\t\tunknown type %d\n", keyval->type);
                }
			}
		}
	}


}

int main(int argc, char **argv)
{
    int i, rc, ret;
    orte_gpr_subscription_t sub, *subs;
    orte_gpr_value_t value, *values;
    char *keys[] = {
        ORTE_NODE_STATE_KEY,
        "stupid-value-one",
        "stupid-value-two",
        "stupid-value-three",
        "stupid-value-four"};

    /* setup the runtime environment */
    if (ORTE_SUCCESS != (rc = orte_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    printf("Registry initted.\n");

	OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
	sub.action = ORTE_GPR_NOTIFY_VALUE_CHG;

	OBJ_CONSTRUCT(&value, orte_gpr_value_t);
	values = &value;
	sub.values = &values;
	sub.cnt = 1; /* number of values */
	value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
	value.segment = strdup(ORTE_NODE_SEGMENT);

	value.cnt = 5; /* number of keyvals */
	value.keyvals = (orte_gpr_keyval_t**)malloc(value.cnt * sizeof(orte_gpr_keyval_t*));

	for (i=0; i < 5; i++) {
	   value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
	   value.keyvals[i]->key = strdup(keys[i]);
    }

	/* Any token */
	value.tokens = NULL;
	value.num_tokens = 0;

	sub.cbfunc = notify_callback;
	sub.user_tag = NULL;

	subs = &sub;
	rc = orte_gpr.subscribe(1, &subs, 0, NULL);

	if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        orte_finalize();
        return 1;
    }

    OBJ_DESTRUCT(&value);
    
    /* now let's write something into those locations */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_NO_OVERWRITE |
                      ORTE_GPR_TOKENS_XAND |
                      ORTE_GPR_KEYS_OR;
    value.segment = strdup(ORTE_NODE_SEGMENT);
    value.num_tokens = 2;
    value.tokens = (char**)malloc(value.num_tokens * sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(value.tokens[i]), "dummy-token-%d", i);
    }
    value.cnt = 5;
    value.keyvals = (orte_gpr_keyval_t**)malloc(5*sizeof(orte_gpr_keyval_t*));
    for (i=0; i < 5; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        (value.keyvals[i])->key = strdup(keys[i]);
    }
    (value.keyvals[0])->type = ORTE_INT32;
    (value.keyvals[0])->value.i32 = 654321;
    (value.keyvals[1])->type = ORTE_INT16;
    (value.keyvals[1])->value.i16 = 128;
    for (i=2; i < 5; i++) {
        (value.keyvals[i])->type = ORTE_INT32;
        (value.keyvals[i])->value.i32 = i * 10;
    }
    values = &value;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    /* now let's update a few of them */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_OVERWRITE |
                      ORTE_GPR_TOKENS_XAND |
                      ORTE_GPR_KEYS_OR;
    value.segment = strdup(ORTE_NODE_SEGMENT);
    value.num_tokens = 2;
    value.tokens = (char**)malloc(value.num_tokens * sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(value.tokens[i]), "dummy-token-%d", i);
    }
    value.cnt = 3;
    value.keyvals = (orte_gpr_keyval_t**)malloc(3*sizeof(orte_gpr_keyval_t*));
    for (i=0; i < 3; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        (value.keyvals[i])->key = strdup(keys[i]);
    }
    (value.keyvals[0])->type = ORTE_INT32;
    (value.keyvals[0])->value.i32 = 123456;
    (value.keyvals[1])->type = ORTE_INT16;
    (value.keyvals[1])->value.i16 = 904;
    (value.keyvals[2])->type = ORTE_STRING;
    (value.keyvals[2])->value.strptr = strdup("idiot-string");
    values = &value;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    orte_finalize();

    printf("Registry finalized.\n");

    return(0);
}
