
typedef int FLAG_t;

enum TRIGGER_ACTION_STATE {
  TRIGGER_PENDING=-1,
  ACTION_SUCCESS=0,
  ACTION_PROBLEM=1,
};

typedef int
  (*trigger_triggered_fn_t)
  (void*);
typedef enum TRIGGER_ACTION_STATE
  (*trigger_action_cb_fn_t)
  (void*);
typedef void
  (*trigger_reset_fn_t)
  (void*);

struct triggerable_t {
  trigger_triggered_fn_t triggered;
  void *trigger;
  trigger_action_cb_fn_t action;
  void *action_cbstate;
  trigger_reset_fn_t reset;
  int auto_reset;
};
typedef struct triggerable_t triggerable_t;

void trigger_reset(triggerable_t thing);

enum TRIGGER_ACTION_STATE trigger_test(triggerable_t thing);

/* some built-in trigger functions */
int triggered_bynonzero_int(void *trigger);
int triggered_byzero_int(void *trigger);
void reset_to_zero_int(void *trigger);
enum TRIGGER_ACTION_STATE action_noop(void *cbstate);
