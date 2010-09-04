#include "bb_blackboard.h"

#include "er_errors.h"

typedef struct {
  enum bb_DataType dt;
  bb_Closure cb;
} Registrant;

struct _bb_Blackboard
{
  // In some sense the name of this data structure is misleading, as
  // we do not actually maintain a "blackboard" of data.
public:
  GSList* reg; // of Registrant
};

extern "C"
bb_Blackboard* bb_Blackboard_new(GError** error)
{
  bb_Blackboard* self = g_try_new0(bb_Blackboard, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  // self->reg is left as NULL, meaning an empty list.
  return self;
}

static void Registrant_free(gpointer self, gpointer dummy)
{
  g_slice_free(Registrant, self);
}

extern "C"
void bb_Blackboard_destroy(bb_Blackboard* self)
{
  if (self) {
    if (self->reg) {
      g_slist_foreach(self->reg, Registrant_free, NULL);
      g_slist_free(self->reg);
    }
    g_free(self);
  }
}

static void Registrant_free(Registrant* self)
{
  if (self)
    g_slice_free(Registrant, self);
}

extern "C"
gboolean bb_Blackboard_register(bb_Blackboard* self,
				enum bb_DataType dt,
				bb_Closure cb,
				GError** error)
{
  Registrant* elem = NULL;

  SET_TRAP_OOM(goto fail);
  elem = g_slice_new(Registrant);
  elem->dt = dt;
  elem->cb = cb;
  self->reg = g_slist_prepend(self->reg, elem);
  UNSET_TRAP_OOM();

  return TRUE;

#if HAVE_TRAP_OOM
 fail:
  Registrant_free(elem);
  if (error) *error = gx_error_no_memory;
  return FALSE;
#endif
}

extern "C"
void bb_Blackboard_deregister(bb_Blackboard* self,
			      bb_Closure cb)
{
  GSList* plink = NULL;
  GSList* link = self->reg;
  while (link != NULL) {
    Registrant* elem = (Registrant*)link->data;
    if (elem->cb.changed == cb.changed) {
      Registrant_free(elem);
      link = g_slist_delete_link(link, link);
      if (plink)
	plink->next = link;
    } else {
      plink = link;
      link = link->next;
    }
  }
}

extern "C"
void bb_Blackboard_notify(bb_Blackboard* self,
			  enum bb_DataType dt,
			  gpointer data, int len)
{
  for (GSList* link = self->reg; link != NULL; link = link->next) {
    Registrant* elem = (Registrant*)link->data;
    if (elem->dt == dt) {
      (*(elem->cb.changed))(self, dt, data, len, elem->cb.arg);
    }
  }
}
