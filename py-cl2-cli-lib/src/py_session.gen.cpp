static void del_Session(obj_Session *self) { destroyAnySession(self); PyObject_Del(self); }
