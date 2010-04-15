/* 
 * Copyright (C) 2006 Nokia Corporation.
 * Copyright 2010 Helsinki Institute for Information Technology (HIIT)
 * and Tero Hasu <tero.hasu@hut.fi>.
 *
 * This is a derived work of SET_LOW_MEMORY_TRAP as found in glowmem.h
 * by Nokia Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#ifndef __glowmem_action_h__
#define __glowmem_action_h__

#include <glowmem.h>

#define SET_LOW_MEMORY_TRAP_ACTION(action)      \
gboolean did_i_set = FALSE;\
{\
        mem_info *m = _get_thread_specific_data();\
        if(m == NULL)\
        {\
                m = (mem_info *)_pAlloc(sizeof(mem_info));\
                if(!m) { action; } \
                m->is_setjmp_called = FALSE;\
                _clearCleanUpStack(&(m->stack));\
                if(_set_thread_specific_data(m))\
                        { action; } \
        }\
        if(!m->is_setjmp_called)\
        {\
                if(setjmp(m->buf) > 0)\
                {\
                        m->is_setjmp_called = FALSE;\
                        _destroCleanUpStack(&(m->stack));\
                        { action; } \
                }\
                m->is_setjmp_called = TRUE;\
                did_i_set = TRUE;\
        }\
}

#endif /* __glowmem_action_h__ */
