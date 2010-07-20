/*
 * This code is extracted and adapted from GLib's gerror.c,
 * and can be considered a derived work.
 *
 * Adaptation at HIIT 2010. Relevant portions
 * Copyright 2010 Helsinki Institute for Information Technology (HIIT)
 * and Tero Hasu <tero.hasu@hut.fi>.
 */

/*
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 * Portions copyright (c) 2006 Nokia Corporation.  All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include "common/gxerror.h"

#include "common/gxlowmem.h"

#include <glib.h>

GError* 
gx_error_new_valist (GQuark         domain,
		     gint           code,
		     const gchar   *format,
		     va_list        args)
{
  GError *error = NULL;
  SET_TRAP_OOM_FAIL();

  error = g_new (GError, 1);
  
  error->domain = domain;
  error->code = code;
  error->message = g_strdup_vprintf (format, args);
  
  UNSET_TRAP_OOM();
  return error;

 fail:
  if (error) {
    g_free(error);
  }
  return NULL;
}

GError*
gx_error_new (GQuark       domain,
	      gint         code,
	      const gchar *format,
	      ...)
{
  GError* error;
  va_list args;

  g_return_val_if_fail (format != NULL, NULL);
  g_return_val_if_fail (domain != 0, NULL);

  va_start (args, format);
  error = gx_error_new_valist (domain, code, format, args);
  va_end (args);

  return error;
}

GError*
gx_error_new_literal (GQuark         domain,
		      gint           code,
		      const gchar   *message)
{
  GError* err;
  SET_TRAP_OOM_VALUE(NULL);
  err = g_error_new_literal(domain, code, message);
  UNSET_TRAP_OOM();
  return err;
}
