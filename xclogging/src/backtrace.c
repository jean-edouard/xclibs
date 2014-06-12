/*
 * Copyright (c) 2011 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <dlfcn.h>
#include <syslog.h>
#include <execinfo.h>

#include "backtrace.h"

void
dump_backtrace (void)
{
  void *ba[256];
  Dl_info info;
  int i;

  int n = backtrace (ba, sizeof (ba) / sizeof (ba[0]));
  if (!n)
    return;

  for (i = 0; i < n; ++i)
    {
      if (dladdr (ba[i], &info))
        {
          char *base, *offset;

          base = info.dli_saddr;
          offset = ba[i];

          syslog (LOG_ERR, "[Fatal] backtrace: (%s %s+0x%x)", info.dli_fname,
                  info.dli_sname, (unsigned int) (offset - base));
        }
    }
}

