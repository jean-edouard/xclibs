#
# Copyright (c) 2013 Citrix Systems, Inc.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

# Makefile.am

SRCS = xcxenstore.h xcxenstore.c

libxcxenstore_la_CFLAGS = -W -Wall -Werror -D_GNU_SOURCE
libxcxenstore_la_SOURCES = ${SRCS}
libxcxenstore_la_LDFLAGS = \
	-version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) \
	-release $(LT_RELEASE) \
	-export-dynamic \
	@LIBEVENT_LIB@

lib_LTLIBRARIES = libxcxenstore.la

libxcxenstore_includedir = $(includedir)/xcxenstore
libxcxenstore_include_HEADERS = xcxenstore.h

VFD = ${srcdir}/..
VF = ${shell cat ${VFD}/version-files}
VFS = ${VF:%=${VFD}/%}
VCHK = ${shell cat ${VFS} | @MD5SUM@ | @AWK@ '{ print $$1 }' }
VNUM = ${shell @GREP@ ${VCHK} ${VFD}/version-md5sums | @AWK@ '{ print $$2 }'  }
VDEF = ${shell echo `cat ${VFD}/version-major`.`cat ${VFD}/version-minor`.`cat ${VFD}/version-micro` }

version.h: $(VFD)/version-files $(VFD)/version-major \
	$(VFD)/version-minor $(VFD)/version-micro \
	$(VFD)/version-md5sums ${VFS} Makefile
	if [ .${VNUM} = . ]; then \
		echo "#define FULL_VERSION \"libxcxenstore Version ${VDEF} + Edits\"" > version.h; \
		echo ${VDEF}-E > version-num; \
	else \
		echo "#define FULL_VERSION \"libxcxenstore Version ${VNUM}\"" > version.h; \
		echo ${VNUM} > version-num; \
	fi
