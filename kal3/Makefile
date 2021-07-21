#------------------------------------------------------------------------
#    This file is part of ee9.
#
#    ee9 is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    ee9 is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with ee9.  If not, see <https://www.gnu.org/licenses/>.
#------------------------------------------------------------------------
CC=gcc
YACC=bison
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-vdt

prefix?=/usr/local

kal3: kal3.c kal3.tab.c
	$(CC) -o $@ $^ $(CFLAGS)

kal3.tab.c: kal3.y
	$(YACC) $(YFLAGS) $^

.PHONY: clean
clean:
	$(RM) kal3
	$(RM) *.tab.[ch]* y.output
	$(RM) *.output

.PHONY: install
install: kal3
	install -m 755 kal3 $(DESTDIR)$(prefix)/bin/

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/kal3

.PHONY: update
update:
#	wget -N http://settle.ddns.net/KDF9/kal3.c --no-if-modified-since
#	-git apply fixpaths.diff
#	wget -N http://settle.ddns.net/KDF9/kal3.y --no-if-modified-since

.PHONY: patch
patch:
	cat fixpaths.diff | (patch -r - --no-backup-if-mismatch --forward || true)
