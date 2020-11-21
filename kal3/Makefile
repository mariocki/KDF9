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
YACC=yacc
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-d --debug

prefix?=/usr/local

kal3: kal3.c kal3syntax.c
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean
clean:
	$(RM) kal3
	$(RM) kal3syntax.c
	$(RM) y.tab.h y.output

.PHONY: install
install: kal3
	install -m 755 kal3 $(DESTDIR)$(prefix)/bin/

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/kal3