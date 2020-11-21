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

kal4: kal4.c kal4syntax.c kal4tab.c
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean
clean:
	$(RM) kal4
	$(RM) kal4syntax.c
	$(RM) y.tab.h y.output

.PHONY: install
install: kal4
	install -m 755 kal4 $(DESTDIR)$(prefix)/bin/

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/kal4