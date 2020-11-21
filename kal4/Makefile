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
YFLAGS=--debug

prefix?=/usr/local

kal4: kal4.c kal4.tab.c kal4tab.c 
	$(CC) -o $@ $^ $(CFLAGS)

kal4.tab.c: kal4.y
	$(YACC) --defines=kal4.tab.h $^ $(YFLAGS)

.PHONY: clean
clean:
	$(RM) kal4
	$(RM) *.tab.[ch] y.output

.PHONY: install
install: kal4
	install -m 755 kal4 $(DESTDIR)$(prefix)/bin/

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/kal4

.PHONY: update
update:
	wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/kal4.c --no-if-modified-since
	wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/kal4.h --no-if-modified-since
	wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/kal4.y --no-if-modified-since
	wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/kal4tab.c --no-if-modified-since