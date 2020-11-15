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
RUNTIME=../runtime

kal3: kal4.c kal4syntax.c kal4tab.c
	$(CC) -o kal4 kal4.c kal4tab.c kal4syntax.c $(CFLAGS)

.PHONY: clean
clean:
	$(RM) kal4
	$(RM) kal4syntax.c
	$(RM) y.tab.h y.output

.PHONY: deploy
deploy: kal4
	cp -f kal4 $(RUNTIME)

.PHONY: distclean
distclean: clean
	$(RM) ${RUNTIME}/kal4
