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
CFLAGS=-I. -fPIE -fPIC
RUNTIME=../runtime

mkchan: 
	$(CC) -o mkchan mkchan.c $(CFLAGS)

.PHONY: clean
clean:
	$(RM) mkchan

.PHONY: deploy
deploy: mkchan
	cp -f mkchan $(RUNTIME)

.PHONY: distclean
distclean: clean
	$(RM) ${RUNTIME}/mkchan

.PHONY: check
check: 
