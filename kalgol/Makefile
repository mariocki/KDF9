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

kalgol: 

.PHONY: clean
clean:

.PHONY: deploy
deploy: 
	${RUNTIME}/a2b -r2p < mksys2.bin >${RUNTIME}/Binary/MKSYS2
	${RUNTIME}/a2b -r2p < KAB00.bin >${RUNTIME}/Binary/KAB00DH--USU

.PHONY: distclean
distclean: clean
	$(RM) ${RUNTIME}/Binary/MKSYS2
	$(RM) ${RUNTIME}/Binary/KAB00DH--USU

.PHONY: check
check: 
