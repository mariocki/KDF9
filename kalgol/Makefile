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

prefix?=/usr/local

kalgol: 

.PHONY: clean
clean:

.PHONY: install
install: 
	install -d $(DESTDIR)$(prefix)/share/kdf9/runtime/Data/
	install -m 644 systape_kalgol.txt mksys2.bin KAB00.bin $(DESTDIR)$(prefix)/share/kdf9/runtime/Data/
	
.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/runtime/Data/systape_kalgol.txt
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/runtime/Data/mksys2.bin
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/runtime/Data/KAB00.bin
	