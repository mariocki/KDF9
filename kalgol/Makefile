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
	install -d $(DESTDIR)$(prefix)/share/kdf9/Data
	install -m 644 mksys2.bin KAB00.bin $(DESTDIR)$(prefix)/share/kdf9/Data/
	install -m 644 systape.txt $(DESTDIR)$(prefix)/share/kdf9/Data/systape_kalgol.txt 
	
.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/Data/systape_kalgol.txt
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/Data/mksys2.bin
	$(RM) $(DESTDIR)$(prefix)/share/kdf9/Data/KAB00.bin
	
.PHONY: update
update:
	#wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/systape.txt --no-if-modified-since
	#wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/KAB00.bin --no-if-modified-since
	##wget -N http://settle.ddns.net/KDF9/kalgol/DavidHo/mksys2.bin --no-if-modified-since
