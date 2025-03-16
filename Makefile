########################################################################
## Makefile
##
## lip - lisp in python
##       https://github.com/minmus-9/lip
## Copyright (C) 2025  Mark Hays (github:minmus-9)
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

all:
	(cd examples && $(MAKE) all)

clean:
	rm -f profile
	find . -type d -name __pycache__ -print0 | xargs -0 -n 25 rm -rf || true

## EOF
