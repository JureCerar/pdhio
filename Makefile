# This file is part of pdhio
# https://github.com/JureCerar/pdhio
#
# Copyright (C) 2024 Jure Cerar
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Compiler & Linker
FC ?= gfortran
FC_FLAGS := -g -O3 -march=native -Wall -fcheck=all -fPIC 
# Install
INSTALL := install
INSTALL_DIR ?= /usr/local
# Source
SRC := src/math.f90 src/pdhio.f90
OBJ := $(SRC)
OBJ := $(OBJ:.F90=.o)
OBJ := $(OBJ:.f90=.o)
# Library
NAME = pdhio 
LIB = libpdhio.so

default: all

.PHONY: all
all: $(LIB)

$(LIB): $(OBJ)
	$(FC) $^ -shared $(FC_FLAGS) -o $@

src/%.o: src/%.f90
	$(FC) -c $< $(FC_FLAGS) -o $@ -J src/

src/%.o: src/%.F90
	$(FC) -c $< $(FC_FLAGS) -o $@ -J src/

.PHONY: test
test: $(LIB)
	$(FC) tests/test.f90 -o test -I src -L . -lpdhio
	@LD_LIBRARY_PATH=. ./test tests/data.pdh

.PHONY: package
package:
	@INSTALL_DIR=$(INSTALL_DIR) envsubst < src/pdhio.in > pdhio.pc

.PHONY: install
install: $(LIB) package
	$(INSTALL) $(LIB) $(INSTALL_DIR)/lib
	$(INSTALL) $(NAME).pc $(INSTALL_DIR)/lib/pkgconfig
	$(INSTALL) $(NAME).mod $(INSTALL_DIR)/include

.PHONY: clean
clean:
	@rm -rf src/*.mod src/*.o *.pc test $(LIB)

.PHONY: help	
help:
	@$(foreach V,$(sort $(.VARIABLES)), \
	$(if $(filter-out environment% default automatic, \
	$(origin $V)),$(warning $V=$($V) ($(value $V)))))