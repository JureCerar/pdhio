# PDHIO

The __pdhio__ is a modern Fortran library for reading and processing PDH (Primary Data Handling) files for scattering data. The PDH format is the native format for the PCG (Physical Chemistry Graz) SAXS software developed at the University of Graz by Glatter _et al._

This library was written during my PhD studies long time ago. If anyone else has any use for it so much better. After so many years there are still some things to do: 

* [ ] Documentation (seriously dude?! After so many years no docs)
* [ ] Add (proper) tests
* [ ] Add smoothing functions (savgol?)
* [ ] Porod & Guinier fits

## Build & Install

Clone repository and type the following:

```bash
make
make install
```
__NOTE:__ To install the library you may need to run command as *root* (or *sudo*).

## Using pdhio in your project

To use the library in your project add `use pdhio` in the modules section of your program:

```fortran
program main
    use pdhio
    implicit none
    ! ...
end program main
```

When compiling your program add `-lpdhio` flag to compiler options. You may also need to use `-I` flag to point to where the modules files are (default `-I/usr/local/include`) even with all of the right environment variables set. When linking use `-L` to point to library file (default `-L/usr/local/lib`).

To make things easier [pkg-config](https://linux.die.net/man/1/pkg-config) file is also included to help you with your program compilation. You may need to add the config file to `PKG_CONFIG_PATH` environment variable (default `/usr/local/lib/pkgconfig`).

```bash
pkg-config pdhio --libs --cflags
```

## [Documentation](doc/README.md)

Documentation is a work in progress (as always with my project) and is available at [doc/README.md](doc/README.md). This includes API documentation and example on how to use library.

## License

This program is licensed under the __GNU General Public License v3.0__

Copyright (C) 2024 [Jure Cerar](https://github.com/JureCerar)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.
