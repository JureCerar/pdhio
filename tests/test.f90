! This file is part of atomlib
! https://github.com/JureCerar/atomlib
!
! Copyright (C) 2022 Jure Cerar
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

program main
    use pdhio
    implicit none
    real, parameter :: DELTA = 0.001

    call test_allocate()
    call test_constructor()
    call test_files("tests/")
    call test_math()
    call test_functions()
    call test_smearing("tests/")

contains

! comment
subroutine test_allocate()
    implicit none
    integer, parameter :: NP = 50
    type(pdh_t) :: pdh
    character(128) :: message
    integer :: status

    ! Check allocation
    call pdh%allocate(NP)
    call pdh%reallocate(2 * NP)
    call pdh%reallocate(NP / 2)
    call pdh%deallocate()

    ! Check allocation w/ error messages
    call pdh%allocate(NP, STAT=status, ERRMSG=message)
    if (status /= 0) error stop message
    call pdh%reallocate(2 * NP, STAT=status, ERRMSG=message)
    if (status /= 0) error stop message
    call pdh%reallocate(NP / 2, STAT=status, ERRMSG=message)
    if (status /= 0) error stop message
    call pdh%deallocate(STAT=status, ERRMSG=message)    
    if (status /= 0) error stop message

    ! Check allocation
    call pdh%allocate(NP)
    if (.not. allocated(pdh%x)) error stop 1
    if (.not. allocated(pdh%y)) error stop 2
    if (.not. allocated(pdh%err)) error stop 3
    if (size(pdh%x) /= NP) error stop 4 
    if (size(pdh%y) /= NP) error stop 5 
    if (size(pdh%err) /= NP) error stop 6

    ! Check reallocation
    pdh%x(1) = 1.0
    pdh%y(1) = 1.0
    pdh%err(1) = 1.0
    call pdh%reallocate(2 * NP)
    if (.not. allocated(pdh%x)) error stop 1 
    if (.not. allocated(pdh%y)) error stop 2
    if (.not. allocated(pdh%err)) error stop 3
    if (size(pdh%x) /= 2 * NP) error stop 4
    if (size(pdh%y) /= 2 * NP) error stop 5
    if (size(pdh%err) /= 2 * NP) error stop 6
    if (pdh%x(1) /= 1.0) error stop 7
    if (pdh%y(1) /= 1.0) error stop 8
    if (pdh%err(1) /= 1.0) error stop 9 
    
    ! Check de-allocation
    call pdh%deallocate()
    if (allocated(pdh%x)) error stop 1 
    if (allocated(pdh%y)) error stop 2 
    if (allocated(pdh%err)) error stop 3
    
end subroutine test_allocate


! Comment
subroutine test_constructor()
    implicit none
    integer, parameter :: NP = 50
    type(pdh_t) :: pdh
    character(128) :: message
    integer :: i, status

    ! Construct a dummy configuration
    call pdh%allocate(NP)
    pdh%title = "TITLE"
    pdh%key_words = [("XXXX", i = 1, 16)]
    pdh%int_const = [(i, i = 1, 8)]
    pdh%real_const = [(real(i), i = 1, 10)]
    pdh%x = [(real(i), i = 1, NP)]
    pdh%y = [(real(i), i = 1, NP)]
    pdh%err = [(real(i), i = 1, NP)]

    print *, pdh

    ! Construct a dummy configuration w/ constructor
    pdh = pdh_t(NP, TITLE="TITLE", KEY_WORDS=[("XXXX", i=1, 16)], &
    &   INT_CONST=[(i, i=1, 8)], REAL_CONST=[(real(i), i=1, 10)], &
    &   X=[(real(i), i=1, NP)], Y=[(real(i), i=1, NP)], ERR=[(real(i), i=1, NP)])

    print *, pdh
    
end subroutine test_constructor


! Comment
subroutine test_files(dir)
    implicit none
    character(*), intent(IN) :: dir
    integer, parameter :: NP = 50
    type(pdh_t) :: pdh
    character(128) :: message
    integer :: i, status

    ! Load file
    call pdh%load(dir // "sphere.pdh")

    ! Load file w/ error handling
    call pdh%load(dir // "sphere.pdh", STAT=status, ERRMSG=message)
    if (status /= 0) error stop message
    
    ! Check if values are correct `sphere`
    if (pdh%size() /= 1000) error stop 1 
    if (pdh%key_words(1) /= "SAXS") error stop 2
    if (pdh%int_const(2) /= 1) error stop 3
    if (abs(pdh%real_const(1) - 1.000) > DELTA) error stop 4
    if (abs(pdh%x(1) - 1.000000E-02) > DELTA) error stop 5
    if (abs(pdh%y(1) - 1.111100E-01) > DELTA) error stop 6
    if (abs(pdh%err(1) - 0.000000E+00) > DELTA) error stop 7

    ! Save file
    call pdh%save("temp.pdh")

    ! Load file w/ error handling
    call pdh%load("temp.pdh", STAT=status, ERRMSG=message)
    if (status /= 0) error stop message  

    ! Clean after yourself
    open(FILE="temp.pdh", UNIT=100, STATUS="old", IOSTAT=status)
    if (status == 0) close(100, STATUS="delete")

end subroutine test_files


! Comment
subroutine test_math()
    implicit none
    integer, parameter :: NP = 100
    type(pdh_t) :: a, b, c
    integer :: i

    ! Construct dummy data
    call a%allocate(NP)
    a%x = [(real(i), i = 1, NP)]
    a%y = [(real(i), i = 1, NP)]
    a%err = 1.0

    call b%allocate(NP)
    b%x = [(real(i), i = 1, NP)]
    b%y = [(real(i), i = 1, NP)]
    b%err = 1.0

    ! Try assignment
    c = a
    if (.not. all(c%x == a%x)) error stop 1
    if (.not. all(c%y == a%y)) error stop 2
    if (.not. all(c%err == a%err)) error stop 3

    ! Try some math
    c = a + b
    c = a - b
    c = a * b
    c = a / b

    ! Check if math is actually correct
    c = a + b
    if (c%y(1) /= 1 + 1) error stop 1
    if (c%y(NP) /= NP + NP) error stop 2
    c = a - b
    if (c%y(1) /= 1 - 1) error stop 1
    if (c%y(NP) /= NP - NP) error stop 2
    c = a * b
    if (c%y(1) /= 1 * 1) error stop 1
    if (c%y(NP) /= NP * NP) error stop 2
    c = a / b
    if (c%y(1) /= 1 / 1) error stop 1
    if (c%y(NP) /= NP / NP) error stop 2

end subroutine test_math


! Comment
subroutine test_functions()
    implicit none  
    integer, parameter :: NP = 100
    type(pdh_t) :: a, b
    integer :: i, status
    character(128) :: message
    
    ! Construct dummy data
    call a%allocate(NP)
    a%x = [(real(i), i = 1, NP)]
    a%y = [(real(i), i = 1, NP)]
    a%err = 1.0

    ! Call some functions
    b = a
    call b%normalize()
    call b%binning(NP * NP)

    ! Call some functions w/ errors
    b = a
    call a%normalize(STAT=status, ERRMSG=message)
    if (status /= 0) error stop
    call a%binning(NP * NP, STAT=status, ERRMSG=message)    
    if (status /= 0) error stop

    ! TODO:Check if function work

end subroutine test_functions


! Comment
subroutine test_smearing(dir)
    implicit none
    character(*), intent(IN) :: dir
    integer, parameter :: NP = 50
    type(pdh_t) :: input, width, length, output
    character(128) :: message
    integer :: i, status

    ! Load file
    call input%load(dir // "sphere.pdh")
    call width%load(dir // "width.pdh")
    call length%load(dir // "length.pdh")

    ! Check functions 
    output = input
    call output%smear()
    call output%smear(LENGTH=length)
    call output%smear(WIDTH=width)
    call output%smear(LENGTH=length, WIDTH=width)

    call output%desmear(1)
    call output%desmear(1, LENGTH=length)
    call output%desmear(1, WIDTH=width)
    call output%desmear(1, LENGTH=length, WIDTH=width)

    ! Check functions w/ errors
    output = input
    call output%smear(STAT=status, ERRMSG=message)
    if (status /= 0) error stop 1
    call output%smear(LENGTH=length, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 2
    call output%smear(WIDTH=width, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 3
    call output%smear(LENGTH=length, WIDTH=width, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 4
    
    call output%desmear(1, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 1
    call output%desmear(1, LENGTH=length, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 2
    call output%desmear(1, WIDTH=width, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 3
    call output%desmear(1, LENGTH=length, WIDTH=width, STAT=status, ERRMSG=message)
    if (status /= 0) error stop 4

    ! Check if smearing / de-smearing work
    output = input
    call output%smear(LENGTH=length, WIDTH=width)
    call output%save("smeared.pdh")
    call output%desmear(30, LENGTH=length, WIDTH=width)
    call output%save("desmeared.pdh")
    if (all(abs(output%y - input%y) > DELTA)) error stop 1

    ! Clean after yourself
    open(FILE="smeared.pdh", UNIT=100, STATUS="old", IOSTAT=status)
    if (status == 0) close(100, STATUS="delete")
    open(FILE="desmeared.pdh", UNIT=100, STATUS="old", IOSTAT=status)
    if (status == 0) close(100, STATUS="delete")

end subroutine test_smearing


end program main
