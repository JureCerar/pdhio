! This file is part of pdhio
! https://github.com/JureCerar/pdhio
!
! Copyright (C) 2024 Jure Cerar
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

module pdhio
    use math
    implicit none
    private
    public :: pdh_t

    ! Global constants
    integer, parameter :: ERR_LEN = 128

    ! .PDH file format
    type pdh_t
        character(80)     :: title = ""
        character(4)      :: key_words(16) = ""
        integer           :: int_const(8) = 0
        real              :: real_const(10) = 0.0
        real, allocatable :: x(:), y(:), err(:)
    contains
        procedure :: is_allocated => pdh_is_allocated
        procedure :: size => pdh_size
        procedure, private :: check => pdh_check
        procedure :: allocate => pdh_allocate
        procedure :: reallocate => pdh_reallocate
        procedure :: deallocate => pdh_deallocate
        procedure, private :: pdh_write_formatted
        generic :: write(formatted) => pdh_write_formatted
        procedure :: save => pdh_save
        procedure :: load => pdh_load
        procedure, private :: pdh_assing
        generic :: assignment(=) => pdh_assing
        procedure, private :: pdh_add
        generic :: operator(+) => pdh_add
        procedure, private :: pdh_subtract
        generic :: operator(-) => pdh_subtract
        procedure, private :: pdh_multiply
        generic :: operator(*) => pdh_multiply
        procedure, private :: pdh_divide
        generic :: operator(/) => pdh_divide
        procedure :: normalize => pdh_normalize
        procedure :: binning => pdh_binning
        procedure :: smear => pdh_smear
        procedure :: desmear => pdh_desmear
    end type pdh_t

    ! Constructor
    interface pdh_t
        module procedure :: pdh_constructor
    end interface pdh_t

contains


! comment
function pdh_is_allocated (this) result (result)
  implicit none
  class(pdh_t), intent(inout) :: this
  logical :: result

  result = allocated(this%x) .and. allocated(this%y) .and. allocated(this%err)

end function pdh_is_allocated


! comment
function pdh_size (this) result (result)
    implicit none
    class(pdh_t), intent(inout) :: this
    integer :: result

    if (allocated(this%x) .and. allocated(this%y) .and. allocated(this%err)) then
        result = size(this%x)
        if (result /= size(this%y) .or. result /= size(this%err)) then
            error stop "Data lengths do not match"
        end if
    else
        result = 0
    end if

end function pdh_size


! comment
subroutine pdh_check(this, stat, errmsg)
    implicit none
    class(pdh_t), intent(IN) :: this
    integer, intent(OUT), OPTIONAL :: stat 
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    integer :: num_pointes, status

    catch: block

        status = 0

        if (.not. (allocated(this%x) .and. allocated(this%y) .and. allocated(this%err))) then
            message = "Data not allocated"
            status = 1
            exit catch
        end if

        num_pointes = size(this%x)
        if (num_pointes /= size(this%y) .or. num_pointes /= size(this%err)) then
            message = "Data lengths do not match"
            status = 1
            exit catch
        end if

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_check


! comment
type(pdh_t) function pdh_constructor (num_points, title, key_words, int_const, real_const, x, y, err) result (this)
    implicit none
    integer, intent(in) :: num_points
    character(*), intent(in), optional :: title
    character(*), intent(in), optional :: key_words(16)
    integer, intent(in), optional :: int_const(8)
    real, intent(in), optional :: real_const(10)
    real, intent(in), optional :: x(num_points)
    real, intent(in), optional :: y(num_points)
    real, intent(in), optional :: err(num_points)

    call this%allocate(num_points)
    if (present(title)) this%title = title
    if (present(key_words)) this%key_words = key_words
    if (present(int_const)) this%int_const = int_const
    if (present(real_const)) this%real_const = real_const
    if (present(x)) this%x = x
    if (present(y)) this%y = y
    if (present(err)) this%err = err

    ! First INT constant is number of points 
    this%int_const(1) = num_points

end function pdh_constructor


! comment
subroutine pdh_allocate (this, num_points, stat, errmsg)
    implicit none
    class(pdh_t), intent(inout) :: this
    integer, intent(in) :: num_points
    integer, intent(out), optional :: stat 
    character(*), intent(out), optional :: errmsg
    character(ERR_LEN) :: message
    integer :: status

    catch: block

        if (num_points < 1) then
            message = "Invalid number of points"
            status = 1
            exit catch
        end if

        if (allocated(this%x)) deallocate (this%x, STAT=status, ERRMSG=message)
        allocate (this%x(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        if (allocated(this%y)) deallocate (this%y, STAT=status, ERRMSG=message)
        allocate (this%y(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        if (allocated(this%err)) deallocate (this%err, STAT=status, ERRMSG=message)
        allocate (this%err(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        ! First INT constant is number of points 
        this%int_const(1) = num_points

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_allocate


! comment
subroutine pdh_reallocate (this, num_points, stat, errmsg)
    implicit none
    class(pdh_t), intent(inout) :: this
    integer, intent(in) :: num_points
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    real, allocatable :: temp(:)
    character(ERR_LEN) :: message
    integer :: i, status

    catch: block

        if (num_points < 1) then
            message = "Invalid number of points"
            status = 1
            exit catch
        end if

        if (allocated(this%x)) then
            allocate (temp(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            i = min(num_points, size(this%x))
            temp(:i) = this%x(:i)
            call move_alloc (temp, this%x)
        else
            allocate (this%x(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
        end if

        if (allocated(this%y)) then
            allocate (temp(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            i = min(num_points, size(this%y))
            temp(:i) = this%y(:i)
            call move_alloc (temp, this%y)
        else
            allocate (this%y(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
        end if

        if (allocated(this%err)) then
            allocate (temp(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            i = min(num_points, size(this%err))
            temp(:i) = this%err(:i)
            call move_alloc (temp, this%err)
        else
            allocate (this%err(num_points), SOURCE=0.0, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
        end if

        ! First INT constant is number of points 
        this%int_const(1) = num_points

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_reallocate


! comment
subroutine pdh_deallocate (this, stat, errmsg)
    implicit none
    class(pdh_t), intent(inout) :: this
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    character(ERR_LEN) :: message
    integer :: status

    catch: block
    
        status = 0

        if (allocated(this%x)) deallocate (this%x, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        if (allocated(this%y)) deallocate (this%y, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        if (allocated(this%err)) deallocate (this%err, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        this%int_const(1) = 0

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_deallocate


! comment
subroutine pdh_write_formatted (this, unit, iotype, v_list, iostat, iomsg)
    implicit none
    class(pdh_t), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype 
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i, num_points

    catch: block

        call this%check(STAT=iostat, ERRMSG=iomsg)
        if (iostat /= 0) exit catch

        num_points = size(this%x)

        if (iotype == "LISTDIRECTED") then  

            write (unit, "(a80,/)", IOSTAT=iostat, IOMSG=iomsg) this%title
            if (iostat /= 0) exit catch

            write (unit, "(16(a4,x),/)", IOSTAT=iostat, IOMSG=iomsg) this%key_words
            if (iostat /= 0) exit catch

            write (unit, "(8(i9,x),/)", IOSTAT=iostat, IOMSG=iomsg) num_points, this%int_const(2:)
            if (iostat /= 0) exit catch

            write (unit, "(5(e14.6,x),/)", IOSTAT=iostat, IOMSG=iomsg) this%real_const(1:5)
            if (iostat /= 0) exit catch

            write (unit, "(5(e14.6,x),/)", IOSTAT=iostat, IOMSG=iomsg) this%real_const(6:10)
            if (iostat /= 0) exit catch
                
            do i = 1, num_points
                write (unit, "(3(1pe14.6,x),/)", IOSTAT=iostat, IOMSG=iomsg) this%x(i), this%y(i), this%err(i)
                if (iostat /= 0) exit catch
            end do   
            
        else
            iostat = 1
            iomsg = "Unsupported iotype"
            exit catch

        end if

    end block catch

end subroutine pdh_write_formatted


! comment
subroutine pdh_save (this, file, stat, errmsg)
    implicit none
    class(pdh_t), intent(IN) :: this
    character(*), intent(IN) :: file
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    integer :: i, num_points, unit, status

    catch: block

        call this%check(STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        num_points = size(this%x)

        open (NEWUNIT=unit, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch

        write (unit, "(a80)", IOSTAT=status, IOMSG=message) this%title
        if (status /= 0) exit catch

        write (unit, "(16(a4,x))", IOSTAT=status, IOMSG=message) this%key_words
        if (status /= 0) exit catch

        write (unit, "(8(i9,x))", IOSTAT=status, IOMSG=message) num_points, this%int_const(2:)
        if (status /= 0) exit catch

        write (unit, "(5(e14.6,x))", IOSTAT=status, IOMSG=message) this%real_const(1:5)
        if (status /= 0) exit catch

        write (unit, "(5(e14.6,x))", IOSTAT=status, IOMSG=message) this%real_const(6:10)
        if (status /= 0) exit catch

        do i = 1, num_points
            write (unit, "(3(1pe14.6,x))", IOSTAT=status, IOMSG=message) this%x(i), this%y(i), this%err(i)
            if (status /= 0) exit catch
        end do    

        close (unit, IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_save


! comment
subroutine pdh_load (this, file, stat, errmsg)
    implicit none
    class(pdh_t), intent(inout) :: this
    character(*), intent(in) :: file
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    character(ERR_LEN) :: message
    integer :: i, unit, num_points, status

    catch: block

        open (NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch

        read (unit, "(a80)", IOSTAT=status, IOMSG=message) this%title
        if (status /= 0) exit catch

        read (unit, "(16(a4,x))", IOSTAT=status, IOMSG=message) this%key_words
        if (status /= 0) exit catch

        read (unit, "(8(i9,x))", IOSTAT=status, IOMSG=message) this%int_const
        if (status /= 0) exit catch

        read (unit, "(5(e14.6,x))", IOSTAT=status, IOMSG=message) this%real_const(1:5)
        if (status /= 0) exit catch

        read (unit, "(5(e14.6,x))", IOSTAT=status, IOMSG=message) this%real_const(6:10)
        if (status /= 0) exit catch

        num_points = this%int_const(1)
        if (num_points < 1) then
            message = "Invalid number of points"
            status = 1
            exit catch
        end if

        call this%allocate(num_points, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        do i = 1, num_points
            read (unit, "(3(e14.6,1x))", IOSTAT=status, IOMSG=message) this%x(i), this%y(i), this%err(i)
            if (status /= 0) exit catch
        end do  

        close (unit, IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_load


! comment
subroutine pdh_assing (this, other)
    implicit none
    class(pdh_t), intent(INOUT) :: this
    type(pdh_t), intent(IN) :: other
    character(ERR_LEN) :: message
    integer :: status, num_points

    call other%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    num_points = size(other%x)

    if (allocated(this%x)) deallocate (this%x, STAT=status, ERRMSG=message)
    allocate (this%x(num_points), STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    if (allocated(this%y)) deallocate (this%y, STAT=status, ERRMSG=message)
    allocate (this%y(num_points), STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    if (allocated(this%err)) deallocate (this%err, STAT=status, ERRMSG=message)
    allocate (this%err(num_points), STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    this%title = other%title
    this%key_words = other%key_words
    this%int_const = other%int_const
    this%real_const = other%real_const
    this%x = other%x
    this%y = other%y
    this%err = other%err

end subroutine pdh_assing


! comment
function pdh_add (this, other) result (result)
    implicit none
    class(pdh_t), intent(IN) :: this
    type(pdh_t), intent(IN) :: other
    type(pdh_t) :: result
    character(ERR_LEN) :: message
    integer :: status

    call this%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    call other%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    result = this

    result%y = this%y + interp1d(result%x, other%x, other%y)
    result%err = sqrt(this%err ** 2 + interp1d(result%x, other%x, other%err) ** 2)

end function pdh_add


! comment
function pdh_subtract(this, other) result (result)
    implicit none
    class(pdh_t), intent(IN) :: this
    type(pdh_t), intent(IN) :: other
    type(pdh_t) :: result
    character(ERR_LEN) :: message
    integer :: status

    call this%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    call other%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    result = this

    result%y = this%y - interp1d(result%x, other%x, other%y)
    result%err = sqrt(this%err ** 2 + interp1d(result%x, other%x, other%err) ** 2)


end function pdh_subtract


! comment
function pdh_multiply(this, other) result (result)
    implicit none
    class(pdh_t), intent(IN) :: this
    type(pdh_t), intent(IN) :: other
    type(pdh_t) :: result
    character(ERR_LEN) :: message
    real, allocatable :: y(:), err(:)
    integer :: status

    call this%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    call other%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    result = this

    y = interp1d(result%x, other%x, other%y)
    err = interp1d(result%x, other%x, other%err)

    result%y = this%y * y
    result%err = abs(result%y) * sqrt(div(this%err, this%y) ** 2 + div(err, y) ** 2) 

end function pdh_multiply


! comment
function pdh_divide(this, other) result (result)
    implicit none
    class(pdh_t), intent(IN) :: this
    type(pdh_t), intent(IN) :: other
    type(pdh_t) :: result
    real, allocatable :: y(:), err(:)
    character(ERR_LEN) :: message
    integer :: status

    call this%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    call other%check(STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    result = this

    y = interp1d(result%x, other%x, other%y)
    err = interp1d(result%x, other%x, other%err)

    result%y = div(this%y, y)
    result%err = abs(result%y) * sqrt(div(this%err, this%y) ** 2 + div(err, y) ** 2) 

end function pdh_divide


! Interpolate pdh to desired number of points
subroutine pdh_binning (this, num_points, stat, errmsg)
    implicit none
    class(pdh_t) :: this
    integer, intent(IN) :: num_points
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    real, allocatable :: x(:), y(:), err(:)
    integer :: i, i_min, i_max, status
    real :: v_min, step

    catch: block

        if (num_points < 2) then
            message = "Invalid number of points"
            status = 1
            exit catch
        end if

        call this%check(STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        allocate(x(num_points), y(num_points), err(num_points), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        i_min = minloc(this%x, DIM=1)
        i_max = maxloc(this%x, DIM=1)
        step = (this%x(i_max) - this%x(i_min)) / (num_points - 1)

        v_min = this%x(i_min)
        do i = 1, num_points
            x(i) = v_min + (i - 1) * step
        end do

        y = interp1d(x, this%x, this%y)
        err = interp1d(x, this%x, this%err)

        this%int_const(1) = num_points
        call move_alloc (x, this%x)
        call move_alloc (y, this%y)
        call move_alloc (err, this%err)

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_binning


! Normalize data to area
subroutine pdh_normalize (this, stat, errmsg)
    implicit none
    class(pdh_t) :: this
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    integer :: status

    catch: block

        call this%check(STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
    
        this%y = this%y / trapz(this%y, this%x)

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_normalize


! comment
subroutine pdh_smear (this, length, width, stat, errmsg)
    implicit none
    class(pdh_t), intent(INOUT) :: this
    type(pdh_t), intent(IN), OPTIONAL :: length
    type(pdh_t), intent(IN), OPTIONAL :: width
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    real, allocatable :: x(:), y(:), weights(:), temp(:)
    integer :: i, j, num_points, num_profile, status

    catch: block

        call this%check(STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        ! Length smearing
        if (present(length)) then
            call length%check(STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            
            num_points = size(this%x)
            allocate(temp(num_points), STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            
            num_profile = size(length%x)
            allocate(x(num_profile), y(num_profile), weights(num_profile), & 
                &   STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

            weights = length%y / trapz(length%y, length%x)

            do i = 1, num_points
                do j = 1, num_profile
                    x(j) = sqrt(this%x(i)**2 + length%x(j)**2)
                end do
                y = weights * interp1d(x, this%x, this%y)
                temp(i) = trapz(y, x)

            end do

            ! TODO: Evaluate error
            call move_alloc(temp, this%y)

            deallocate(x, y, weights, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

        end if 

        ! Width smearing
        if (present(width)) then
            call width%check(STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            
            num_points = size(this%x)
            allocate(temp(num_points), STAT=status, ERRMSG=message)
            if (status /= 0) exit catch
            
            num_profile = size(width%x)
            allocate(x(num_profile), y(num_profile), weights(num_profile), & 
                &   STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

            weights = width%y / trapz(width%y, width%x)

            do i = 1, num_points
                do j = 1, num_profile
                    x(j) = this%x(i) - width%x(j)
                end do
                y = weights * interp1d(x, this%x, this%y)
                temp(i) = abs(trapz(y, x))
            end do

            ! TODO: Evaluate error
            call move_alloc(temp, this%y)

            deallocate(x, y, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

        end if
    
    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_smear


! Lake desmearing
! TODO: add tolerance `tol` to exit prematurely.
subroutine pdh_desmear (this, num_iter, length, width, stat, errmsg)
    implicit none
    class(pdh_t), intent(INOUT) :: this
    integer, intent(IN) :: num_iter
    type(pdh_t), intent(IN), OPTIONAL :: length
    type(pdh_t), intent(IN), OPTIONAL :: width
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    integer :: i, num_points, status
    real, allocatable :: next(:), current(:)
    type(pdh_t) :: smeared

    catch: block

        call this%check(STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        num_points = size(this%x)
        allocate(next(num_points), current(num_points), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        smeared = this
        call smeared%smear(length, width, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        
        ! Initial guess
        next = 2 * this%y - smeared%y

        do i = 1, num_iter
            current = next
            smeared%y = current
            call smeared%smear(length, width, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

            next = current + this%y - smeared%y

        end do

        this%y = next

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine pdh_desmear

end module pdhio

