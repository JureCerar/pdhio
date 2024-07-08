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

module math
    implicit none

contains

! Safe division
real elemental function div(a, b) result (result)
    implicit none
    real, intent(IN) :: a, b

    result = merge(a / b, 0., b /= 0)

end function div


! Trapezoidal integration
real function trapz (y, x) result (result)
    implicit none
    real, intent(IN) :: y(:), x(size(y))
    integer :: i

    result = 0.0
    do i = 1, size(y) - 1
        result = result + 0.5 * (y(i+1) + y(i)) * (x(i+1) - x(i))
    end do
 
end function trapz


! One-dimensional linear interpolation for monotonically increasing sample points
function interp1d (x, fx, fp) result (result)
    real, intent(IN) :: x(:)
    real, intent(IN) :: fx(:), fp(size(fx))
    real :: result(size(x))
    real :: slope
    integer :: i, j, np

    np = size(fx)
    do i = 1, size(x)
        if (x(i) < fx(1)) then
            ! result(i) = fp(1)
            slope = (fp(2) - fp(1)) / (fx(2) - fx(1))
            result(i) = fp(1) + slope * (x(i) - fx(1))
        else if (x(i) > fx(np)) then
            ! result(i) = fp(np)
            slope = (fp(np) - fp(np-1)) / (fx(np) - fx(np-1))
            result(i) = fp(np-1) + slope * (x(i) - fx(np-1))
        else
            do j = 1, np - 1
                if (x(i) >= fx(j) .and. x(i) <= fx(j+1)) then
                    slope = (fp(j+1) - fp(j)) / (fx(j+1) - fx(j))
                    result(i) = fp(j) + slope * (x(i) - fx(j))
                    exit
                end if
            end do
        end if
    end do

end function interp1d

end module math