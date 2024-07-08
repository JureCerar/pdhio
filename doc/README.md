## Example

__Work in progress__

```fortran
use pdhio
type(pdh_t) :: a, b, w, l
character(128) :: message
integer :: i, num_iter, status

! Allocate, reallocate, and deallocate data
call a%allocate(100, STAT=status, ERRMSG=message)
call a%reallocate(200, STAT=status, ERRMSG=message)
call a%deallocate(STAT=status, ERRMSG=message)

! Check data allocation and size
if (a%is_allocated()) then
    print *, a%size()
end if

! Load and save files
call a%load("input.pdh", STAT=status, ERRMSG=message)
call a%save("output.pdh", STAT=status, ERRMSG=message)

! Get PDH information
print *, a%title
print *, a%key_words(:)
print *, a%int_const(:)
print *, a%real_const(:)
do i = 1, a%size()
    print *, a%x(i), a%y(i), a%err(i)
end do 

! Read mock data
call a%load("input.pdh", STAT=status, ERRMSG=message)
call b%load("input.pdh", STAT=status, ERRMSG=message)

! Basic math operations
a = a + b
a = a - b
a = a * b
a = a / b

! Normalize to area
call a%normalize(STAT=status, ERRMSG=message)

! Data binning
call a%binning(250, STAT=status, ERRMSG=message)

! Load profile data
call a%load("input.pdh", STAT=status, ERRMSG=message)
call w%load("width.pdh", STAT=status, ERRMSG=message)
call l%load("length.pdh", STAT=status, ERRMSG=message)

! Smearing
call a%smear(WIDTH=w, LENGTH=l, STAT=status, ERRMSG=message)

! De-semaring (Lake's algorithm)
num_iter = 30
call a%desmear(num_iter, WIDTH=w, LENGTH=l, STAT=status, ERRMSG=message)
```