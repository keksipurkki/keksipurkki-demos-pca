MODULE PUTSTRMODULE ! DUMMY VERSION
  ! An auxilliary module that accompanies DISPMODULE. This module contains dummy versions of the
  ! subroutines putstr and putnl that do nothing. It is needed to avoid an "undefined symbol" link
  ! error for these. In addition it defines the named constant (or parameter) DEFAULT_UNIT = -3,
  ! which makes the asterisk unit (usually the screen) the default to display on.
  !
  ! The purpose of having this module is to make displaying possible in situations where ordinary
  ! print- and write-statements do not work. Then this module should be replaced by one defining
  ! functional versions of putstr and putnl. An example is given by the commented out PUTSTRMODULE
  ! for Matlab mex files below.
  !
  integer, parameter :: DEFAULT_UNIT = -3
  !
CONTAINS
  subroutine putstr(s)
    character(*), intent(in) :: s
    integer ldummy, ldummy1  ! these variables exist to avoid unused variable warnings
    ldummy = len(s)
    ldummy1 = ldummy
    ldummy = ldummy1
  end subroutine putstr
  subroutine putnl()
  end subroutine putnl
END MODULE PUTSTRMODULE
