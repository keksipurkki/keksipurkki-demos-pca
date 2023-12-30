module assertions
  contains

    subroutine assert(ok, message)
      logical, intent(in) :: ok
      character(*), intent(in) :: message
      if (ok) then
        return
      else
        print *, message
        stop 1
      endif
    end subroutine

end module
