module utils
  use iso_fortran_env
  use assertions
  implicit none

  contains

    !! Column-wise zero mean and unit variance
    pure function normalize(x) result(r)
      real(real64), intent(in) :: x(:,:)
      real(real64), allocatable :: r(:,:)
      real(real64), allocatable :: mean(:), variance(:)
      integer :: i, n

      r = x
      n = size(r, 1)

      mean = sum(r, 1) / n
      variance = sum((r - spread(mean, 1, n))**2, 1) / n

      do i = 1, n
        r(i,:) = (r(i,:) - mean) / sqrt(variance)
      enddo

    end function

    function grayscale(rgb) result(r)
      real(real64), intent(in) :: rgb(:,:)
      real(real64), allocatable :: r(:,:), tmp(:,:)
      integer :: n, k, i
      call assert(mod(size(rgb, 2), 3) == 0, 'Expected column rank to be divisible by 3')

      n = size(rgb, 1)
      k = size(rgb, 2) / 3
      allocate(r(n, k), source=0.0d0)

      do i = 1, n
        tmp = reshape(rgb(i,:), [3, k])
        r(i,:) = sum(tmp, 1) / 3
      enddo

    end function

    function country_code(fname)
      character(len=*), intent(in) :: fname
      character(:), allocatable :: country_code
      integer :: start, end
      country_code = trim(fname)
      start = scan(country_code, '/', back=.false.)
      end = scan(country_code, '.', back=.true.)
      country_code = country_code(start + 1:end - 1)
    end function

    function country_emoji(code)
      character(len=2), intent(in) :: code
      integer, parameter :: ucs2 = selected_char_kind('ISO_10646')
      character(kind=ucs2, len=2) :: country_emoji
      integer, allocatable :: codepoints(:)
      integer, parameter :: offset = 127365
      integer :: i
      codepoints = [( ichar(code(i:i)) + offset, i = 1, len(code) )]
      country_emoji = transfer(codepoints, country_emoji)
    end function

    function cli_arguments()
      integer :: i
      character (len=32) :: arg
      character(len=32) :: cli_arguments(command_argument_count())
      do i = 1, size(cli_arguments)
        call get_command_argument(i, arg)
        cli_arguments(i) = trim(arg)
      end do
    end function

end module
