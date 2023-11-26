module image_class
  use iso_fortran_env
  implicit none

  type :: Image
    character(:), allocatable :: file_name
    integer, allocatable :: bytes(:)
  end type

  type :: ImageMatrix
    type(Image), allocatable :: images(:)
  end type

  contains

    function matrix(self)
      class(ImageMatrix), intent(in) :: self
      real(real64), allocatable :: matrix(:,:)
      integer :: i, rows, cols

      rows = size(self%images)
      cols = maxval([( size(self%images(i)%bytes), i = 1, size(self%images) )])

      allocate(matrix(rows, cols), source=0.0d0)

      do i = 1, size(self%images)
        matrix(i,:) = 1.0d0 * self%images(i)%bytes
      enddo

    end function

    function matrix_from_commandline_arguments(arguments) result(x)
      use, intrinsic :: iso_c_binding
      real(real64), allocatable :: x(:,:)
      character(len=*), intent(in) :: arguments(:)
      type(Image) :: images(size(arguments))
      type(ImageMatrix) :: m

      integer :: i, file_size
      character(c_char), allocatable :: buffer(:)

      do i = 1, size(arguments)
        inquire(file=arguments(i), size=file_size)
        allocate(buffer(file_size))

        open(10, file = arguments(i), access = 'stream', form = 'unformatted')
        read(10) buffer
        close(10)

        images(i) = image(file_name=arguments(i), bytes=ichar(buffer))
        deallocate(buffer)
      enddo

      m = ImageMatrix(images)
      x = matrix(m)

    end function

end module

program main
  use svd
  use image_class
  use dispmodule
  use iso_fortran_env
  implicit none

  real(real64), allocatable, target :: x(:,:), orig(:,:)
  real(real64), allocatable :: s(:), e(:), t(:,:)
  real(real64), pointer :: u(:,:), v(:,:)
  character(len=32), allocatable :: args(:)
  integer :: i, j, axis, n, p, info, job, rank
  integer, allocatable, target :: result(:,:)
  integer, pointer :: r(:)

  args = cli_arguments()
  rank = 30
  call disp('Args: ', size(args))
  call disp('Rank: ', rank)

  allocate(result(rank, size(args)), source=0)

  orig = matrix_from_commandline_arguments(args)
  x = matrix_from_commandline_arguments(args)
  u => x
  n = size(x, 1)
  p = size(x, 2)

  job = 10
  allocate(s(min(n + 1, p)))
  allocate(e(p))

  call dsvdc(x, n, p, s, e, u, v, job, info)
  call assert(info == 0, 'Singular value decomposition failed')
  call disp('Left singular vectors: ', shape(u))

  t = score_matrix(s, u, rank)

  do i = 1, size(args)
    axis = maxloc([( cosine(orig(i,:), t(:,j)), j = 1, rank )], 1)
    r => result(axis,:)
    j = findloc(r, 0, 1)
    r(j) = i
  enddo

  call principal_components(args, result)

contains

  function score_matrix(sigma, u, rank) result(t)
    integer, intent(in) :: rank
    real(real64), intent(in) :: u(:,:), sigma(:)
    real(real64), allocatable :: t(:,:)
    integer :: i
    allocate(t(size(u, 2), rank), source=0.0d0)

    do i = 1, rank
      t(:,i) = sigma(i) * u(i,:)
    enddo

  end function

  subroutine principal_components(args, result)
    character(len=32), intent(in) :: args(:)
    integer, intent(in), target :: result(:,:)
    integer :: rank, i, j
    integer, allocatable :: cluster(:)
    integer, pointer :: r(:)

    rank = size(result, 1)
    call assert(rank > 0, 'Expected a non-zero rank')

    call disp('-------------------------------------------------------------------')

    do i = 1, rank
      r => result(i, :)
      cluster = pack(r, r /= 0)

      if (size(cluster) == 0) then
        cycle
      endif

      do j = 1, size(cluster)
        write (*, '(AA)', advance='no') country_code(args(cluster(j))), " "
      enddo

      print *, ''
    enddo

    call disp('-------------------------------------------------------------------')
  end subroutine

  function country_code(fname)
    character(len=*), intent(in) :: fname
    character(:), allocatable :: country_code
    integer :: start, end
    country_code = trim(fname)
    start = scan(country_code, '/', back=.false.) + 1
    end = scan(country_code, '.', back=.true.) - 1
    country_code = country_code(start:end)
  end function

  function cosine(a, b)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: cosine
    cosine = sum(a * b) / (norm2(a) * norm2(b))
  end function

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

  function cli_arguments()
    integer :: i
    character (len=32) :: arg
    character(len=32) :: cli_arguments(command_argument_count())
    do i = 1, size(cli_arguments)
      call get_command_argument(i, arg)
      cli_arguments(i) = trim(arg)
    end do
  end function


end program
