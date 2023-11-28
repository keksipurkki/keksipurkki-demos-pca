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
      integer(int32) :: offset

      cols = size(self%images)

      do i = 1, size(self%images)
        offset = transfer(self%images(i)%bytes(11:14), 1_int32) + 1
        rows = max(rows, size(self%images(i)%bytes(offset:)))
      enddo

      allocate(matrix(rows, cols), source=0.0d0)

      do i = 1, size(self%images)
        offset = transfer(self%images(i)%bytes(11:14), 1_int32) + 1
        matrix(:,i) = 1.0d0 * self%images(i)%bytes(offset:)
      enddo

    end function

    function input(arguments) result(x)
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
  use image_class
  use dispmodule
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: x(:,:), t(:,:)
  real(real64), allocatable :: s(:), u(:,:), v(:,:)
  character(len=32), allocatable :: args(:)
  integer :: i, j, axis, rank
  integer, allocatable, target :: result(:,:)
  integer, pointer :: r(:)

  open(output_unit, encoding='utf-8')

  rank = 30
  args = cli_arguments()

  if (size(args) == 0) then
    call disp("Expected at least one argument")
    stop 0
  endif

  call disp('Args: ', size(args))
  call disp('Rank: ', rank)

  allocate(result(rank, size(args)), source=0)

  x = input(args)
  call dsvd(x, s, u, v)
  call disp('Left singular vectors: ', shape(u))

  t = score(s, u, rank)

  do i = 1, size(args)
    axis = maxloc([( cosine(x(:,i), t(:,j)), j = 1, rank )], 1)
    r => result(axis,:)
    j = findloc(r, 0, 1)
    r(j) = i
  enddo

  call pca(args, result)

contains

  function score(sigma, u, rank) result(t)
    integer, intent(in) :: rank
    real(real64), intent(in) :: u(:,:), sigma(:)
    real(real64), allocatable :: t(:,:)
    integer :: i

    call assert(rank <= size(sigma), 'Array shape mismatch')

    allocate(t(size(u,1), rank), source=0.0d0)

    do i = 1, rank
      t(:,i) = sigma(i) * u(:,i)
    enddo

  end function

  subroutine pca(args, result)
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
        write (*, '(AA)', advance='no') country_emoji(country_code(args(cluster(j)))), " "
      enddo

      print *, ''
    enddo

    call disp('-------------------------------------------------------------------')
  end subroutine

  subroutine dsvd(x, s, u, v)
    use svd, only: dsvdc
    real(real64), intent(in) :: x(:,:)
    real(real64), intent(out), allocatable :: s(:), u(:,:), v(:,:)
    real(real64), allocatable :: e(:)
    real(real64), allocatable :: xx(:,:)
    integer :: n, p, job, info

    ! automatic allocation
    xx = x

    n = size(xx, 1)
    p = size(xx, 2)

    call assert(n >= p, 'Expected a matrix with shape n >= p')

    job = 20
    allocate(s(min(n + 1, p)), source=0.0d0)
    allocate(e(p), source=0.d0)
    allocate(u(n, p), source=0.0d0)

    call dsvdc(xx, n, p, s, e, u, v, job, info)
    call assert(info == 0, 'Singular value decomposition failed')
    call assert(all(e < epsilon(1.0_real64)), 'Singular value decomposition failed')

  end subroutine

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
    codepoints = [( ichar(code(i:i)) + offset, i = 1, len(code) )]
    country_emoji = transfer(codepoints, country_emoji)
  end function

  function cosine(a, b)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: cosine
    call assert(size(a) == size(b), 'Input rank mismatch')
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
