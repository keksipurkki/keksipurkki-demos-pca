include 'assertions.f90'
include 'utils.f90'

module image_class
  use iso_fortran_env
  implicit none

  type :: image
    character(:), allocatable :: file_name
    integer(int32), allocatable :: bitmap(:,:)
  end type

  type :: image_matrix
    type(image), allocatable :: images(:)
  end type

  contains

    !!
    !! Output shape: x(samples, image_bytes)
    !! Columns : rgb_1,rgb_2,rgb_3...
    !!
    function matrix(self)
      class(image_matrix), intent(in) :: self
      real(real64), allocatable :: matrix(:,:)
      integer :: i, rows, cols
      integer(int32), allocatable :: bytes(:)

      rows = size(self%images)

      do i = 1, rows
        cols = max(cols, size(self%images(i)%bitmap))
      enddo

      allocate(matrix(rows, cols), source=0.0d0)

      do i = 1, rows
        bytes = reshape(self%images(i)%bitmap, [size(self%images(i)%bitmap)])
        matrix(i,:) = 1.0d0 * bytes
      enddo

    end function

end module

program main
  use image_class
  use dispmodule
  use iso_fortran_env
  use assertions
  use utils
  implicit none

  character(len=32), allocatable :: args(:)
  integer :: n, rank
  logical :: normalization = .true.
  real(real64), allocatable :: x(:,:)

  call disp_set(sep = ', ')
  open(output_unit, encoding='utf-8')

  args = cli_arguments()
  n = size(args)
  rank = 10

  if (n == 0) then
    call disp("Expected at least one argument")
    stop 0
  endif

  call disp('Args: ', n)
  call disp('Rank: ', rank)

  call disp('-------------------------------------------------------------------')

  call disp('RGB')
  x = normalize(input(args))
  call output('rgb.txt', args, pca(x, rank))

  call disp('-------------------------------------------------------------------')

  call disp('Grayscale')
  x = normalize(grayscale(input(args)))
  call output('grayscale.txt', args, pca(x, rank))

contains

  function input(arguments) result(x)
    use, intrinsic :: iso_c_binding
    real(real64), allocatable :: x(:,:)
    character(len=*), intent(in) :: arguments(:)
    integer(int32), allocatable :: bytes(:)
    integer(int32) :: offset, i, file_size
    type(image) :: images(size(arguments))
    type(image_matrix) :: m
    character(c_char), allocatable :: buffer(:)

    integer, allocatable, target :: rgba(:,:)
    integer, pointer :: alpha(:), rgb(:,:)

    do i = 1, size(arguments)
      inquire(file=arguments(i), size=file_size)
      allocate(buffer(file_size))

      open(10, file = arguments(i), access = 'stream', form = 'unformatted')
      read(10) buffer
      close(10)

      ! BMP image spec. Start of the bitmap array
      offset = transfer(buffer(11:14), 1_int32) + 1
      buffer = buffer(offset:)

      ! Sanity check
      call assert(mod(size(buffer), 4) == 0, 'Array size mismatch')

      rgba = reshape(ichar(buffer), [4, size(buffer)/4])
      rgb => rgba(1:3,:)
      alpha => rgba(4,:)

      ! Sanity check
      call assert(all(alpha == 255), 'Expected full opacity')

      images(i) = image(file_name=arguments(i), bitmap=rgb)
      deallocate(buffer)
    enddo

    m = image_matrix(images)
    x = matrix(m)

  end function

  subroutine output(fname, args, result)
    character(len=*), intent(in) :: fname
    character(len=32), intent(in) :: args(:)
    real(real64), intent(in), target :: result(:,:)
    integer :: i

    integer :: io

    open(newunit=io, file=fname, status="replace", action="write", encoding='utf-8')

    do i = 1, size(args)
      write (io, '(AA)', advance='no') country_code(args(i)), ";"
      write (io, '(AA)', advance='no') country_emoji(country_code(args(i))), " ;"
      write (io, '(*(G0.5,:";"))', advance='no') result(i, :)
      write (io, '(A)', advance='yes') ''
    enddo

    close(io)
  end subroutine

  function pca(x, rank) result(t)

    real(real64), allocatable, intent(in) :: x(:,:)
    integer, intent(in) :: rank

    real(real64), allocatable :: t(:,:)
    real(real64), allocatable :: s(:), u(:,:), v(:,:)
    integer :: i

    call dsvd(x, s, u, v)
    call disp('Left singular vectors = ', shape(u), orient = 'row')
    call disp('Sigma = ', reshape(s, [size(s)/10, 10], order=[2,1]))

    allocate(t(size(u,1), size(u, 2)), source=0.0d0)

    do i = 1, size(t, 2)
      t(:,i) = u(:,i) * s(i)
    enddo

    call disp('Result shape = ', shape(t), orient='row')

    t = t(:, 1:rank) ! Rank reduction

    call disp('Truncated result shape = ', shape(t), orient='row')
  end function

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

    call disp('SVD shape = ', shape(xx), orient='row')
    call assert(n < p, 'Expected a matrix with shape n < p')

    job = 10
    allocate(s(min(n + 1, p)), source=0.0d0)
    allocate(e(p), source=0.d0)
    allocate(u(n, n), source=0.0d0)

    call dsvdc(xx, n, p, s, e, u, v, job, info)
    call assert(info == 0, 'Singular value decomposition failed.')
    call assert(all(e(1:n) < epsilon(1.0_real64)), 'Singular value decomposition failed')

  end subroutine

end program
