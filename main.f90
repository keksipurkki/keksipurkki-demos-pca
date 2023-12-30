module image_class
  use iso_fortran_env
  use dispmodule
  implicit none

  type :: Image
    character(:), allocatable :: file_name
    integer(int32), allocatable :: bytes(:)
  end type

  type :: ImageMatrix
    type(Image), allocatable :: images(:)
  end type

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

    !!
    !! Output shape: x(samples, image_bytes)
    !!
    function matrix(self)
      class(ImageMatrix), intent(in) :: self
      real(real64), allocatable :: matrix(:,:)
      integer :: i, rows, cols

      rows = size(self%images)

      do i = 1, rows
        cols = max(cols, size(self%images(i)%bytes))
      enddo

      allocate(matrix(rows, cols), source=0.0d0)

      do i = 1, rows
        matrix(i,:) = 1.0d0 * self%images(i)%bytes
      enddo

    end function

    function input(arguments, grayscale) result(x)
      use, intrinsic :: iso_c_binding
      real(real64), allocatable :: x(:,:)
      character(len=*), intent(in) :: arguments(:)
      logical, intent(in) :: grayscale
      integer(int32), allocatable :: bytes(:)
      integer(int32) :: offset, i, j, file_size
      type(Image) :: images(size(arguments))
      type(ImageMatrix) :: m
      character(c_char), allocatable :: buffer(:)

      integer, allocatable, target :: rgba(:,:)
      integer, pointer :: alpha(:), rgb(:,:)

      logical :: normalization = .true.
      real(real64), allocatable :: mean(:), variance(:)

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

        if (grayscale) then
          images(i) = image(file_name=arguments(i), bytes=sum(rgb, 1)/3)
        else
          images(i) = image(file_name=arguments(i), bytes=reshape(rgb, [size(rgb)]))
        end if

        deallocate(buffer)
      enddo

      m = ImageMatrix(images)
      x = matrix(m)

      ! Column-wise zero mean and unit variance
      if (normalization) then
        mean = sum(x, 1) / size(x, 1)
        variance = sum((x - spread(mean, 1, size(x, 1)))**2, 1) / size(x, 1)

        call disp('Mean vector: ', shape(mean))
        call disp('Variance vector: ', shape(variance))

        do i = 1, size(arguments)
          x(i,:) = (x(i,:) - mean) / variance
        enddo
      endif

    end function

end module

program main
  use image_class
  use dispmodule
  use iso_fortran_env
  implicit none

  character(len=32), allocatable :: args(:)
  integer :: n, rank

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
  call output('rgb.txt', args, pca(input(args, grayscale=.false.), rank))

  call disp('Grayscale')
  call output('grayscale.txt', args, pca(input(args, grayscale=.true.), rank))

contains

  function pca(x, rank) result(t)

    real(real64), allocatable, intent(in) :: x(:,:)
    integer, intent(in) :: rank

    real(real64), allocatable :: t(:,:)
    real(real64), allocatable :: s(:), u(:,:), v(:,:)
    integer :: i

    call dsvd(x, s, u, v)
    call disp('Left singular vectors: ', shape(u))
    call disp('Singular values: ')
    call disp(s)

    allocate(t(size(u,1), size(u, 2)), source=0.0d0)

    do i = 1, size(t, 2)
      t(:,i) = u(:,i) * s(i)
    enddo

    call disp('Result shape: ', shape(t))

    t = t(:, 1:rank)

    call disp('Truncated result shape: ', shape(t))
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

    call disp('SVD shape')
    call disp('n: ', n)
    call disp('p: ', p)
    call assert(n < p, 'Expected a matrix with shape n < p')

    job = 10
    allocate(s(min(n + 1, p)), source=0.0d0)
    allocate(e(p), source=0.d0)
    allocate(u(n, n), source=0.0d0)

    call dsvdc(xx, n, p, s, e, u, v, job, info)
    call assert(info == 0, 'Singular value decomposition failed.')
    call assert(all(e(1:n) < epsilon(1.0_real64)), 'Singular value decomposition failed')

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

end program
