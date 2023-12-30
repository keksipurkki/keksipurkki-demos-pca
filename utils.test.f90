include 'assertions.f90'
include 'utils.f90'

program main_test
  use utils
  use assertions
  use dispmodule
  implicit none

  call disp('-------------------------------------------------------------------')
  call assert_normalization_works();
  call disp('-------------------------------------------------------------------')

  call disp('-------------------------------------------------------------------')
  call assert_reshape_works();
  call disp('-------------------------------------------------------------------')

  call disp('-------------------------------------------------------------------')
  call assert_grayscale_works();
  call disp('-------------------------------------------------------------------')

  contains

    subroutine assert_normalization_works()

      integer, parameter :: n = 5, eps = epsilon(1.0_real64)
      integer :: j
      real(real64) :: input(n, 10), output(n, 10)
      real(real64), allocatable :: variance(:), mean(:)

      do j = 1, n
        input(j,:) = 1.0d0 * j
      enddo

      output = normalize(input)

      call disp('output = ', output)

      mean = sum(output, 1) / n
      variance = sum((output - spread(mean, 1, n))**2, 1) / n

      call disp('mean = ', mean, advance = 'no')
      call disp('variance = ', variance)

      call assert(all(mean == 0.0d0), 'Expected zero mean')
      call assert(all(variance - 1.0d0 < eps), 'Expected unit variance')

    end subroutine

    subroutine assert_reshape_works()
      integer :: input(3,3), actual(3*3), expected(3*3)
      input(1,:) = [1, 2, 3]
      input(2,:) = [5, 5, 6]
      input(3,:) = [0, 0, 0]
      expected = [1,5,0,2,5,0,3,6,0]
      actual = reshape(input, [9])
      call disp('actual = ', actual, advance = 'no')
      call disp('expected = ', expected)
      call assert(all(actual == expected), 'Expected a different order')
    end subroutine

    subroutine assert_grayscale_works()
      real(real64) :: input(1,6), actual(1, 6/3), expected(1, 6/3)
      input(1,:) = [1, 255, 54, 255, 128, 30]
      expected(1,:) = [103, 138]
      actual = nint(grayscale(input))
      call disp('actual = ', actual, advance = 'no')
      call disp('expected = ', expected)
      call assert(all(actual == expected), 'Grayscaling does not work')
    end subroutine

end program
