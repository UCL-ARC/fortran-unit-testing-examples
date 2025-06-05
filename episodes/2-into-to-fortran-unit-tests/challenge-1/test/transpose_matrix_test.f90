module transpose_matrix_test
    use cli, only : cli_args_t
    use matrix_operations, only : transpose_matrix
    use veggies, only: &
            assert_equals, &
            describe, &
            example_t, &
            fail, &
            input_t, &
            it, &
            result_t, &
            test_item_t

    implicit none

    private
    public :: transpose_matrix_test_suite

    !> @class test_parameters_t
    !!
    !! @brief A class to store test parameters
    type, extends(input_t) :: test_parameters_t
        integer :: nx, ny
        real, dimension(:,:), allocatable :: matrix, expected_result
    end type test_parameters_t

    interface test_parameters_t
        module procedure test_parameters_constructor
    end interface test_parameters_t

contains

    !> A constructor for the test_parameters_t type
    pure function test_parameters_constructor(nx, ny, matrix, expected_result) result(test_parameters)
        !> The length of the x axis of the matrix
        integer, intent(in) :: nx
        !> The length of the y axis of the matrix
        integer, intent(in) :: ny
        !> The matrix to be transposed
        real, dimension(:,:), allocatable, intent(in) :: matrix
        !> The expected transposed form on the matrix
        real, dimension(:,:), allocatable, intent(in) :: expected_result

        type(test_parameters_t) :: test_parameters
        integer :: x, y

        test_parameters%nx = nx
        test_parameters%ny = ny

        allocate(test_parameters%matrix(ny, nx))
        allocate(test_parameters%expected_result(nx, ny))
        do y = 1, ny
            do x = 1, nx
                test_parameters%matrix(y, x) = matrix(y, x)
                test_parameters%expected_result(x, y) = expected_result(x, y)
            end do
        end do
    end function test_parameters_constructor

    !> The test suite for the matrix_operations module tests
    function transpose_matrix_test_suite() result(tests)
        type(test_item_t) :: tests

        real, dimension(:,:), allocatable :: input_matrix, output_matrix
        type(example_t), dimension(5) :: transpose_matrix_test_data

        ! 3x3 Matrices tests
        allocate(input_matrix(3,3))
        allocate(output_matrix(3,3))

        ! Identity
        input_matrix(:,1) = [1.0,0.0,0.0]
        input_matrix(:,2) = [0.0,1.0,0.0]
        input_matrix(:,3) = [0.0,0.0,1.0]
        transpose_matrix_test_data(1) = example_t(test_parameters_t(3, 3, input_matrix, input_matrix))

        ! Zero Matrix
        input_matrix(:,1) = [0.0,0.0,0.0]
        input_matrix(:,2) = [0.0,0.0,0.0]
        input_matrix(:,3) = [0.0,0.0,0.0]
        transpose_matrix_test_data(2) = example_t(test_parameters_t(3, 3, input_matrix, input_matrix))

        ! Unsymetric matrix
        input_matrix(:,1) = [1.0,2.0,3.0]
        input_matrix(:,2) = [4.0,5.0,6.0]
        input_matrix(:,3) = [7.0,8.0,9.0]

        output_matrix(1,:) = [1.0,2.0,3.0]
        output_matrix(2,:) = [4.0,5.0,6.0]
        output_matrix(3,:) = [7.0,8.0,9.0]
        transpose_matrix_test_data(3) = example_t(test_parameters_t(3, 3, input_matrix, output_matrix))

        deallocate(input_matrix)
        deallocate(output_matrix)

        ! 2x3 Matrices tests
        allocate(input_matrix(3,2))
        allocate(output_matrix(2,3))

        ! Zero Matrix
        input_matrix(:,1) = [0.0,0.0,0.0]
        input_matrix(:,2) = [0.0,0.0,0.0]

        output_matrix(1,:) = [0.0,0.0,0.0]
        output_matrix(2,:) = [0.0,0.0,0.0]
        transpose_matrix_test_data(4) = example_t(test_parameters_t(2, 3, input_matrix, output_matrix))

        ! Unsymetric matrix
        input_matrix(:,1) = [1.0,2.0,3.0]
        input_matrix(:,2) = [4.0,5.0,6.0]

        output_matrix(1,:) = [1.0,2.0,3.0]
        output_matrix(2,:) = [4.0,5.0,6.0]
        transpose_matrix_test_data(5) = example_t(test_parameters_t(2, 3, input_matrix, output_matrix))

        deallocate(input_matrix)
        deallocate(output_matrix)

        tests = describe( &
                    "matrix_operations::transpose_matrix", &
                    [ it( &
                        "works as expected", &
                        transpose_matrix_test_data, &
                        test_transpose_matrix) &
                    ])
    end function transpose_matrix_test_suite

    !> A unit test for the matrix_operations::transpose_matrix subroutine.
    function test_transpose_matrix(input) result(result_)
        !> An instance of the test_parameters_t containing function inputs and expected outputs.
        class(input_t), intent(in) :: input
        !> The result of the test (pass or fail) of type result_t
        type(result_t) :: result_

        real, dimension(:,:), allocatable :: actual_result

        select type (input)
        type is (test_parameters_t)
            allocate(actual_result(input%nx, input%ny))

            call transpose_matrix(input%matrix, actual_result)

            result_ = assert_equals(input%expected_result, actual_result)

            deallocate(actual_result)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select

    end function test_transpose_matrix
end module transpose_matrix_test
