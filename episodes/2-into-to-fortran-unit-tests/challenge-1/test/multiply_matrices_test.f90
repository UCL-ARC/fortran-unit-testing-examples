module multiply_matrices_test
    use cli, only : cli_args_t
    use matrix_operations, only : multiply_matrices
    use veggies, only: &
            assert_equals_within_absolute, &
            describe, &
            example_t, &
            fail, &
            input_t, &
            it, &
            result_t, &
            test_item_t

    implicit none

    private
    public :: multiply_matrices_test_suite

    !> @class test_parameters_t
    !!
    !! @brief A class to store test parameters
    type, extends(input_t) :: test_parameters_t
        integer :: nx1, ny1, nx2, ny2
        real, dimension(:,:), allocatable :: matrix1, matrix2, expected_result
    end type test_parameters_t

    interface test_parameters_t
        module procedure test_parameters_constructor
    end interface test_parameters_t

contains

    !> A constructor for the test_parameters_t type
    pure function test_parameters_constructor(nx1, ny1, matrix1, nx2, ny2, matrix2, expected_result) result(test_parameters)
        !> The length of the x axis of the first matrix
        integer, intent(in) :: nx1
        !> The length of the y axis of the first matrix
        integer, intent(in) :: ny1
        !> The first matrix to be multiply
        real, dimension(:,:), allocatable, intent(in) :: matrix1
        !> The length of the x axis of the second matrix
        integer, intent(in) :: nx2
        !> The length of the y axis of the second matrix
        integer, intent(in) :: ny2
        !> The second matrix to be multiply
        real, dimension(:,:), allocatable, intent(in) :: matrix2
        !> The expected sum of the two matrices
        real, dimension(:,:), allocatable, intent(in) :: expected_result

        type(test_parameters_t) :: test_parameters
        integer :: x1, y1, x2, y2

        test_parameters%nx1 = nx1
        test_parameters%ny1 = ny1
        test_parameters%nx2 = nx2
        test_parameters%ny2 = ny2

        allocate(test_parameters%matrix1(nx1, ny1))
        allocate(test_parameters%matrix2(nx2, ny2))
        allocate(test_parameters%expected_result(nx1, ny2))
        do y1 = 1, ny1
            do x1 = 1, nx1
                test_parameters%matrix1(x1, y1) = matrix1(x1, y1)
            end do
        end do
        do y2 = 1, ny2
            do x2 = 1, nx2
                test_parameters%matrix2(x2, y2) = matrix2(x2, y2)
            end do
            do x1 = 1, nx1
                test_parameters%expected_result(x1, y2) = expected_result(x1, y2)
            end do
        end do
    end function test_parameters_constructor

    !> The test suite for the matrix_operations::multiply_matrices subroutine tests
    function multiply_matrices_test_suite() result(tests)
        type(test_item_t) :: tests

        real, dimension(:,:), allocatable :: input_matrix1, input_matrix2, output_matrix
        type(example_t), dimension(4) :: test_data
        integer :: x2

        ! 3x3 Matrices tests
        allocate(input_matrix1(3,3))
        allocate(input_matrix2(3,3))
        allocate(output_matrix(3,3))

        ! Identity
        input_matrix1(1,:) = [1.0,0.0,0.0]
        input_matrix1(2,:) = [0.0,1.0,0.0]
        input_matrix1(3,:) = [0.0,0.0,1.0]

        input_matrix2(1,:) = [1.0,0.0,0.1]
        input_matrix2(2,:) = [4.0,-50.0,0.01]
        input_matrix2(3,:) = [0.1,-0.03,0.3]
        test_data(1) = example_t(test_parameters_t(3, 3, input_matrix1, 3, 3, input_matrix1, input_matrix1))
        test_data(2) = example_t(test_parameters_t(3, 3, input_matrix1, 3, 3, input_matrix2, input_matrix2))

        ! Misc matrix
        input_matrix1(1,:) = [1.0,2.0,3.0]
        input_matrix1(2,:) = [4.0,5.0,6.0]
        input_matrix1(3,:) = [7.0,8.0,9.0]

        input_matrix2(1,:) = [10.0,11.0,12.0]
        input_matrix2(2,:) = [13.0,14.0,15.0]
        input_matrix2(3,:) = [16.0,17.0,18.0]

        output_matrix(1,:) = [84.0,90.0,96.0]
        output_matrix(2,:) = [201.0,216.0,231.0]
        output_matrix(3,:) = [318.0,342.0,366.0]
        test_data(3) = example_t(test_parameters_t(3, 3, input_matrix1, 3, 3, input_matrix2, output_matrix))

        deallocate(input_matrix1)
        deallocate(input_matrix2)
        deallocate(output_matrix)

        ! Mismatched matrix sizes
        allocate(input_matrix1(3,2))
        allocate(input_matrix2(2,3))
        allocate(output_matrix(3,3))

        ! Non-square matrix
        input_matrix1(1,:) = [1.0,2.0]
        input_matrix1(2,:) = [4.0,5.0]
        input_matrix1(3,:) = [7.0,8.0]

        input_matrix2(1,:) = [10.0,11.0,12.0]
        input_matrix2(2,:) = [13.0,14.0,15.0]

        output_matrix(1,:) = [36.0,39.0,42.0]
        output_matrix(2,:) = [105.0,114.0,123.0]
        output_matrix(3,:) = [174.0,189.0,204.0]
        test_data(4) = example_t(test_parameters_t(3, 2, input_matrix1, 2, 3, input_matrix2, output_matrix))

        deallocate(input_matrix1)
        deallocate(input_matrix2)
        deallocate(output_matrix)

        tests = describe( &
                    "matrix_operations::multiply_matrices", &
                    [ it( &
                        "works as expected", &
                        test_data, &
                        test_multiply_matrices) &
                    ])
    end function multiply_matrices_test_suite

    !> A unit test for the matrix_operations::multiply_matrices subroutine.
    function test_multiply_matrices(input) result(result_)
        !> An instance of the test_parameters_t containing function inputs and expected outputs.
        class(input_t), intent(in) :: input
        !> The result of the test (pass or fail) of type result_t
        type(result_t) :: result_

        real, dimension(:,:), allocatable :: actual_result

        select type (input)
        type is (test_parameters_t)
            allocate(actual_result(input%nx1, input%ny2))

            call multiply_matrices(input%matrix1, input%matrix2, actual_result)

            result_ = assert_equals_within_absolute(input%expected_result, actual_result, 1e-3)

            deallocate(actual_result)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select

    end function test_multiply_matrices
end module multiply_matrices_test
