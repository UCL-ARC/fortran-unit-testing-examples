module add_matrices_test
    use cli, only : cli_args_t
    use matrix_operations, only : add_matrices
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
    public :: add_matrices_test_suite

    !> @class test_parameters_t
    !!
    !! @brief A class to store test parameters
    type, extends(input_t) :: test_parameters_t
        integer :: nx, ny
        real, dimension(:,:), allocatable :: matrix1, matrix2, expected_result
    end type test_parameters_t

    interface test_parameters_t
        module procedure test_parameters_constructor
    end interface test_parameters_t

contains

    !> A constructor for the test_parameters_t type
    pure function test_parameters_constructor(nx, ny, matrix1, matrix2, expected_result) result(test_parameters)
        !> The length of the x axis of the matrix
        integer, intent(in) :: nx
        !> The length of the y axis of the matrix
        integer, intent(in) :: ny
        !> The first matrix to be add
        real, dimension(:,:), allocatable, intent(in) :: matrix1
        !> The second matrix to be add
        real, dimension(:,:), allocatable, intent(in) :: matrix2
        !> The expected sum of the two matrices
        real, dimension(:,:), allocatable, intent(in) :: expected_result

        type(test_parameters_t) :: test_parameters
        integer :: x, y

        test_parameters%nx = nx
        test_parameters%ny = ny

        allocate(test_parameters%matrix1(ny, nx))
        allocate(test_parameters%matrix2(ny, nx))
        allocate(test_parameters%expected_result(ny, nx))
        do y = 1, ny
            do x = 1, nx
                test_parameters%matrix1(y, x) = matrix1(y, x)
                test_parameters%matrix2(y, x) = matrix2(y, x)
                test_parameters%expected_result(y, x) = expected_result(y, x)
            end do
        end do
    end function test_parameters_constructor

    !> The test suite for the matrix_operations::add_matrices subroutine tests
    function add_matrices_test_suite() result(tests)
        type(test_item_t) :: tests

        real, dimension(:,:), allocatable :: input_matrix1, input_matrix2, output_matrix
        type(example_t), dimension(3) :: test_data

        ! 3x3 Matrices tests
        allocate(input_matrix1(3,3))
        allocate(input_matrix2(3,3))
        allocate(output_matrix(3,3))

        ! Identity
        input_matrix1(:,1) = [0.0,0.0,0.0]
        input_matrix1(:,2) = [0.0,0.0,0.0]
        input_matrix1(:,3) = [0.0,0.0,0.0]

        input_matrix2(:,1) = [1.0,0.0,0.1]
        input_matrix2(:,2) = [4.0,-50.0,0.01]
        input_matrix2(:,3) = [0.1,-0.03,0.3]
        test_data(1) = example_t(test_parameters_t(3, 3, input_matrix1, input_matrix1, input_matrix1))
        test_data(2) = example_t(test_parameters_t(3, 3, input_matrix1, input_matrix2, input_matrix2))

        ! Misc matrix
        input_matrix1(:,1) = [412.0,23.0,0.0]
        input_matrix1(:,2) = [0.1231,-120.0,0.0]
        input_matrix1(:,3) = [1.0,0.0,0.1]

        input_matrix2(:,1) = [4.0,-50.0,0.01]
        input_matrix2(:,2) = [0.22,-3.0,3.0]
        input_matrix2(:,3) = [0.1,-0.03,0.3]

        output_matrix(:,1) = [416.0,-27.0,0.01]
        output_matrix(:,2) = [0.3431,-123.0,3.0]
        output_matrix(:,3) = [1.1,-0.03,0.4]
        test_data(3) = example_t(test_parameters_t(3, 3, input_matrix1, input_matrix2, output_matrix))

        deallocate(input_matrix1)
        deallocate(input_matrix2)
        deallocate(output_matrix)

        tests = describe( &
                    "matrix_operations::add_matrices", &
                    [ it( &
                        "works as expected", &
                        test_data, &
                        test_add_matrices) &
                    ])
    end function add_matrices_test_suite

    !> A unit test for the matrix_operations::add_matrices subroutine.
    function test_add_matrices(input) result(result_)
        !> An instance of the test_parameters_t containing function inputs and expected outputs.
        class(input_t), intent(in) :: input
        !> The result of the test (pass or fail) of type result_t
        type(result_t) :: result_

        real, dimension(:,:), allocatable :: actual_result

        select type (input)
        type is (test_parameters_t)
            allocate(actual_result(input%ny, input%nx))

            call add_matrices(input%matrix1, input%matrix2, actual_result)

            result_ = assert_equals(input%expected_result, actual_result)

            deallocate(actual_result)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select

    end function test_add_matrices
end module add_matrices_test
