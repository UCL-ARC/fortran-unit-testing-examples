module scale_matrix_test
    use cli, only : cli_args_t
    use matrix_operations, only : scale_matrix
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
    public :: scale_matrix_test_suite

    !> @class test_parameters_t
    !!
    !! @brief A class to store test parameters
    type, extends(input_t) :: test_parameters_t
        integer :: nx, ny
        real, dimension(:,:), allocatable :: matrix, expected_result
        real :: scale_factor
    end type test_parameters_t

    interface test_parameters_t
        module procedure test_parameters_constructor
    end interface test_parameters_t

contains

    !> A constructor for the test_parameters_t type
    pure function test_parameters_constructor(nx, ny, matrix, scale_factor, expected_result) result(test_parameters)
        !> The length of the x axis of the matrix
        integer, intent(in) :: nx
        !> The length of the y axis of the matrix
        integer, intent(in) :: ny
        !> The matrix to be scaled
        real, dimension(:,:), allocatable, intent(in) :: matrix
        !> The factor by which to scale the matrix
        real, intent(in) :: scale_factor
        !> The expected scaled form on the matrix
        real, dimension(:,:), allocatable, intent(in) :: expected_result

        type(test_parameters_t) :: test_parameters
        integer :: x, y

        test_parameters%nx = nx
        test_parameters%ny = ny
        test_parameters%scale_factor = scale_factor

        allocate(test_parameters%matrix(nx, ny))
        allocate(test_parameters%expected_result(nx, ny))
        do y = 1, ny
            do x = 1, nx
                test_parameters%matrix(x, y) = matrix(x, y)
                test_parameters%expected_result(x, y) = expected_result(x, y)
            end do
        end do
    end function test_parameters_constructor

    !> The test suite for the matrix_operations::scale_matrix subroutine tests
    function scale_matrix_test_suite() result(tests)
        type(test_item_t) :: tests

        real, dimension(:,:), allocatable :: input_matrix, output_matrix
        type(example_t), dimension(7) :: test_data

        ! 3x3 Matrices tests
        allocate(input_matrix(3,3))
        allocate(output_matrix(3,3))

        ! Identity
        input_matrix(1,:) = [1.0,0.0,0.0]
        input_matrix(2,:) = [0.0,1.0,0.0]
        input_matrix(3,:) = [0.0,0.0,1.0]

        output_matrix(1,:) = [2.0,0.0,0.0]
        output_matrix(2,:) = [0.0,2.0,0.0]
        output_matrix(3,:) = [0.0,0.0,2.0]
        test_data(1) = example_t(test_parameters_t(3, 3, input_matrix, 2.0, output_matrix))

        ! Zero Matrix
        input_matrix(1,:) = [0.0,0.0,0.0]
        input_matrix(2,:) = [0.0,0.0,0.0]
        input_matrix(3,:) = [0.0,0.0,0.0]
        test_data(2) = example_t(test_parameters_t(3, 3, input_matrix, 5.0, input_matrix))
        test_data(3) = example_t(test_parameters_t(3, 3, input_matrix, -23.0, input_matrix))

        ! Negative matrix elements
        input_matrix(1,:) = [-1.0,2.0,3.0]
        input_matrix(2,:) = [-4.0,5.0,6.0]
        input_matrix(3,:) = [7.0,-8.0,9.0]

        output_matrix(1,:) = [2.0,-4.0,-6.0]
        output_matrix(2,:) = [8.0,-10.0,-12.0]
        output_matrix(3,:) = [-14.0,16.0,-18.0]
        test_data(4) = example_t(test_parameters_t(3, 3, input_matrix, -2.0, output_matrix))

        deallocate(input_matrix)
        deallocate(output_matrix)

        ! 2x3 Matrices tests
        allocate(input_matrix(2,3))
        allocate(output_matrix(2,3))

        ! Zero Matrix
        input_matrix(1,:) = [0.0,0.0,0.0]
        input_matrix(2,:) = [0.0,0.0,0.0]
        test_data(5) = example_t(test_parameters_t(2, 3, input_matrix, 233.0, input_matrix))
        test_data(6) = example_t(test_parameters_t(2, 3, input_matrix, -3412.0, input_matrix))

        ! Negative matrix elements
        input_matrix(1,:) = [1.0,-2.0,3.0]
        input_matrix(2,:) = [-4.0,5.0,6.0]

        output_matrix(1,:) = [-0.5,1.0,-1.5]
        output_matrix(2,:) = [2.0,-2.5,-3.0]
        test_data(7) = example_t(test_parameters_t(2, 3, input_matrix, -0.5, output_matrix))

        deallocate(input_matrix)
        deallocate(output_matrix)

        tests = describe( &
                    "matrix_operations::scale_matrix", &
                    [ it( &
                        "works as expected", &
                        test_data, &
                        test_scale_matrix) &
                    ])
    end function scale_matrix_test_suite

    !> A unit test for the matrix_operations::scale_matrix subroutine.
    function test_scale_matrix(input) result(result_)
        !> An instance of the test_parameters_t containing function inputs and expected outputs.
        class(input_t), intent(in) :: input
        !> The result of the test (pass or fail) of type result_t
        type(result_t) :: result_

        real, dimension(:,:), allocatable :: actual_result

        select type (input)
        type is (test_parameters_t)
            allocate(actual_result(input%nx, input%ny))

            call scale_matrix(input%matrix, input%scale_factor, actual_result)

            result_ = assert_equals(input%expected_result, actual_result)

            deallocate(actual_result)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select

    end function test_scale_matrix
end module scale_matrix_test
