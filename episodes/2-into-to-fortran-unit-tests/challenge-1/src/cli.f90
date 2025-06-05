module cli
    implicit none

    public

    type :: cli_args_t
        character(len=:), allocatable :: requested_operation
        integer :: nx1, ny1, nx2, ny2
        real, dimension(:,:), allocatable :: matrix1, matrix2
        real :: scale_factor
    end type cli_args_t

contains

    subroutine deallocate_args(parsed_args)
        type(cli_args_t), intent(inout) :: parsed_args

        if (allocated(parsed_args%requested_operation)) then
            deallocate(parsed_args%requested_operation)
        end if

        if (allocated(parsed_args%matrix1)) then
            deallocate(parsed_args%matrix1)
        end if

        if (allocated(parsed_args%matrix2)) then
            deallocate(parsed_args%matrix2)
        end if
    end subroutine deallocate_args

    subroutine parse_cli_args(parsed_args, stat)
        type(cli_args_t), intent(inout) :: parsed_args
        integer, intent(out) :: stat

        character(len=:), allocatable :: executable_string
        character(len=300) :: usage_message

        stat = 0

        ! Create usage message
        call get_string_cli_arg(0, executable_string)
        usage_message = "Usage: "//executable_string//" <options>"//NEW_LINE('a')// &
                        ""//NEW_LINE('a')// &
                        " Operations: "//NEW_LINE('a')// &
                        "   transpose <nx> <ny> <matrix>"//NEW_LINE('a')// &
                        "   scale <nx> <ny> <matrix> <factor>"//NEW_LINE('a')// &
                        "   add <nx> <ny> <matrix1> <matrix2>"//NEW_LINE('a')// &
                        "   multiply <nx2> <ny2> <matrix1> <nx2> <ny2> <matrix2>"
        deallocate(executable_string)

        call get_string_cli_arg(1, parsed_args%requested_operation)
        if (parsed_args%requested_operation /= "transpose" .and. &
                parsed_args%requested_operation /= "scale" .and. &
                parsed_args%requested_operation /= "add" .and. &
                parsed_args%requested_operation /= "multiply") then
            write (*,*) "Invalid operation ", parsed_args%requested_operation
            write (*,*) usage_message
            stat = 1
            return
        end if

        if (command_argument_count() == 4) then ! Transpose operation
            ! Verify the expected operation
            if (parsed_args%requested_operation /= "transpose") then
                write (*,*) "Incorrect number of args for ", parsed_args%requested_operation
                write (*,*) usage_message
                stat = 1
                return
            end if

            ! matrix 1
            call get_integer_cli_arg(2, parsed_args%nx1)
            call get_integer_cli_arg(3, parsed_args%ny1)
            allocate(parsed_args%matrix1(parsed_args%ny1, parsed_args%nx1))
            call get_matrix_cli_arg(4, parsed_args%matrix1)

        elseif (command_argument_count() == 5) then ! scale or add operation
            ! Verify the expected operation
            if (parsed_args%requested_operation /= "scale" .and. &
                    parsed_args%requested_operation /= "add") then
                write (*,*) "Incorrect number of args for ", parsed_args%requested_operation
                write (*,*) usage_message
                stat = 1
                return
            end if

            ! Verify the expected operation
            if (parsed_args%requested_operation == "scale") then
                ! matrix 1
                call get_integer_cli_arg(2, parsed_args%nx1)
                call get_integer_cli_arg(3, parsed_args%ny1)
                allocate(parsed_args%matrix1(parsed_args%ny1, parsed_args%nx1))
                call get_matrix_cli_arg(4, parsed_args%matrix1)
                ! scale factor
                call get_real_cli_arg(5, parsed_args%scale_factor)

            elseif (parsed_args%requested_operation == "add") then
                ! matrix 1
                call get_integer_cli_arg(2, parsed_args%nx1)
                call get_integer_cli_arg(3, parsed_args%ny1)
                allocate(parsed_args%matrix1(parsed_args%ny1, parsed_args%nx1))
                call get_matrix_cli_arg(4, parsed_args%matrix1)
                ! matrix 2
                allocate(parsed_args%matrix2(parsed_args%ny1, parsed_args%nx1))
                call get_matrix_cli_arg(5, parsed_args%matrix2)

            end if
        elseif (command_argument_count() == 7) then ! multiply operation
            ! Verify the expected operation
            if (parsed_args%requested_operation /= "multiply") then
                write (*,*) "Incorrect number of args for ", parsed_args%requested_operation
                write (*,*) usage_message
                stat = 1
                return
            end if

            ! matrix 1
            call get_integer_cli_arg(2, parsed_args%nx1)
            call get_integer_cli_arg(3, parsed_args%ny1)
            allocate(parsed_args%matrix1(parsed_args%ny1, parsed_args%nx1))
            call get_matrix_cli_arg(4, parsed_args%matrix1)
            ! matrix 2
            call get_integer_cli_arg(5, parsed_args%nx2)
            call get_integer_cli_arg(6, parsed_args%ny2)
            allocate(parsed_args%matrix2(parsed_args%ny2, parsed_args%nx2))
            call get_matrix_cli_arg(7, parsed_args%matrix2)

        else
            write (*,*) usage_message
            stat = 1
            return
        end if
    end subroutine parse_cli_args

    subroutine get_string_cli_arg(arg_index, cli_string)
        integer, intent(in) :: arg_index
        character(len=:), allocatable, intent(inout) :: cli_string

        integer :: argl

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: cli_string)
        call get_command_argument(arg_index, cli_string)
    end subroutine get_string_cli_arg

    subroutine get_matrix_cli_arg(arg_index, matrix)
        integer, intent(in) :: arg_index
        real, dimension(:,:), allocatable, intent(inout) :: matrix

        integer :: argl, required_matrix_size, actual_matrix_size, stat
        character(len=:), allocatable :: a
        character(len=100) :: error_message

        stat = 0

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: a)
        call get_command_argument(arg_index, a)

        required_matrix_size = size(matrix, 1)*size(matrix, 2)
        actual_matrix_size = 1 + (argl / 2)
        if (required_matrix_size /= actual_matrix_size) then
            write (error_message,*) "Matrix of incorrect size provided. Required ", &
                                    required_matrix_size, " but got ", actual_matrix_size
            stat = 1
        end if

        if (stat == 0) then
            read(a,*) matrix
        end if

        deallocate(a)

        if (stat /= 0) then
            write (*,*) error_message
            stop
        end if
    end subroutine get_matrix_cli_arg

    subroutine get_integer_cli_arg(arg_index, integer_value)
        integer, intent(in) :: arg_index
        integer, intent(out) :: integer_value

        integer :: argl
        character(len=:), allocatable :: a

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: a)
        call get_command_argument(arg_index, a)
        read(a,*) integer_value
        deallocate(a)
    end subroutine get_integer_cli_arg

    subroutine get_real_cli_arg(arg_index, real_value)
        integer, intent(in) :: arg_index
        real, intent(out) :: real_value

        integer :: argl
        character(len=:), allocatable :: a

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: a)
        call get_command_argument(arg_index, a)
        read(a,*) real_value
        deallocate(a)
    end subroutine get_real_cli_arg
end module cli