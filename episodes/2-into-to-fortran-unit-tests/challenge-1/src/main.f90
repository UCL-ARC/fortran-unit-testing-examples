program main
    use matrix_operations, only : transpose_matrix, scale_matrix, add_matrices, multiply_matrices
    use cli, only : cli_args_t, parse_cli_args, deallocate_args
    implicit none

    type(cli_args_t) :: parsed_args
    integer :: stat
    real, dimension(:,:), allocatable :: result

    call parse_cli_args(parsed_args, stat)

    if (stat /= 0) then
        call deallocate_args(parsed_args)
        stop
    end if

    write (*,*) "Running ", parsed_args%requested_operation

    if (parsed_args%requested_operation == "transpose") then
        allocate(result(parsed_args%nx1, parsed_args%ny1))
        call transpose_matrix(parsed_args%matrix1, result)
    elseif (parsed_args%requested_operation == "scale") then
        allocate(result(parsed_args%nx1, parsed_args%ny1))
        call scale_matrix(parsed_args%matrix1, parsed_args%scale_factor, result)
    elseif (parsed_args%requested_operation == "add") then
        allocate(result(parsed_args%nx1, parsed_args%ny1))
        call add_matrices(parsed_args%matrix1, parsed_args%matrix2, result)
    elseif (parsed_args%requested_operation == "multiply") then
        allocate(result(parsed_args%nx1, parsed_args%ny2))
        call multiply_matrices(parsed_args%matrix1, parsed_args%matrix2, result)
    end if

    write (*,*) "Result: "
    write (*,*) "  ", result

    call deallocate_args(parsed_args)
end program main