module matrix_operations
    implicit none

    private

    public :: transpose_matrix, scale_matrix, add_matrices, multiply_matrices

contains

    subroutine transpose_matrix(matrix, transposed)
        real, intent(in) :: matrix(:,:)
        real, intent(out) :: transposed(size(matrix, 2), size(matrix, 1))
        integer :: i, j

        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                transposed(j, i) = matrix(i, j)
            end do
        end do
    end subroutine transpose_matrix

    subroutine scale_matrix(matrix, factor, scaled)
        real, intent(in) :: matrix(:,:)
        real, intent(in) :: factor
        real, intent(out) :: scaled(size(matrix, 1), size(matrix, 2))
        integer :: i, j

        do j = 1, size(matrix, 2)
            do i = 1, size(matrix, 1)
                scaled(i, j) = matrix(i, j) * factor
            end do
        end do
    end subroutine scale_matrix

    subroutine add_matrices(matrix1, matrix2, result)
        real, intent(in) :: matrix1(:,:)
        real, intent(in) :: matrix2(:,:)
        real, intent(out) :: result(size(matrix1, 1), size(matrix1, 2))
        integer :: i, j

        if (size(matrix1) /= size(matrix2)) then
            print *, "Error: Matrices must have the same dimensions for addition."
            stop
        end if

        do j = 1, size(matrix1, 2)
            do i = 1, size(matrix1, 1)
                result(i, j) = matrix1(i, j) + matrix2(i, j)
            end do
        end do
    end subroutine add_matrices

    subroutine multiply_matrices(matrix1, matrix2, result)
        real, intent(in) :: matrix1(:,:)
        real, intent(in) :: matrix2(:,:)
        real, intent(out) :: result(size(matrix1, 1), size(matrix2, 2))
        integer :: i, j, k

        if (size(matrix1, 2) /= size(matrix2, 1)) then
            print *, "Error: Number of columns in first matrix must equal number of rows in second matrix."
            stop
        end if

        do j = 1, size(matrix2, 2)
            do i = 1, size(matrix1, 1)
                result(i, j) = 0.0
                do k = 1, size(matrix1, 2)
                    result(i, j) = result(i, j) + matrix1(i, k) * matrix2(k, j)
                end do
            end do
        end do
    end subroutine multiply_matrices
end module matrix_operations