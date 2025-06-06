module matrix_operations
    implicit none

    private

    public :: transpose_matrix, scale_matrix, add_matrices, multiply_matrices

contains

    !> Transpose a given matrix
    subroutine transpose_matrix(matrix, transposed)
        !> The matrix to be transposed
        real, intent(in) :: matrix(:,:)
        !> The resultant transposed matrix
        real, intent(out) :: transposed(size(matrix, 2), size(matrix, 1))
        integer :: x, y

        do y = 1, size(matrix, 2)
            do x = 1, size(matrix, 1)
                transposed(y, x) = matrix(x, y)
            end do
        end do
    end subroutine transpose_matrix

    !> Scale a matrix by a given factor
    subroutine scale_matrix(matrix, factor, scaled)
        !> The matrix to be scaled
        real, intent(in) :: matrix(:,:)
        !> The factor by which to scale the matrix
        real, intent(in) :: factor
        !> The resultant scaled matrix
        real, intent(out) :: scaled(size(matrix, 1), size(matrix, 2))
        integer :: x, y

        do y = 1, size(matrix, 2)
            do x = 1, size(matrix, 1)
                scaled(x, y) = matrix(x, y) * factor
            end do
        end do
    end subroutine scale_matrix

    !> Add two matrices together
    subroutine add_matrices(matrix1, matrix2, result)
        !> The first matrix to be summed
        real, intent(in) :: matrix1(:,:)
        !> The second matrix to be summed
        real, intent(in) :: matrix2(:,:)
        !> The resultant summed matrix
        real, intent(out) :: result(size(matrix1, 1), size(matrix1, 2))
        integer :: x, y

        if (size(matrix1) /= size(matrix2)) then
            print *, "Error: Matrices must have the same dimensions for addition."
            stop
        end if

        do y = 1, size(matrix1, 2)
            do x = 1, size(matrix1, 1)
                result(x, y) = matrix1(x, y) + matrix2(x, y)
            end do
        end do
    end subroutine add_matrices

    !> Multiply two matrices together
    subroutine multiply_matrices(matrix1, matrix2, result)
        !> The first matrix to be multiplied
        real, intent(in) :: matrix1(:,:)
        !> The second matrix to be multiplied
        real, intent(in) :: matrix2(:,:)
        !> The resultant multiplied matrix
        real, intent(out) :: result(size(matrix1, 1), size(matrix2, 2))
        integer :: x1, y2, k

        if (size(matrix1, 2) /= size(matrix2, 1)) then
            print *, "Error: Number of columns in first matrix must equal number of rows in second matrix."
            stop
        end if

        do y2 = 1, size(matrix2, 2)
            do x1 = 1, size(matrix1, 1)
                result(x1, y2) = 0.0
                do k = 1, size(matrix1, 2)
                    result(x1, y2) = result(x1, y2) + matrix1(x1, k) * matrix2(k, y2)
                end do
            end do
        end do
    end subroutine multiply_matrices
end module matrix_operations
