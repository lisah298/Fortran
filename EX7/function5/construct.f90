module construct
    contains
    subroutine fill(n, A, B)
        implicit none
        real :: pi = 3.14159265359
        real :: a_ij, b_ij
        integer, intent(in) :: n
        integer :: i, j
        real, dimension(n, n), intent(out) :: A, B
    
        do i = 1, n
          do j = i, n
            a_ij = SIN((2*pi*i*j)/n)
            b_ij = COS((2*pi*i*j)/n)
            A(i,j) = a_ij
            B(i,j) = b_ij
            !matrix is symmetric under swap of columns and lines
            A(j,i) = a_ij
            B(j,i) = b_ij
          end do
        end do
      end subroutine

end module construct
 