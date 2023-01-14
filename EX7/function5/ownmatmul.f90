module ownMatMul
    contains
    subroutine multiply(n, A, B, C)
        implicit none
        real(kind=8) :: c_ij 
        integer, intent(in) :: n
        integer :: i, j, res
        real(kind=8), dimension(n, n), intent(in) :: A, B
        real(kind=8), dimension(n, n), intent(out) :: C

       !compute matrix element c_ij
        do i = 1, n
          do j = 1, n
            c_ij = 0
            do res = 1, n
              c_ij = c_ij + A(i,res) * B(res,j)
            end do
            C(i,j) = c_ij
          end do
        end do
      end subroutine

end module ownMatMul
 