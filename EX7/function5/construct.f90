module construct
    contains
    subroutine fill(i,j,n,a_ij, b_ij)
        implicit none
        real :: pi = 3.14159265359
        integer, intent(in) :: i,j,n
        real, intent(out):: a_ij,b_ij
    
        a_ij = COS(2*pi*i*j/n)
        b_ij = SIN(2*pi*i*j/n)
      end subroutine

   
end module construct
 