module trace
    implicit none
    contains
    real function  tr(M, dim)
    integer, intent(in) :: dim
    integer :: i
    real :: result = 0
    real, dimension(dim, dim), intent(in) :: M
 
    do i=1, dim
        result = result + M(i,i)
    end do
    tr=result
    end function  tr

end module  trace
 