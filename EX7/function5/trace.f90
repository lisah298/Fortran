module trace
    implicit none
    contains
    real(kind=8) function  tr(M, dim)
    integer, intent(in) :: dim
    integer :: i
    real(kind=8) :: result = 0
    real(kind=8), dimension(dim, dim), intent(in) :: M
 
    do i=1, dim
        result = result + M(i,i)
    end do
    tr=result
    end function  tr

end module  trace
 