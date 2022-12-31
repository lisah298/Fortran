module func
    
    contains
    real function  yourfunc(x)
    implicit none
    real, intent(in) :: x
 
    !exemplary function
    yourfunc = COS(x)
    end function  yourfunc


end module  func
 