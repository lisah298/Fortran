module potential
    implicit none
    contains
    real function  LJ_potential(x)
    implicit none
    real, intent(in) :: x
 
    !exemplary function
    LJ_potential = 4*(x**(-12)-x**(-6))
    end function  LJ_potential
end module potential