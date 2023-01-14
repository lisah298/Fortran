module potential
    implicit none
    contains

    !LJ potential
    real function  V(x)
    implicit none
    real, intent(in) :: x
    V = 4*(1/x**12-1/x**6)
    end function  V

    !force
    real function  f(x)
    implicit none
    real, intent(in) :: x
    f = -4*((-12/x**(13))-(-6/x**(7)))
    end function  f

    !first derivative of force
    real function  f_der(x)
    implicit none
    real, intent(in) :: x
    f_der = -4*((12*13)/(x**14)-((6*7)/x**8))
    end function  f_der

    !second derivative of force
    real function  f_der2(x)
    implicit none
    real, intent(in) :: x
    f_der2 =-4*((12*13*(-14))/(x**15)-((6*7*(-8))/x**9))

    end function  f_der2
end module potential