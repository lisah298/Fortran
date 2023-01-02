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
    f = -24*((-2/x**(13))+(1/x**(7)))
    end function  f

    !first derivative of force
    real function  f_der(x)
    implicit none
    real, intent(in) :: x
    f_der = -(12*(4*x**6-4))/x**13-24/x**7
    end function  f_der

    !second derivative of force
    real function  f_der2(x)
    implicit none
    real, intent(in) :: x
    f_der2 =(504*x**6-624)/x**14

    end function  f_der2
end module potential