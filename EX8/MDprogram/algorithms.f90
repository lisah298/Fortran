module algorithms
    use potential
    implicit none
    contains
    subroutine euler1(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a
        x_next = x0 + v0 * dt
        a = f(x0)/m
        v_next = v0 + a * dt
    end subroutine

    subroutine verlet(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a, a_next
        a = f(x0)/m
        x_next = x0 + v0*dt + 0.5*a*(dt**2) 
        a_next = f(x_next)/m
        v_next = v0 +  0.5 * (a+a_next) * dt 
    end subroutine

    subroutine euler2(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a
        a = f(x0)/m
        x_next = x0 + v0 * dt + 0.5 * a * dt**2 
        v_next = v0 + a*dt + 0.5 * ((f_der(x0)*v0)/m) * (dt**2) 
    end subroutine

    subroutine euler3(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a
        a = f(x0)/m
        x_next = x0 + v0 * dt + 0.5 * a * dt**2 
        v_next = v0 + a*dt + 0.5 * ((f_der(x0)*v0)/m) * (dt**2) + (1/(6*m))*(f_der2(x0)*(v0**2)+ f_der(x0)*a) * (dt**3) 
    end subroutine

    subroutine rungekutta(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        REAL, DIMENSION(1:2) :: FQ, k1, k2, k3, k4, Q0, Q

        FQ(1) = v0
        FQ(2) = f(x0)/m
        Q0(1) = x0
        Q0(2) = v0

        k1 = dt * FQ
        k2 = dt * (FQ+0.5*k1)
        k3 = dt * (FQ+0.5*k2)
        k4 = dt * (FQ+k3)
        Q = Q0 + k1/6 + k2/3 + k3/3 + k4/6
        
        x_next = Q(1)
        v_next = Q(2)
    end subroutine

end module algorithms