module algorithms
    use potential
    implicit none
    contains
    subroutine fo_euler(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a
        x_next = x0 + v0 * dt
        !print*, x_next
        a = f(x0)/m
        v_next = v0 + a * dt
        !print*, v_next
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

    subroutine so_euler(dt, x0, v0, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x0, v0, m
        real, intent(out) :: x_next, v_next
        real :: a
        a = f(x0)/m
        x_next = x0 + v0 * dt + 0.5 * a * dt**2 
        !print*, x_next
        v_next = v0 + a*dt + 0.5 * ((f_der(x0)*v0)/m) * (dt**2) 
        !print*, v_next
    end subroutine



end module algorithms