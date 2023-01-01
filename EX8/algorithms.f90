module algorithms
    implicit none
    contains
    subroutine fo_euler(dt, x, v, m, x_next, v_next)
        implicit none
        real, intent(in) :: dt, x, v, m
        real, intent(out) :: x_next, v_next
        real :: a
        x_next = x + v*dt
        a = m
        v_next = v + a*dt

    end subroutine

    
end module algorithms