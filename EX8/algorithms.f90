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
        a = LJ_potential(x0)/m
        v_next = v0 + a * dt

    end subroutine

    
end module algorithms