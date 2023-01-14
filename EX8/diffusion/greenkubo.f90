module greenkubo
    implicit none
    contains
    subroutine diffusion_greenkubo(filename, molecules, sites, ta, te, dt, D)
        implicit none
        real, intent(in) :: ta, te, dt
        integer, intent(in):: molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time
        real, intent(out) :: D
        real :: v_square, riemann_sum, t
        integer :: step,  atom, dt_i,Nts, i, k, step_a, step_e, steps, N
        real, allocatable :: vel(:, :, :)
        
    
   
   
    print*, 'Green-Kubo chosen. Computation started.'

    open (2, file = filename, status = 'old')
    open (3, file = 'vel_auto.out', status = 'unknown')
    open (4, file = 'vel.out', status = 'unknown')
    write(3,*) '#dt       t       v^2 [nm^2/ps^2]       integral'
   
    step_a = INT(ta/dt)
    step_e = INT(te/dt)+1
    steps = step_e - step_a
    print*, steps
    allocate(vel(1:3, 1:sites*molecules, 1:steps))
   

    do step = 1,  steps+1
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
        do atom = 1, sites*molecules
                read(2,*)  SOL, site, index, junk, junk, junk, vel(1:3, atom, step)
                write(4, *) abs_step, atom, vel(1 ,atom, step), vel(2,atom, step), vel(3, atom, step)
        end do
        read(2,*)
    end do
  
    riemann_sum = 0
    Nts = steps+1
    do dt_i = 1, Nts
        !print*, dt_i
        N = Nts - dt_i
        v_square = 0
        !iterate over all possible combinations for dt=dt_i
        do k = 1 , Nts - dt_i
            !iterate over all atoms for one dt
            do atom = 1, sites*molecules
                do i = 1, 3
                    v_square = v_square + (vel(i, atom, k) * vel(i, atom, k+dt_i))
                end do
            end do
        end do
        v_square = v_square/(sites*molecules)
        v_square = v_square /(Nts - dt_i)
        t = dt_i * dt
        riemann_sum = riemann_sum + v_square * dt
        write(3,*) dt_i, t, v_square, riemann_sum 
    end do
    
    D = ((riemann_sum)/(3.0)*10**(-2.0))
    print*, 'D [cm^2/s] =', D

  
    
    end subroutine

end module greenkubo