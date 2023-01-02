program MDprogram
    use algorithms
    use potential
    implicit none
    real :: t_start, t_finish, timestep, pos, vel,  new_pos, new_vel, ekin, epot , etot, t = 0, x_init = 1.2, v_init = 1.0, mass = 1
    integer :: method, i, steps

    !Ask user to select algorithm
    print*, 'Which method would you like to use? 0: 1st order Euler, 1: Velocity Verlet, 2: 2nd order Euler'
    read(*,*) method

    print*, 'How many time steps?'
    read(*,*) steps

    print*, 'Which time step?'
    read(*,*) timestep

    select case (method)
        case (0)
            print*, '1st order Euler was chosen'
        case (1)
            print*, 'Velocity Verlet was chosen'
        case (2)
            print*, '2nd order Euler'
        !case (3)
        !    print*, 'Oh Fortran, my dear'
        case default
            print*, 'default'
    end select 
    call cpu_time(t_start)
   
    pos = x_init
    vel = v_init
    open(1, file = 'MD.log',status='unknown')
    write(1,*) 'step    ', 't   ', 'x   ', 'v   '  , 'E_kin    ', 'E_pot   ', 'E_tot   '
    !switch for executing selected method
    do i = 0, steps
        !print*, 'pos', pos
        !print*, 'vel', vel
        ekin = 0.5*mass*(vel**2)
        epot = V(pos)
        etot= ekin + epot
        t = t + timestep
        write(1,*) i, t, pos, vel, ekin, epot, etot
        select case (method)
            case (0)
                call fo_euler(timestep, pos, vel, mass, new_pos, new_vel)
            case (1)
                call verlet(timestep, pos, vel, mass, new_pos, new_vel)
            case (2)
                call so_euler(timestep, pos, vel, mass, new_pos, new_vel)
        !case (3)
        !    print*, 'Oh Fortran, my dear'
        end select      
        pos = new_pos
        vel = new_vel
    end do
    
  
    call cpu_time(t_finish)
    print*, 'MD run completed. Output is printed in MD.log file.'
    print '("Execution time = ",f6.5," seconds.")', t_finish-t_start
   
end program MDprogram