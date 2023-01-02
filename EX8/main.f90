program MDprogram
    use algorithms
    use potential
    implicit none
    real :: t_start, t_finish, timestep, pos, vel,  new_pos, new_vel, ekin, epot , etot, t = 0, x_init = 1.2, v_init = 1.0, mass = 1
    integer :: method, i, steps

    !Ask user to select algorithm
    print*, 'Which method would you like to use? 0: 1st order Euler, &
    1: Velocity Verlet, 2: 2nd order Euler, 3: 3rd order Euler, 4: Runge-Kutta.'
    read(*,*) method

    select case (method)
        case (0)
            print*, '1st order Euler was chosen'
        case (1)
            print*, 'Velocity Verlet was chosen'
        case (2)
            print*, '2nd order Euler was chosen'
        case (3)
            print*, '3rd order Euler was chosen'
        case (4)
            print*, 'Runge-Kutta was chosen'
        case default
            print*, 'No method chosen. Program will be aborted!'
            call exit()
    end select 
    
    print*, 'How many time steps?'
    read(*,*) steps

    print*, 'Which time step?'
    read(*,*) timestep

    call cpu_time(t_start)
   
    !initial conditions
    pos = x_init
    vel = v_init

    !open file and write header line
    open(1, file = 'MD.log',status='unknown')
    write(1,*) 'step    ', 't   ', 'x   ', 'v   '  , 'E_kin    ', 'E_pot   ', 'E_tot   '
    
    !switch for propagating the simulation
    do i = 0, steps
        ekin = 0.5*mass*(vel**2)
        epot = V(pos)
        etot= ekin + epot
        t = t + timestep
        write(1,*) i, t, pos, vel, ekin, epot, etot
        select case (method)
            case (0)
                call euler1(timestep, pos, vel, mass, new_pos, new_vel)
            case (1)
                call verlet(timestep, pos, vel, mass, new_pos, new_vel)
            case (2)
                call euler2(timestep, pos, vel, mass, new_pos, new_vel)
            case (3)
               call euler3(timestep, pos, vel, mass, new_pos, new_vel)
            case (4)
                call rungekutta(timestep, pos, vel, mass, new_pos, new_vel)
            case default
                print*, 'Something went wrong. Program will be aborted'
                call exit()
        end select      
        pos = new_pos
        vel = new_vel
    end do
    
  
    call cpu_time(t_finish)
    print*, 'MD run completed. Output is printed in MD.log file.'
    print '("Execution time = ",f6.5," seconds.")', t_finish-t_start
   
end program MDprogram