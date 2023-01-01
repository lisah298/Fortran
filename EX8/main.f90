program MDprogram
    use algorithms
    implicit none
    integer :: method, i, steps
    real :: t_start, t_finish
    real :: v0 = 1.2, x0 = 1.0, dt = 1, m = 1, x = 0, v = 0
   
   
    
    !Ask user to select algorithm
    print*, 'Which method would you like to use? 0: 1st order Euler, 1: Velocity Verlet, 2: 2nd order Euler'
    read(*,*) method

    print*, 'How many time steps?'
    read(*,*) steps


    call cpu_time(t_start)
   

    !switch for executing selected method
    do i = 0, steps
        !t = t + i*dt
        select case (method)
        case (0)
            print*, '1st order Euler was chosen'
            call fo_euler(dt, x0, v0, m, x, v)
            print*, x
            print*, v
        case (1)
            print*, 'Velocity Verlet was chosen'
        case (2)
            print*, '2nd order Euler'
        !case (3)
        !    print*, 'Oh Fortran, my dear'
        case default
            print*, 'default' 
    end select    
    
end do
    
  
    call cpu_time(t_finish)
    print '("Execution time = ",f6.5," seconds.")', t_finish-t_start
   
end program MDprogram