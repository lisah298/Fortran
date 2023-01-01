program MDprogram
    use algorithms
    implicit none
    integer :: method
    real :: start, finish
   
    

    !User input
   
    !Ask user to select algorithm
    print*, 'Which method would you like to use? 0: 1st order Euler, 1: Velocity Verlet, 2: 2nd order Euler'
    read(*,*) method


    call cpu_time(start)
   

    !switch for executing selected method
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
  
    call cpu_time(finish)
    print '("Execution time = ",f6.5," seconds.")',finish-start
   
end program MDprogram