program matrixmultiplication
    use construct
    use MatMul
    implicit none
    integer :: dim, method
    real, dimension(:, :), allocatable :: A, B, C
    real :: start, finish, start_fill, finish_fill

    !User input
    print*, 'Please enter the dimensionality of your square matrices'
    read(*,*) dim

    !Ask user to select method
    print*, 'Which method would you like to use? 0: Dummy, 1: BLAS, 2: own, 3: intrinsic'
    read(*,*) method

    !allocate and fill matrices
    allocate(A(dim, dim))
    allocate(B(dim, dim))
    allocate(C(dim, dim))

    call cpu_time(start_fill)
    call fill(dim, A, B)
    call cpu_time(finish_fill)
   
    

    !switch for executing selected method
    select case (method)
        case (0)
            print*, 'Dummy, no multiplication'
            print '("Time = ",f6.3," seconds.")',finish_fill-start_fill
        case (1)
            call cpu_time(start)
            print*, 'BLAS'
            call cpu_time(finish)
            print '("Time = ",f6.3," seconds.")',finish-start
        case (2)
            print*, 'own Method selected'
            call cpu_time(start)
            call multiply(dim, A, B, C)
            call cpu_time(finish)
            print*, C(1,1)
            print*, C(2,2)
            print '("Time = ",f6.3," seconds.")',finish-start
        case (3)
            print*, 'Oh Fortran, my dear'
            call cpu_time(start)
            call cpu_time(finish)
            print '("Time = ",f6.3," seconds.")',finish-start
    end select
    
    
end program matrixmultiplication