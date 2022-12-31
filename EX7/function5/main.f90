program matrixmultiplication
    use construct
    use ownmatmul
    use trace

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
            print*, 'Dummy method, no multiplication'
        case (1)
            print*, 'DGEMM from BLAS selected '
            call cpu_time(start)
            call DGEMM(A,B)
            call cpu_time(finish)
        case (2)
            print*, 'own Method selected'
            call cpu_time(start)
            call multiply(dim, A, B, C)
            call cpu_time(finish)
        case (3)
            print*, 'Oh Fortran, my dear'
            call cpu_time(start)
            C = MATMUL(A, B)
            call cpu_time(finish)
    end select
  
    if ( method == 0 ) then
        print '("Time for filling the matrices = ",f6.3," seconds.")',finish_fill-start_fill
    else
        print '("First element of computed matrix = ",f6.3, "")', C(1,1)
        print '("Trace of computed matrix = ",f6.3, "")', tr(C, dim)
        print '("Time for matrix multiplication = ",f6.3," seconds.")',finish-start
    end if

    deallocate(A)
    deallocate(B)
    deallocate(C)
end program matrixmultiplication