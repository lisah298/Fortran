program matrixmultiplication
    use construct
    use ownmatmul
    use trace
    use evalMem

    implicit none
    integer :: dim, method
    real, dimension(:, :), allocatable :: A, B, C
    real :: start, finish, start_fill, finish_fill
    character(len=5) :: memory_start, memory_end
    
    !User input
    print*, 'Please enter the dimensionality of your square matrices'
    read(*,*) dim

    !Ask user to select method
    print*, 'Which method would you like to use? 0: Dummy, 1: BLAS, 2: own, 3: intrinsic'
    read(*,*) method

    !allocate matrices
    allocate(A(dim, dim))
    allocate(B(dim, dim))
    allocate(C(dim, dim))

    !fille matrices and measure time
    call cpu_time(start_fill)
    call fill(dim, A, B)
    call cpu_time(finish_fill)
   
    call system_mem_usage(memory_start)

    !switch for executing selected method
    select case (method)
        case (0)
            print*, 'Dummy method, no multiplication'
        case (1)
            print*, 'DGEMM from BLAS selected '
            call cpu_time(start)
            call DGEMM('n', 'n', dim, dim, dim, 1.0, A, dim, B, dim, 1.0, C, dim)
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
        print '("Time for filling the matrices = ",f6.5," seconds.")',finish_fill-start_fill
    else
        call system_mem_usage(memory_end)
        print *,"First element of computed matrix = ", C(1,1)
        print *,"Trace of computed matrix = ", tr(C, dim)
        print *, "Time for matrix multiplication in seconds = ",finish-start
        print*, 'Memory usage before multiplication = ', memory_start
        print*, 'Memory usage after multiplication = ', memory_end
    end if

    deallocate(A)
    deallocate(B)
    deallocate(C)
end program matrixmultiplication