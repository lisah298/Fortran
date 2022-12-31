program matrixmultiplication
    use construct
    use MatMul
    implicit none
    integer :: dim, method
    real, dimension(:, :), allocatable :: A, B, C

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
    call fill(dim, A, B)
    !print*, A(1,1), B(1,1) 
    !print*, A(2,2), B(2,2) 

    !switch for executing selected method
    select case (method)
        case (0)
            print*, 'Dummy'
        case (1)
            print*, 'BLAS'
        case (2)
            print*, 'own Method selected'
            call multiply(dim, A, B, C)
            print*, C(1,1)
            print*, C(2,2)
        case (3)
            print*, 'oFortran'
        case default
            print*, 'None'
    end select
    

    
end program matrixmultiplication