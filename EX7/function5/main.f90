program matrixmultiplication
    use construct
    implicit none
    integer :: dim, method
    real, dimension(:, :), allocatable :: A, B

    !User input
    print*, 'Please enter the dimensionality of your square matrices'
    read(*,*) dim

    print*, 'Which method would you like to use? 0: Dummy, 1: BLAS, 2: own, 3: intrinsic'
    read(*,*) method
    !allocate and fill matrices
    allocate(A(dim, dim))
    allocate(B(dim, dim))
    call fill(dim, A, B)
    print*, A(1,1), B(1,1) 
    print*, A(2,2), B(2,2) 

    !Ask user to select method
    
    
end program matrixmultiplication