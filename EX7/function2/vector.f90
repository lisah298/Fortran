program vector
    implicit none
    integer :: k, i, j, dim
    real :: angle, len_vec1, len_vec2 ,square_vec1, square_vec2, dot_prod
    real, allocatable :: vec1(:), vec2(:)

    !User input
    print*, 'Please enter the dimensionality of your vectors'
    read(*,*) dim
    
    allocate(vec1(dim))
    allocate(vec2(dim))
    
    print*, 'Please enter your first vector as x1 x2 ... xn'
    read(*,*) (vec1(i), i=1, dim) 

    print*, 'Please enter your second vector as x1 x2 ... xn'
    read(*,*) (vec2(k), k=1, dim) 
    
    !Computation of scalar products
    do j = 1, dim
       dot_prod = dot_prod + vec2(j)*vec1(j)  
       square_vec1 = square_vec1 + vec1(j)*vec1(j)
       square_vec2 = square_vec2 + vec2(j)*vec2(j)
    end do
    
    !Computation of result
    len_vec1 = SQRT(square_vec1)
    len_vec2 = SQRT(square_vec2)
    angle = ACOS(dot_prod/(len_vec1*len_vec2))

    
    !Print results to console
    print*, 'The angle between your two vectors in radians is', angle
    print*, 'The angle between your two vectors in degrees is', angle*180/3.14159265359
    
    deallocate (vec1)
    deallocate (vec2)
end program vector