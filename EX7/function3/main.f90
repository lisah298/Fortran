program main
    use func
    implicit none
    real :: start, end, increment, x

    !User input section
    print*, 'Please enter the start value'
    read(*,*) start
    print*, 'Please enter the end value'
    read(*,*) end
    print*, 'Please enter the increment'
    read(*,*) increment
    x = start
    
    !Open new file
    open(1, file = 'data.dat',status='new')

    !Compute value of function for each x and write into file
    do while (x < end)
        write(1,*) x, yourfunc(x)
        x = x + increment
    end do

end program main