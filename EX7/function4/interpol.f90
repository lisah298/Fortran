program interpolation
    implicit none
    real, allocatable :: x(:), y(:)
    character(20):: filename
    real :: x_val, x_1, x_2, y_interpol, m
    integer :: i,io, nlines

    !let user select value to be interpolated
    print*, 'Please enter the name of your file.'
    read(*,*) filename
    print*, filename
    

    !First count number of lines
    open (2, file = filename, status = 'old')
    nlines=0
    i=0
    do
        read (2,*, iostat=io) 
        !break loop at the end of the file
        if (io/=0) exit
        nlines = nlines + 1    
    end do

    !allocate array with size of lines in file and read in array
    open (3, file = filename, status = 'old')
    allocate(x(nlines))
    allocate(y(nlines))
    do i = 1, nlines
        read(3,*) x(i), y(i)
    end do 

    !let user select value to be interpolated
    print*, 'Please enter the x value to be interpolated'
    read(*,*) x_val

    !Check whether x value is in interval of the given data
    !If values lies not in the interval of the given data points a linear extrapolation will be used with a warning
    if ( x_val <= x(1) ) then
        m = (y(2)-y(1))/(x(2)-x(1))
        y_interpol= m*(x_val-x(1))+y(1)
        print*, 'WARNING! Your x is lower then all available datapoints. Linear extrapolation of lowest two datapoints is used.' 
        print*, 'Extrapolated value is', y_interpol 
    else if ( x_val > x(nlines) ) then
        m = (y(nlines)-y(nlines+1))/(x(nlines)-x(nlines+1))
        y_interpol= m*(x_val-x(nlines))+y(nlines)
        print*, 'WARNING! Your x is higher then all available datapoints. Linear extrapolation of largest two data points is used.'
        print*, 'Extrapolated value is', y_interpol 
    else

    do i = 1, nlines-1
        x_1=x(i)
        x_2=x(i+1)
        if ( x_1 < x_val .and. x_2>=x_val ) then
            m = (y(i+1)-y(i))/(x_2-x_1)
            y_interpol= m*(x_val-x_1)+y(i)
            print*, 'Interpolated value is', y_interpol 
           
            exit          
        end if
        !This case should not occur because of l.34-44. However it remains as error handler
        if ( i == nlines-1 ) then
            print*, 'Your x is not in interval of computed data. Program will be terminated' 
            call exit          
        end if
        
    end do 
end if
    deallocate (x)
    deallocate (y)
end program interpolation