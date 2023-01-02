program MDprogram
    implicit none
    real :: t_start, t_finish
    integer :: method
    character(20):: filename
    character(1):: lines_known
    integer :: i,io, nlines


    !let user enter file name
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    read(*,*) filename
    

    !ask user about size of file
    print*, 'Do you know how much lines your file contains? Enter y for yes and n for no.'
    read(*,*) lines_known
    if (lines_known == 'y') then
        print*, 'Hooray, we can save computation time! Please enter the number of lines.'
        read(*,*) nlines
    else if (lines_known == 'n') then
        !Count number of lines
        open (2, file = filename, status = 'old')
        nlines=0
        i=0
        do
            read (2,*, iostat=io) 
            !break loop at the end of the file
            if (io/=0) exit
            nlines = nlines + 1    
        end do
    end if

    select case (method)
        case (0)
            print*, '1st order Euler was chosen'
        case (1)
            print*, 'Velocity Verlet was chosen'
        case default
            print*, 'No method chosen. Program will be aborted!'
            call exit()
    end select 
   
    call cpu_time(t_start)
   
    

    !open file and write header line
    !open(1, file = 'MD.log',status='unknown')
   
    
    
  
    call cpu_time(t_finish)
    print*, 'MD run completed. Output is printed in MD.log file.'
    print '("Execution time = ",f6.5," seconds.")', t_finish-t_start
   
end program MDprogram