program diffusion
    use einstein
    use greenkubo
    implicit none
    real :: t_start, t_finish,  D, ta, te, dt
    integer :: method
    character(20):: filename
    integer :: molecules, sites

    !let user enter information
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    read(*,*) filename
   
    print*, 'How many water molecules does your system contain?'
    molecules = 221

    print*, 'How many sites/atoms does the molecules of your system contain?'
    sites = 3

    print*, 'What is the timestep of your trajectory?'
    read(*,*) dt

    print*, 'Pleas enter start and end time for computation as ta te'
    read(*,*) ta, te

    print*, 'Which method do you want to use for computing the diffusion coefficient? 1: Einstein, 2: Green-Kubo'
    read(*,*) method
    
    !start computation
    call cpu_time(t_start)
    select case (method)
    case (1)
        call diffusion_einstein(filename, molecules, sites, ta, te, dt, D)
    case (2)
        call diffusion_greenkubo(filename, molecules, sites, ta, te, dt, D)
    end select
    
    call cpu_time(t_finish)
    print*, 'Computation is done. Execution time = ', t_finish-t_start
   
end program diffusion