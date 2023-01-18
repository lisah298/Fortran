program diffusion
    use einstein
    use greenkubo
    implicit none
    real :: t_start, t_finish,  D, dt, t
    integer :: method
    character(20):: filename
    integer :: molecules, sites

    !let user enter information
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    read(*,*) filename
   
    print*, 'How many water molecules does your system contain?'
    read(*,*) molecules 

    print*, 'How many sites/atoms does each molecule of your system contain?'
    read(*,*) sites 

    print*, 'What is the full time length of your trajectory in ps?'
    read(*,*) t

    print*, 'What is the timestep of your trajectory in ps?'
    read(*,*) dt

    print*, 'Which method do you want to use for computing the diffusion coefficient? 1: Einstein, 2: Green-Kubo'
    read(*,*) method
    
    !start computation
    call cpu_time(t_start)
    select case (method)
    case (1)
        call diffusion_einstein(filename, molecules, sites, t, dt, D)
    case (2)
        call diffusion_greenkubo(filename, molecules, sites, t, dt, D)
    end select
    
    call cpu_time(t_finish)
    print*, 'Computation is done. Execution time = ', t_finish-t_start
   
end program diffusion