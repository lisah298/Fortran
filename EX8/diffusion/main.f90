program diffusion
    use einstein
    use greenkubo
    implicit none
    real :: t_start, t_finish,  D, ta, te, dt
    integer :: method
    character(20):: filename
    integer :: molecules, sites
    real, allocatable :: r0(:,:,:), v0(:,:,:), r(:,:,:), v(:,:,:)
    !real, allocatable :: MSD(:)


    !let user enter file name
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    !read(*,*) filename
    filename = 'spc_full_no_pbc.gro'
    print*, 'How many water molecules does your system contain?'
    !read(*,*) molecules
    molecules = 221

    print*, 'How many sites/atoms does the molecules of your system contain?'
    !read(*,*) sites
    sites = 3

    !ask user about size of file
    print*, 'What is the timestep of your trajectory?'
    !read(*,*) dt
    dt = 0.002

    print*, 'Pleas enter start and end time for computation as ta te'
    read(*,*) ta, te


    print*, 'Which method do you want to use for computing the diffusion coefficient? 1: Einstein, 2: Green-Kubo'
    read(*,*) method
    
    select case (method)
    case (1)
        allocate(r0(1:molecules,1:sites,1:3))
        allocate(r(1:molecules,1:sites,1:3))
        call cpu_time(t_start)
        call diffusion_einstein(filename, molecules, sites, ta, te, dt, D)
        call cpu_time(t_finish)
        deallocate (r0)
        deallocate (r)
    case (2)
        allocate(v0(1:molecules,1:sites,1:3))
        allocate(v(1:molecules,1:sites,1:3))
        call cpu_time(t_start)
        call diffusion_greenkubo(filename, molecules, sites, ta, te, dt, D)
        call cpu_time(t_finish)
        deallocate (v0)
        deallocate (v)
    end select
   
   
    
    
  
    
    print*, 'Execution time = ', t_finish-t_start
   
    
    
end program diffusion