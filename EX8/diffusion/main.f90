program diffusion
    use einstein
    implicit none
    real :: t_start, t_finish,  D
    integer :: method
    character(20):: filename
    integer :: molecules, sites, steps
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
    print*, 'How many steps does your trajectory have?'
    !read(*,*) steps
    steps = 50000
  

    print*, 'Which method do you want to use for computing the diffusion coefficient? 1: Einstein, 2: Green-Kubo'
    read(*,*) method
    
    select case (method)
    case (1)
        allocate(r0(1:molecules,1:sites,1:3))
        allocate(r(1:molecules,1:sites,1:3))
        call cpu_time(t_start)
        call diffusion_einstein(filename, steps, molecules, sites, D)
        call cpu_time(t_finish)
        deallocate (r0)
        deallocate (r)
    case (2)
        allocate(v0(1:molecules,1:sites,1:3))
        allocate(v(1:molecules,1:sites,1:3))
        !call verlet(timestep, pos, vel, mass, new_pos, new_vel)
        deallocate (v0)
        deallocate (v)
    end select
   
   
    
    
  
    
    print*, 'Execution time = ', t_finish-t_start
   
    
    
end program diffusion