program MDprogram
    implicit none
    real :: t_start, t_finish, msd
    !integer :: method
    character(20):: filename
    character(6) :: SOL, site, index
    character(8) :: junk, abs_step, time
    !character(1):: lines_known
    integer :: step, mol, atom, molecules, sites, steps, i
    real, allocatable :: r0(:,:,:), v0(:,:,:), r(:,:,:), v(:,:,:)
    !real, allocatable :: MSD(:)


    !let user enter file name
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    !read(*,*) filename
    filename = 'spc_short_20_25.gro'
    print*, 'How many water molecules does your system contain?'
    !read(*,*) molecules
    molecules = 221

    !print*, 'How many sites/atoms does the molecules of your system contain?'
    !read(*,*) sites
    sites = 3

    !ask user about size of file
    print*, 'How many steps does your trajectory have?'
    !read(*,*) steps

    steps= 500
    
    allocate(r0(1:molecules,1:sites,1:3))
    allocate(v0(1:molecules,1:sites,1:3))
    allocate(r(1:molecules,1:sites,1:3))
    allocate(v(1:molecules,1:sites,1:3))
    !allocate(MSD(steps))

    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
   
    call cpu_time(t_start)
   
    read(2,*) 
    read(2,*)
    do mol = 1, molecules
        do atom = 1, sites
            read(2,*)  SOL, site, index, r0(mol, atom, 1), r0(mol, atom, 2)&
            , r0(mol, atom, 3), v0(mol, atom, 1), v0(mol, atom, 2), v0(mol, atom, 3)
            !print*, r0(mol, atom, 1), r0(mol, atom, 2)&
            !, r0(mol, atom, 3), v0(mol, atom, 1), v0(mol, atom, 2), v0(mol, atom, 3)
        end do
        !print*, SOL
    end do

    !call exit()
    !iterate through steps
    do step = 1, steps
        !skip lines with number of molecules and time stats
        read(2,*)
        read(2,*) junk, junk, junk,  time, junk, abs_step
        read(2,*)
        msd = 0
        !print*, junk2, junk2, junk3, junk4
        !iterate through molecules of a single step
        do mol = 1, molecules
            !iterate
            do atom = 1, sites
                read(2,*)  SOL, site, index, r(mol, atom, 1), r(mol, atom, 2)&
                , r(mol, atom, 3), v(mol, atom, 1), v(mol, atom, 2), v(mol, atom, 3)
                !print*, r(mol, atom, 1), r(mol, atom, 2)&
                !, r(mol, atom, 3), v(mol, atom, 1), v(mol, atom, 2), v(mol, atom, 3)
                
                !iterate over coordinates for computing the msd
                do i = 1, 3
                    msd = msd + (r0(mol, atom, i)-r(mol, atom, i))**2
                    !print*, 'r0', r0(mol, atom, i), 'r', r(mol, atom, i)
                    
                end do
                !print*,'msd', msd
            end do
           !print*, SOL
           msd = msd/sites
        end do
        write(3,*) abs_step, time, msd
    end do
    

    !open file and write header line
    !open(1, file = 'MD.log',status='unknown')
   
    
    
  
    call cpu_time(t_finish)
    print '("Execution time = ",f6.5," seconds.")', t_finish-t_start
   
    deallocate (r0)
    deallocate (v0)
    deallocate (r)
    deallocate (v)
end program MDprogram