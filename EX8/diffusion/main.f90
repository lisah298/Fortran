program MDprogram
    implicit none
    real :: t_start, t_finish, msd, msd_av, sum_msd, t, t0, dt, D
    !integer :: method
    character(20):: filename
    character(6) :: SOL, site, index
    character(12) :: junk, abs_step, time, time0
    !character(1):: lines_known
    integer :: step, mol, atom, molecules, sites, steps, i
    real, allocatable :: r0(:,:,:), v0(:,:,:), r(:,:,:), v(:,:,:)
    !real, allocatable :: MSD(:)


    !let user enter file name
    print*, 'Please enter the name of your .gro file containing the trajectory.'
    !read(*,*) filename
    filename = 'spc_full_no_pbc.gro'
    print*, 'How many water molecules does your system contain?'
    !read(*,*) molecules
    molecules = 221

    !print*, 'How many sites/atoms does the molecules of your system contain?'
    !read(*,*) sites
    sites = 3

    !ask user about size of file
    print*, 'How many steps does your trajectory have?'
    !read(*,*) steps

    steps = 50000
    
    allocate(r0(1:molecules,1:sites,1:3))
    allocate(v0(1:molecules,1:sites,1:3))
    allocate(r(1:molecules,1:sites,1:3))
    allocate(v(1:molecules,1:sites,1:3))
    !allocate(MSD(steps))

    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
    write(3,*) 'step', 'time', 'msd'
   
    call cpu_time(t_start)
   
    read(2,*) junk, junk, junk, time0, junk, abs_step
    read(2,*)
    do mol = 1, molecules
        do atom = 1, sites
            read(2,*)  SOL, site, index, r0(mol, atom, 1), r0(mol, atom, 2)&
            , r0(mol, atom, 3), v0(mol, atom, 1), v0(mol, atom, 2), v0(mol, atom, 3)
            print*, r0(mol, atom, 1), r0(mol, atom, 2)&
            , r0(mol, atom, 3), v0(mol, atom, 1), v0(mol, atom, 2), v0(mol, atom, 3)
        end do
        print*, SOL
    end do

    !call exit()
    !iterate through steps
    sum_msd = 0
    do step = 1, steps
        !skip lines with number of molecules and time stats
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
        msd = 0
        !print*, time
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
                    msd = msd + ((r(mol, atom, i)-r0(mol, atom, i))**2)
                    !print*, 'r0', r0(mol, atom, i), 'r', r(mol, atom, i)
                    
                end do
                !print*,'msd', msd
            end do
           !print*, SOL
           !sum_msd = sum_msd + msd
        end do
        msd = (msd/(sites*molecules))
        !sum_msd = sum_msd + msd
        write(3,*) abs_step, time, msd
    end do
    
    !print*
    read(time,*)  t
    read(time0,*)  t0
    dt = (t - t0)/steps

    print*, 'Timestep=', dt
    print*, 'Time t=', t
    print*, 'Time t0=', t0
    print*, 'Time=', time
    print*, 'Time0=', time0
    msd_av = (sum_msd * dt)/t
    D = (msd_av/(6*t)) * 10**3
    print*, 'D=', D

    !open file and write header line
    !open(1, file = 'MD.log',status='unknown')
   
    
    
  
    call cpu_time(t_finish)
    print*, 'Execution time = ', t_finish-t_start
   
    deallocate (r0)
    deallocate (v0)
    deallocate (r)
    deallocate (v)
end program MDprogram