module einstein
    implicit none
    contains
    subroutine diffusion_einstein(filename, steps, molecules, sites, D)
        implicit none
        !real, intent(in) :: dt
        integer, intent(in):: steps, molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time, time0
        real, intent(out) :: D
        real :: r0(1:molecules,1:sites,1:3), r(1:molecules,1:sites,1:3)
        real :: msd, sum_msd, t, t0, dt, msd_av
        integer :: step, mol, atom, i
        
    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
    write(3,*) 'step    ', 'time    ', 'msd '
   
    print*, 'Einstein chosen'
    !read in initial positions    
    read(2,*) junk, junk, junk, time0, junk, abs_step
    read(2,*)
    do mol = 1, molecules
        do atom = 1, sites
            read(2,*)  SOL, site, index, r0(mol, atom, 1), r0(mol, atom, 2), r0(mol, atom, 3)
        end do
    end do


    !iterate through steps
    sum_msd = 0
    do step = 1, steps
        !skip lines with number of molecules and time stats
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
        msd = 0
        !iterate through molecules of a single step
        do mol = 1, molecules
            !iterate over atoms/sites ins molecule
            do atom = 1, sites
                read(2,*)  SOL, site, index, r(mol, atom, 1), r(mol, atom, 2)&
                , r(mol, atom, 3)
                
                !iterate over coordinates for computing the msd
                do i = 1, 3
                    msd = msd + ((r(mol, atom, i)-r0(mol, atom, i))**2)
                    
                end do
            end do
           !sum_msd = sum_msd + msd
        end do
        msd = (msd/(sites*molecules))
        sum_msd = sum_msd + msd
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
    msd_av = (sum_msd)/steps
    D = ((2*msd_av)/(6*t*10**2))
    print*,'msd_av= ', msd_av
    !D = ((msd_av/(6)))
    print*, 'D=', D

   
    
    
  
    
    end subroutine

end module einstein