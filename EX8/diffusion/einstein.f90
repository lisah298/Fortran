module einstein
    implicit none
    contains
    subroutine diffusion_einstein(filename, molecules, sites, ta, te, dt, D)
        implicit none
        real, intent(in) :: ta, te, dt
        integer, intent(in):: molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time, time0
        real, intent(out) :: D
        real :: r0(1:molecules,1:sites,1:3), r(1:molecules,1:sites,1:3)
        real :: msd, sum_msd, t, msd_av
        integer :: step, mol, atom, i, k, step_a, step_e,steps
        
    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
    write(3,*) 'step    ', 'Time [ps]    ', 'msd [nm^2/ps]'
   
    print*, 'Einstein chosen'
    
    !read in initial positions    
    read(2,*) junk, junk, junk, time0, junk, abs_step
    read(2,*)
    
    step_a = INT(ta/dt)+1
    step_e = INT(te/dt)+1
    steps = step_e - step_a


    do k = 1, step_a
        do mol = 1, molecules
            do atom = 1, sites
                read(2,*)  SOL, site, index, r0(mol, atom, 1), r0(mol, atom, 2), r0(mol, atom, 3), junk, junk, junk
            end do
        end do
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
    end do

    !start computing msd
    sum_msd = 0
    do step = 1, step_e-step_a
        msd = 0
        !iterate through molecules of a single step
        do mol = 1, molecules
            !iterate over atoms/sites ins molecule
            do atom = 1, sites
                read(2,*)  SOL, site, index, r(mol, atom, 1), r(mol, atom, 2)&
                , r(mol, atom, 3), junk, junk, junk
                
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
        !skip lines with number of molecules and time stats
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
    end do
    
    !compute D
    t = te - ta
    msd_av = (sum_msd)/steps
    D = ((2*msd_av)/(6*t*10**2))
    print*, 'D =', D

  
    
    end subroutine

end module einstein