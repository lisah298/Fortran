module einstein
    implicit none
    contains
    subroutine diffusion_einstein(filename, molecules, sites, ta, te, dt, D)
        implicit none
        real, intent(in) :: ta, te, dt
        integer, intent(in):: molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time
        real, intent(out) :: D
        real :: msd, t
        integer :: step,  atom, dt_i,Nts,i, k, step_a, step_e, steps, N
        real, allocatable :: pos(:, :, :)
        !real, allocatable :: MSDs(:)
        

        
    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
    open (4, file = 'pos.out', status = 'unknown')
    write(3,*) '#dt   ', 'msd [nm^2/ps]'
   
    print*, 'Einstein chosen. Computation started.'
    
   
    
    step_a = INT(ta/dt)
    step_e = INT(te/dt)+1
    steps = step_e - step_a
    print*, steps
    allocate(pos(1:3, 1:sites*molecules, 1:steps))
    !allocate(MSD(1:3))

    do step = 1,  steps+1
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
        do atom = 1, sites*molecules
                read(2,*)  SOL, site, index, pos(1:3, atom, step), junk, junk, junk
                write(4, *) abs_step, atom, pos(1 ,atom, step), pos(2,atom, step), pos(3, atom, step)
        end do
        read(2,*)
    end do
  
    Nts = steps+1
    do dt_i = 1, Nts
        !print*, dt_i
        N = Nts - dt_i
        msd = 0
        !iterate over all possible combinations for dt=dt_i
        do k = 1 , Nts - dt_i
            !iterate over all atoms for one dt
            do atom = 1, sites*molecules
                do i = 1, 3
                    msd = msd + (pos(i, atom, k) - pos(i, atom, k+dt_i))**2
                end do
            end do
        end do
        msd = msd/(sites*molecules)
        msd = msd /(Nts - dt_i)
        t = dt_i * dt
        write(3,*) dt_i, t, msd 
    end do
   
    !compute D
    !t = te - ta
    !msd_av = (sum_msd)/steps
    !D = sum_msd/(6*t)
    !print*, 'D =', D
    D=3
  
    
    end subroutine

end module einstein