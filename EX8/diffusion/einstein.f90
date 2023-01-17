module einstein
    implicit none
    contains
    subroutine diffusion_einstein(filename, molecules, sites, t_ges,  dt, D)
        implicit none
        real, intent(in) ::  dt, t_ges
        integer, intent(in):: molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time
        real, intent(out) :: D
        real :: ta, te, msd, sum_D, t
        integer :: step,  atom, dt_i,Nts,i, k, step_a, step_e, steps, N, steps_ges, counter, increment
        real, allocatable :: pos(:, :, :)
        
    !open files
    open (2, file = filename, status = 'old')
    open (3, file = 'MSD.out', status = 'unknown')
    open (4, file = 'pos.out', status = 'unknown')
    write(3,*) '#dt   t[ps]   msd [nm^2/ps]   D    sum_D'
   
    print*, 'Einstein chosen.'
    print*, 'Please enter start and end time for computation!'
    read(*,*) ta, te

    print*, 'Please enter an increment to speed up the computation!'
    read(*,*) increment

    print*, 'Computation started.'
    
    step_a = INT(ta/dt)
    step_e = INT(te/dt)+1
    steps = step_e - step_a
    steps_ges = INT(t_ges/dt)

    !read in positions
    allocate(pos(1:3, 1:sites*molecules, 1:steps_ges))
    do step = 1,  steps_ges
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
        do atom = 1, sites*molecules
                read(2,*)  SOL, site, index, pos(1:3, atom, step), junk, junk, junk
                write(4, *) abs_step, atom, pos(1 ,atom, step), pos(2,atom, step), pos(3, atom, step)
        end do
        read(2,*)
    end do

    print*, 'Done with reading in positions!'

    counter = 0

    Nts = steps_ges
    sum_D = 0
    do dt_i = 1, steps_ges, increment
        N = Nts - dt_i
        msd = 0
        D = 0
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
        D = msd/(6.0*t)
        if ( dt_i >= step_a .and. dt_i < step_e ) then
            sum_D = sum_D + D
            counter = counter + 1
        end if
        write(3,*) dt_i, t, msd, D, sum_D
    end do
   
    print*, counter
    !compute D by averaging over all computed D's
    D=(sum_D/(counter))*(10**(-2.0))
    print*, 'D in cm^2/s=', D
    end subroutine

end module einstein