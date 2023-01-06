module greenkubo
    implicit none
    contains
    subroutine diffusion_greenkubo(filename, molecules, sites, ta, te, dt, D)
        implicit none
        real, intent(in) :: ta, te, dt
        integer, intent(in):: molecules, sites
        character(20), intent(in):: filename
        character(6) :: SOL, site, index
        character(12) :: junk, abs_step, time, time0
        real, intent(out) :: D
        real :: v0(1:molecules,1:sites,1:3), v(1:molecules,1:sites,1:3)
        real :: v_square, sum_vsquare, t, v_square_av
        integer :: step, mol, atom, i, k, step_a, step_e,steps
        
    open (2, file = filename, status = 'old')
   
   
    print*, 'Green-Kubo chosen. Computation started.'
    
    !read in initial positions    
    read(2,*) junk, junk, junk, time0, junk, abs_step
    read(2,*)
    
    !Compute first end last step based on user in
    step_a = INT(ta/dt)+1
    step_e = INT(te/dt)+1
    steps = step_e - step_a


    do k = 1, step_a
        do mol = 1, molecules
            do atom = 1, sites
                read(2,*)  SOL, site, index, junk, junk, junk, v0(mol, atom, 1), v0(mol, atom, 2), v0(mol, atom, 3)
            end do
        end do
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
    end do

    !start computing msd
    sum_vsquare = 0
    do step = 1, step_e-step_a
        v_square = 0
        !iterate through molecules of a single step
        do mol = 1, molecules
            !iterate over atoms/sites ins molecule
            do atom = 1, sites
                read(2,*)  SOL, site, index, junk, junk, junk, v(mol, atom, 1), v(mol, atom, 2)&
                , v(mol, atom, 3)
                
                !iterate over coordinates for computing the msd
                do i = 1, 3
                    v_square = v_square + ((v(mol, atom, i)*v0(mol, atom, i)))
                end do
            end do
           !sum_msd = sum_msd + msd
        end do
        v_square = (v_square/(sites*molecules))
        sum_vsquare = sum_vsquare + v_square
       
        !skip lines with number of molecules and time stats
        read(2,*)
        read(2,*) junk, junk, junk, time, junk, abs_step
        read(2,*)
    end do
    
    !compute D
    t = te - ta
    !msd_av = (sum_vsquare)/steps
    v_square_av = (sum_vsquare)/steps
    D = ((sum_vsquare*dt)/(3*10**2))
    print*, 'D =', D

  
    
    end subroutine

end module greenkubo