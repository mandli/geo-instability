subroutine src2(meqn,mbc,mx,my,xlower,ylower,dx,dy,q,maux,aux,t,dt)
    
    use sea_module, only: g
    use sea_module, only: coriolis_forcing, coriolis
    use sea_module, only: friction_forcing, manning_coefficient, dry_tolerance

    implicit none
    
    ! Input parameters
    integer, intent(in) :: meqn,mbc,mx,my,maux
    double precision, intent(in) :: xlower,ylower,dx,dy,t,dt
    
    ! Output
    double precision, intent(inout) :: q(meqn,1-mbc:mx+mbc,1-mbc:my+mbc)
    double precision, intent(inout) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)

    ! Locals
    integer :: i, j, n
    real(kind=8) :: h, hu, hv, tau, xc, yc, fdt, a(2,2)

    ! Algorithm parameters
    ! Parameter controls when to zero out the momentum at a depth in the
    ! friction source term
    real(kind=8), parameter :: friction_tolerance = 1.0d-30
    real(kind=8), parameter :: exponent = 7.d0 / 3.d0

    real(kind=8) :: u,v

    ! ----------------------------------------------------------------
    ! Friction source term - Use backward Euler to solve
    ! Hybrid friction formula with a spatially varying Manning's-N factor
    if (friction_forcing) then
        do j=1,my
            yc = ylower + (j - 0.5d0) * dy
            do i=1,mx
                xc = xlower + (i - 0.5d0) * dx
                if (q(1,i,j) < friction_tolerance) then
                    q(2:3,i,j) = 0.d0
                else
                    tau = g * manning_coefficient**2 / q(1,i,j)**exponent &
                            * sqrt(q(2,i,j)**2 + q(3,i,j)**2)
                    q(2,i,j) = q(2,i,j) / (1.d0 + dt * tau)
                    q(3,i,j) = q(3,i,j) / (1.d0 + dt * tau)
                endif
            enddo
        enddo
    endif
    ! End of friction source term

    ! Coriolis source term
    ! Use backward Euler to solve, q_t = A q -> q^n+1 = (I + dt * A)^-1 q^n
    if (coriolis_forcing) then
        do j=1,my
            yc = ylower + (j - 0.5d0) * dy
            fdt = coriolis(yc) * dt ! Calculate f dependent on coordinate system

            ! Calculate matrix components
            a(1,:) = [1.d0,  fdt]
            a(2,:) = [-fdt, 1.d0]
            a = a / (1.d0 + fdt**2)

            do i=1,mx
                hu = q(2,i,j)
                hv = q(3,i,j)
                q(2,i,j) = hu * a(1,1) + hv * a(1,2)
                q(3,i,j) = hu * a(2,1) + hv * a(2,2)
            enddo
        enddo
    endif
    ! End of coriolis source term

end subroutine src2
