subroutine qinit(meqn,mbc,mx,my,xlower,ylower,dx,dy,q,maux,aux)
    
    use random_module
    use sea_module, only: sea_level
    
    implicit none
    
    ! Subroutine arguments
    integer, intent(in) :: meqn,mbc,mx,my,maux
    real(kind=8), intent(in) :: xlower,ylower,dx,dy
    real(kind=8), intent(inout) :: q(meqn,1-mbc:mx+mbc,1-mbc:my+mbc)
    real(kind=8), intent(inout) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)
    
    ! Locals
    integer :: i,j,m

    real(kind=8) :: rando

    call init_random_seed()
    
    ! Set flat state based on sea_level
    q = 0.d0
    forall(i=1:mx, j=1:my)
        q(1,i,j) = max(0.d0, sea_level - aux(1,i,j))
    end forall

    do i=1,mx
        do j=1,my
            call random_number(rando)
            q(1,i,j) = max(0.0d0, (rando - 0.5d0) * 2.d-4 - aux(1,i,j))
!             print *,(rando - 0.5d0) * 2.d-4 - aux(1,i,j)
!             call random_number(rando)
!             q(2,i,j) = ((rando - 0.5d0) * 2.d-4) * q(1,i,j)
!             call random_number(rando)
!             q(3,i,j) = ((rando - 0.5d0) * 2.d-4) * q(1,i,j)
        enddo
    enddo

end subroutine qinit
