subroutine setaux(mbc,mx,my,xlower,ylower,dx,dy,maux,aux)

    use sea_module, only: topo_location, topo_left, topo_right

    implicit none

    integer, intent(in) :: mbc, mx, my, maux
    real(kind=8), intent(in) :: xlower, ylower, dx, dy
    real(kind=8), intent(in out) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)

    integer :: i, j
    real(kind=8) :: x, y

    do j=1-mbc,my+mbc
        y = ylower + (j - 0.5d0) * dy 
        do i=1-mbc,mx+mbc
            x = xlower + (i - 0.5d0) * dx    
            if (x < topo_location) then
                aux(1,i,j) = topo_left
            else
                aux(1,i,j) = topo_right
            end if
        end do
    end do

end subroutine setaux