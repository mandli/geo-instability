module sea_module

    implicit none
    save

    ! Constants
    real(kind=8), parameter :: pi = 4.d0*datan(1.d0)
    real(kind=8), parameter :: g = 9.8d0
    real(kind=8), parameter :: DEG2RAD = pi / 180.d0
    real(kind=8), parameter :: RAD2DEG = 180.d0 / pi
    real(kind=8), parameter :: omega = 2.0d0 * pi / 86164.2d0
    real(kind=8), parameter :: theta_0 = 0.d0
    integer, parameter :: coordinate_system = 1
    
    ! Physics
    real(kind=8) :: sea_level
    real(kind=8) :: manning_coefficient
    real(kind=8) :: dry_tolerance
    logical :: friction_forcing, coriolis_forcing

    ! Topography
    real(kind=8) :: topo_location, topo_left, topo_right

contains

    subroutine set_sea_data(path)
        
        implicit none

        character(len=*), intent(in) :: path

        integer, parameter :: DATA_UNIT = 13

        call opendatafile(DATA_UNIT, path)

        read(DATA_UNIT, "(d16.8)") sea_level
        read(DATA_UNIT, "(d16.8)") manning_coefficient
        read(DATA_UNIT, *)
        read(DATA_UNIT, "(d16.8)") dry_tolerance
        read(DATA_UNIT, *) friction_forcing
        read(DATA_UNIT, *) coriolis_forcing
        read(DATA_UNIT, *)
        read(DATA_UNIT, "(d16.8)") topo_location
        read(DATA_UNIT, "(d16.8)") topo_left
        read(DATA_UNIT, "(d16.8)") topo_right

        close(DATA_UNIT)
        
    end subroutine set_sea_data

    ! ==========================================================================
    !  Calculate the coriolis constant f
    !   If coordinate_system == 1 then
    !       A beta-plane approximation is used and y should be in meters
    !   if coordinate_system == 2 then
    !       Grid is in lat-long and y should be in degrees which is converted
    !       to radians
    ! ==========================================================================
    real(kind=8) pure function coriolis(y)

        implicit none
        
        ! Input
        real(kind=8), intent(in) :: y
        
        ! Locals
        real(kind=8) :: theta
        
        ! Assume beta plane approximation and y is in meters    
        if (coordinate_system == 1) then
            theta = y / 111d3 * DEG2RAD + theta_0
            coriolis = 2.d0 * omega * (sin(theta_0) + (theta - theta_0)     &
                                                    * cos(theta_0))
        else if (coordinate_system == 2) then        
            coriolis = 2.d0 * omega * sin(y * DEG2RAD)
        else
            ! Unknown coordinate system, return nothing
            coriolis = 0.d0
        endif
    end function coriolis

end module sea_module
