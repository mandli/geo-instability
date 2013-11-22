subroutine setprob()

    use sea_module, only: set_sea_data

    implicit none

    call set_sea_data("sea.data")

end subroutine setprob