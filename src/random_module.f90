module random_module

    implicit none
    
contains

    subroutine init_random_seed()
        integer :: i, n, clock
        integer, dimension(:), ALLOCATABLE :: seed

        call random_seed(size = n)
        allocate(seed(n))

        call system_clock(COUNT=clock)

        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        call random_seed(PUT = seed)

        deallocate(seed)
    end subroutine

end module random_module