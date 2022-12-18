program day01
    use iso_fortran_env, only: int32
    use stdlib_selection, only: select
    implicit none

    integer, parameter :: ip = int32
    integer(ip), allocatable :: elf_sums(:), elf_work(:)
    integer(ip) :: fid, i, nelves, top3, j, third_largest

    open(newunit=fid, file='data/input_day01.txt', form='formatted', action='read')
    allocate(elf_sums(2)) ! Grow as required
    elf_sums = 0
    nelves = 1
    do
        read(fid, '(I10)', end=20) i
        if(i == 0) then
            ! Grow array
            nelves = nelves + 1
            if(nelves > size(elf_sums)) elf_sums = [elf_sums, 0*elf_sums]
        else
            ! Add calories
            elf_sums(nelves) = elf_sums(nelves) + i
        end if
    end do
    20 close(fid)

    print*, 'The maximum calories: ', maxval(elf_sums)

    ! Stdlib approach to top 3 eleves
    elf_work = elf_sums(1:nelves)
    call select(elf_work, nelves-2, third_largest)
    print*, 'Sum of top 3 elves: ', sum(elf_work((nelves-2):))

end program
