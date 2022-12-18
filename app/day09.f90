program day09
    ! To compile this I used 
    !     export FPM_FFLAGS="-O3 -fno-range-check"
    ! where the final flag is needed for stdlib hashmaps
    use stdlib_kinds, only: int8, int32
    use stdlib_hashmaps, only: chaining_hashmap_type, open_hashmap_type
    use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, set
    implicit none

    character(len=1), allocatable :: dir(:)    
    integer, allocatable :: steps(:)
    integer, parameter :: num_rope_knots_both_parts(2) = [2, 10]
    integer(int32), allocatable :: rope_p(:,:) ! Points for eack knot
    integer(int32) :: fid, Nsteps=0, i, j, k, n, ipart, ierr
    integer(int8) :: ki(2*4) ! Convert [int32, int32] to array of int8 for hash map
    character(len=1) :: c
    type(chaining_hashmap_type), allocatable :: map
    type(key_type) :: key
    logical :: has_key

    ! Count number of steps in file
    open(newunit=fid, file='data/input_day09.txt', form='formatted', action='read')
    countLines: do
        read(fid, *, iostat=ierr) c, n
        if(ierr /= 0) exit
        Nsteps = Nsteps + 1
    end do countLines
    rewind(fid)

    print*, 'Number of steps: ', Nsteps

    ! Read file
    allocate(steps(Nsteps), dir(Nsteps))
    do i = 1, Nsteps
        read(fid, *, iostat=ierr) dir(i), steps(i)
    end do

    ! Solve parts 1 and 2 (which have differing numbers of knots)
    do ipart = 1, 2

        n = num_rope_knots_both_parts(ipart)

        allocate(rope_p(2, n))
        rope_p = 0 ! Arbitrary starting position, the same for all knots

        ! Setup hash table to store visited locations
        allocate(map)
        call map%init(fnv_1_hasher)
        ! Record tail position
        ki = transfer(rope_p(:,n), ki)
        call set(key, ki)
        call map%key_test(key, has_key)
        if(.not. has_key) call map%map_entry(key)

        do i = 1, Nsteps
            do j = 1, steps(i)
                ! Move the head in direction dir(i)
                call move(rope_p(:,1), dir(i), rope_p(:,2))
                do k = 2, n - 1
                    ! The "head" has already moved as the tail of something else.
                    ! So just move the tail --> dir = '-'
                    call move(rope_p(:,k), '-', rope_p(:,k+1))
                end do
                ! Append the tail location to the hash table
                ki = transfer(rope_p(:,n), ki)
                call set(key, ki)
                call map%key_test(key, has_key)
                if(.not. has_key) call map%map_entry(key)
            end do
        end do

        print*, '# sites that tail visited, part ', ipart, ': ', map%entries()
        deallocate(map)
        deallocate(rope_p)
    end do

    contains
    subroutine move(head_p, dir, tail_p)
        integer(int32), intent(inout) :: head_p(2), tail_p(2)
        character(len=1), intent(in) :: dir

        integer(int32) :: pdiff(2)

        if(dir == 'U') then
            head_p(2) = head_p(2) + 1
        else if(dir == 'D') then
            head_p(2) = head_p(2) - 1
        else if(dir == 'R') then
            head_p(1) = head_p(1) + 1
        else if(dir == 'L') then
            head_p(1) = head_p(1) - 1
        else if(dir == '-') then
            ! Assume already moved
        else
            stop 'unknown dir'
        end if

        pdiff = head_p - tail_p
        ! If we are diagonally touching, tail_p stays put
        if(all(abs(pdiff) <= 1)) return

        ! Otherwise this should characterise how tail_p moves
        tail_p = tail_p + max(-1, min(pdiff, 1))

    end subroutine
end program
