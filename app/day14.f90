program day14
    use stdlib_strings, only: find, count_string=>count
    implicit none
    integer, parameter :: air = 0, rock = 1, sand = 2
    logical, parameter :: verbose = .false.
    integer, parameter :: cl = 1024, max_steps=1000
    integer, allocatable :: cave(:,:), all_ends(:), backup_cave(:,:)
    integer :: fid, counter=0, ierr, i, j, k, n, j0, j1, jlast, i0, i1, inc, particle, part, sandi = 500, sandj=0, &
        ci0, ci1, cj0, cj1
    character(len=cl) :: c1, side
    character(len=cl), allocatable :: inputs_c(:)
    type array2d
        integer, allocatable :: x(:,:)
    end type
    type(array2d), allocatable :: inputs_i(:)


    open(newunit=fid, file='data/input_day14.txt', form='formatted', action='read')
    !open(newunit=fid, file='data/test_input_day14.txt', form='formatted', action='read')
   
    ! Read strings 
    allocate(inputs_c(2))
    reader: do
        read(fid, '(a)', iostat=ierr) c1
        if(ierr /= 0) exit reader
        counter = counter + 1
        if(size(inputs_c) < counter) inputs_c = [inputs_c, inputs_c]
        inputs_c(counter) = c1
    end do reader
    inputs_c = inputs_c(1:counter)

    print*, '# lines: ', counter

    ! Read lines
    allocate(inputs_i(counter))
    parser: do i = 1, counter
        c1 = inputs_c(i)
        n = count_string(c1, "->")
        allocate(inputs_i(i)%x(n+1, 2))
        j0 = 1
        do j = 1, n
            j1 = find(c1, "->", j) - 1
            if(verbose) print*, j0, j1, trim(c1(j0:j1))
            read(c1(j0:j1), *) inputs_i(i)%x(j, 1:2)
            j0 = j1 + 3
        end do
        if(verbose) print*, j0, 'Inf', trim(c1(j0:))
        read(c1(j0:len_trim(c1)), *) inputs_i(i)%x(n+1, 1:2)
    end do parser

    ! Find indices for the cave
    i0 = HUGE(1); i1 = -HUGE(1)
    j0 = HUGE(1); j1 = -HUGE(1)
    do i = 1, size(inputs_i)
        i0 = min(i0, minval(inputs_i(i)%x(:,1)))
        i1 = max(i1, maxval(inputs_i(i)%x(:,1)))
        j0 = min(j0, minval(inputs_i(i)%x(:,2)))
        j1 = max(j1, maxval(inputs_i(i)%x(:,2)))
        if(verbose) then
            print*, ' '
            print*, 'LINE ', i
            do j = 1, size(inputs_i(i)%x, 1)
                print*, inputs_i(i)%x(j,1:2)
            end do
        end if
    end do

    print*, 'Rock range: ', i0, i1, j0, j1
    j0 = min(0, j0) ! Sand comes in at j=0

    ci0 = i0
    ci1 = i1
    cj0 = j0
    cj1 = j1

    ! Parts 1 and 2
    problem_part: do part = 1, 2

        ! Populate the cave
        if(allocated(cave)) deallocate(cave)
        allocate(cave(ci0:ci1, cj0:cj1))
        cave = air
        do i = 1, size(inputs_i)
            do j = 1, size(inputs_i(i)%x, 1) - 1
                i0 = minval(inputs_i(i)%x(j:j+1,1))
                i1 = maxval(inputs_i(i)%x(j:j+1,1))
                j0 = minval(inputs_i(i)%x(j:j+1,2))
                j1 = maxval(inputs_i(i)%x(j:j+1,2))
                if(i0 == i1) then
                    if(j0 >= j1) stop 'j0 >= j1'
                    do k = j0, j1
                        cave(i0, k) = rock
                    end do
                else if(j0 == j1) then
                    if(i0 >= i1) stop 'i0 >= i1'
                    do k = i0, i1
                        cave(k, j0) = rock
                    end do
                else
                    stop 'diagonal line'
                end if
            end do
        end do

        if(allocated(backup_cave)) deallocate(backup_cave)
        allocate(backup_cave, source=cave)

        if(part == 2) then
            ! Add in a row at the bottom
            i0 = lbound(cave, dim=1)
            i1 = ubound(cave, dim=1)
            j0 = lbound(cave, dim=2)
            j1 = ubound(cave, dim=2)
            deallocate(cave)
            allocate(cave(i0:i1, j0:j1+2))
            cave(i0:i1, j0:j1) = backup_cave
            cave(i0:i1, j1+1) = air
            cave(i0:i1, j1+2) = rock
        end if

        !call print_cave

        particle = 0
        drop_sand: do
            particle = particle + 1

            i = sandi
            j = sandj
           
            one_particle: do 

                if(part == 2) then
                    ! Extend the cave if we are getting close to its boundaries
                    if(i < lbound(cave, dim=1) + 1) then
                        side = 'left'
                        call extend_cave(cave, backup_cave, side)
                    else if(i > ubound(cave, dim=1) - 1) then
                        side = 'right'
                        call extend_cave(cave, backup_cave, side)
                    end if
                end if

                j = j+1
                if(j > ubound(cave, dim=2)) then
                    print*, 'Exit via bottom for particle: ', particle
                    if(part == 2) stop 'ERROR: SHOULD NOT HAPPEN IN PART 2'
                    exit drop_sand
                end if

                if(cave(i,j-1) /= air) then
                    print*, 'ERROR: Particle held a non-air position'
                    call print_cave
                    stop
                end if
                
                if(cave(i,j) /= air) then
                    ! Cave(i,j) is rock or sand
                    if(i-1 < lbound(cave, dim=1)) then
                        print*, 'Exit via left for particle ', particle
                        exit drop_sand
                    end if
                    if(cave(i-1, j) == air) then
                        i = i-1
                        cycle one_particle
                    end if

                    ! Cave(i-1:i,j) is not air
                    if(i+1 > ubound(cave, dim=1)) then
                        print*, 'Exit via right for particle ', particle
                        exit drop_sand
                    end if
                    if(cave(i+1,j) == air) then
                        i = i+1
                        cycle one_particle
                    end if

                    ! cave(i-1:i+1,j) are rock or sand, so the sand settles in its previous position
                    cave(i,j-1) = sand
                    if(i == 500 .and. j - 1 == 0) then
                        ! Part 2
                        exit drop_sand
                    else
                        exit one_particle
                    end if
                end if
            end do one_particle
            !exit drop_sand
        end do drop_sand

        !call print_cave

        if(part == 1) then
            print*, 'Number of settled sand units (part ', part, '): ', particle -1, ', (check: ', count(cave > rock), ')'
        else if(part == 2) then
            print*, 'Number of settled sand units (part ', part, '): ', count(cave > rock)
        end if
    end do problem_part

    contains
    subroutine print_cave
        integer :: j
        character(len=cl) :: c1
        print*, ''
        print*, 'CAVE GEOMETRY'
        print*, ''
        write(c1, '(A7,I8,A2,A1)') '(I4,A1,', (ubound(cave, dim=1) - lbound(cave, dim=1) + 1), 'I1', ')'
        do j = lbound(cave, dim=2), ubound(cave, dim=2)
            print trim(c1), j, '-', cave(:,j)
        end do
    end subroutine

    subroutine extend_cave(cave, backup_cave, side)
        integer, allocatable, intent(inout) :: cave(:,:), backup_cave(:,:)
        character(len=cl), intent(in) :: side
        integer :: i0, i1, j0, j1, k0, k1

        i0 = lbound(cave, dim=1)
        i1 = ubound(cave, dim=1)
        j0 = lbound(cave, dim=2)
        j1 = ubound(cave, dim=2)

        deallocate(backup_cave)
        allocate(backup_cave, source=cave)
        if(side == 'left') then
            k0 = i0 - (i1 - i0)
            k1 = i1
        else if(side == 'right') then
            k0 = i0
            k1 = i1 + (i1 - i0)
        end if

        deallocate(cave)
        allocate(cave(k0:k1, j0:j1))
        cave = air
        cave(i0:i1, j0:j1) = backup_cave
        cave(:,j1) = rock
    end subroutine


end program day14
