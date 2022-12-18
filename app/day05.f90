program day05
    use stdlib_ascii, only: reverse
    implicit none
    integer, parameter :: num_stacks=9
    character(len=1024) :: stacks(num_stacks), c1, c2, c3, stacks2(num_stacks)
    integer :: fid, i=0, j, k, n, s1, s2

    open(newunit=fid, file='data/input_day05.txt', form='formatted', action='read')

    stacks = ""

    read_stack: do
        read(fid, '(a)') c1
        if(c1(1:1) == '[') then
            i = i+1
            ! Add to the data structure
            do j = 1, num_stacks
                k = (j-1)*4 + 2
                stacks(j) = trim(stacks(j)) // c1(k:k)
            end do
        else
            exit read_stack
        end if
    end do read_stack
    print*, 'Read ', i, ' levels of stacks'
    print*, 'Starting position: '
    call print_stack(stacks)

    stacks2 = stacks

    ! Operate on the stack
    do
        read(fid, *, end=20) c1, n, c2, s1, c3, s2
        ! Part 1
        c1 = stacks(s1)
        c2 = stacks(s2)
        stacks(s2) = reverse(c1(1:n)) // trim(c2)
        stacks(s1) = c1(n+1:)

        ! Part 2
        c1 = stacks2(s1)
        c2 = stacks2(s2)
        stacks2(s2) = c1(1:n) // trim(c2)
        stacks2(s1) = c1(n+1:)

    end do         
    20 close(fid)

    print*, '##########################'
    print*, 'Ending position, part 1:'
    call print_stack(stacks)

    print*, '##########################'
    print*, 'Ending position, part 2:'
    call print_stack(stacks2)

    print*, ''

    print*, 'Top level (part 1): '
    do j = 1, num_stacks
        write(*, '(A1)', advance='no') stacks(j)
    end do
    write(*,*)""

    print*, 'Top level (part 2): '
    do j = 1, num_stacks
        write(*, '(A1)', advance='no') stacks2(j)
    end do
    write(*,*)""

    contains 
        subroutine print_stack(stacks)
            character(len=1024) :: stacks(num_stacks)
            integer :: j
            do j = 1, num_stacks
                write(*, '(A6, i2, A2, A)'), 'Stack', j, ': ', trim(stacks(j))
            end do
        end subroutine

end program
