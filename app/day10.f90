program day10
    implicit none
    integer :: i, fid, toadd, ncycles, nlines, sprite(3)
    character(len=20) :: instruct
    integer, allocatable :: X(:), cycles(:)
    integer, parameter :: inds(6) = [20, 60, 100, 140, 180, 220]
    character(len=1), allocatable, target :: CRT_flat(:)
    character(len=1), pointer :: CRT(:,:)

    open(newunit=fid, file='data/input_day10.txt', form='formatted', action='read')
    !open(newunit=fid, file='data/input_day10_testonly.txt', form='formatted', action='read')

    ! Determine how many lines and cycles
    ncycles = 0
    nlines = 0
    do 
        read(fid, "(A5,I5)", blank='ZERO', end=20) instruct, toadd
        nlines = nlines + 1
        if(instruct == 'addx') then
            ncycles = ncycles + 2            
        else if(instruct == 'noop') then
            ncycles = ncycles + 1
        end if
    end do
    20 rewind(fid)

    ! Get the data
    allocate(X(0:ncycles), cycles(0:ncycles))
    X(0) = 1
    cycles(0) = 0
    ncycles = 0
    do i = 1, nlines
        read(fid, "(A5, I5)", blank='ZERO') instruct, toadd
        if(instruct == 'addx') then
            ncycles = ncycles + 1
            ! First instruction
            cycles(ncycles) = ncycles
            X(ncycles) = X(ncycles - 1)
            ! Second instruction
            ncycles = ncycles + 1
            cycles(ncycles) = ncycles
            X(ncycles) = X(ncycles - 1) + toadd
        else if(instruct == 'noop') then
            ncycles = ncycles + 1
            cycles(ncycles) = ncycles
            X(ncycles) = X(ncycles - 1)
        end if
    end do

    ! X records the value at the END of the cycle. To get values DURING
    ! a cycle we need to offset X by 1
    print*, 'Sum of scores = ', dot_product(cycles(inds),X(inds-1))

    allocate(CRT_flat(0:ncycles-1))
    CRT_flat = ""

    ! Part 2
    do i = 1, maxval(cycles, dim=1)
        sprite = X(i-1) + [-1, 0, 1]
        ! Here it wasn't obvious to me (from the problem text) that the sprite position is always
        ! on 'one row', and we colour the CRT if it's position matches that horizontally (whatever row it is on).
        if(any(sprite == mod((i-1), 40))) then 
            CRT_flat(i-1) = '#'
        else
            CRT_flat(i-1) = '.'
        end if
    end do

    CRT(0:39,1:6) => CRT_flat

    do i = 1, size(CRT, 2)
        print*, CRT(:,i)
    end do

end program
