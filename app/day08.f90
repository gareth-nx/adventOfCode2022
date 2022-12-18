program day08
    implicit none
    integer, allocatable :: th(:,:), scene_score(:,:)
    integer :: nx, ny, fid, j, i, ierr, s(4)
    character(len=2048) :: c1
    logical, allocatable :: can_see(:,:)
    logical :: is_visible

    ! Get nx/ny in array
    open(newunit=fid, file='data/input_day08.txt', form='formatted', action='read') 
    ny = 0
    readonce: do
        read(fid, '(a)', iostat=ierr) c1
        if(ierr /= 0) exit readonce
        ny = ny+1
        nx = len_trim(c1)
    end do readonce
    close(fid)

    print*, 'nx: ', nx, ', ny: ', ny
    allocate(th(nx, ny), can_see(nx, ny))

    ! Read the data
    open(newunit=fid, file='data/input_day08.txt', form='formatted', action='read') 
    readall: do j = 1, ny
        read(fid, '(a)', iostat=ierr) c1
        do i = 1, nx
            read(c1(i:i), '(I1)') th(i,j)
        end do
    end do readall
    close(fid)

    ! Part 1
    can_see = .true.
    do j = 2, ny-1
        do i = 2, nx-1
            is_visible = (&
                all(th( 1:(i-1),j) < th(i,j))  .or. &
                all(th((i+1):nx,j) < th(i,j)) .or. &
                all(th(i,1:(j-1)) < th(i,j))  .or. &
                all(th(i,(j+1):ny) < th(i,j)) )
            if(.not. is_visible) can_see(i,j) = .false.
        end do 
    end do
    print*, 'Number of visible trees = ', count(can_see)

    ! Part 2
    allocate(scene_score(nx, ny))
    scene_score = 0 ! Edge trees will have a score of zero

    do j = 2, ny-1
        do i = 2, nx-1
            s(1) = score(th(1:(i-1), j) , th(i,j), back=.true.) 
            s(2) = score(th((i+1):nx, j), th(i,j), back=.false.)
            s(3) = score(th(i, 1:(j-1)) , th(i,j), back=.true.) 
            s(4) = score(th(i, (j+1):ny), th(i,j), back=.false.)

            scene_score(i,j) = product(s)
        end do
    end do

    print*, 'Maximum scene score: ', maxval(scene_score)

    contains
        integer function score(ints, mytree, back)
            ! Score for part 2 in one direction
            integer, intent(in) :: ints(:), mytree
            logical, intent(in) :: back
            integer:: i
        
            score = 0
            if(back) then
                do i = size(ints), 1, -1
                    score = score + 1
                    if(ints(i) >= mytree) return
                end do
            else
                do i = 1, size(ints)
                    score = score + 1
                    if(ints(i) >= mytree) return
                end do
            end if

        end function
end program
