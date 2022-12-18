program day06
    implicit none
    integer :: fid, ioerr, counter=0
    character(len=1) :: c1, last_4(4), last_14(14)
    logical :: part1_passed = .FALSE., part2_passed = .FALSE.

    open(newunit=fid, file='data/input_day06.txt', form='formatted', action='read', access='sequential')

    reader: do
        counter=counter+1
        read(fid, '(A1)', advance='no', iostat=ioerr) c1
        if(ioerr /= 0) exit reader

        if(.not. part1_passed) then
            ! Part 1 
            last_4(2:) = last_4(:3)
            last_4(1) = c1

            if(counter > 3 .and. unique_char(last_4)) then
                print*, 'Part 1 position is: ', counter
                part1_passed = .TRUE.
            end if
        end if

        if(.not. part2_passed) then
            ! Part 2
            last_14(2:) = last_14(:13)
            last_14(1) = c1
            if(counter > 13 .and. unique_char(last_14)) then
                print*, 'Part 2 position is: ', counter
                part2_passed = .TRUE.
            end if
        end if
    end do reader
    close(fid)

    contains
        logical function unique_char(c)
            character(len=1), intent(in) :: c(:)
            integer :: i
            unique_char = .TRUE.
            do i = 2, size(c)
                if(any(c(1:(i-1)) == c(i))) then
                    unique_char = .FALSE.
                    return
                end if
            end do
        end function

end program
