program day04
    implicit none
    integer :: fid, i, r1(2), r2(2), overlap_count1=0, overlap_count2=0
    character(len=1024) :: c1

    open(newunit=fid, file='data/input_day04.txt', form='formatted', action='read')

    do
        read(fid, "(a)", end=20) c1
        do i = 1, len_trim(c1)
            if(c1(i:i) == "-") c1(i:i) = ","
        end do
        read(c1, *) r1(1), r1(2), r2(1), r2(2)
        ! Part 1
        if((r1(1) <= r2(1) .and. r1(2) >= r2(2)) .or. &
           (r2(1) <= r1(1) .and. r2(2) >= r1(2)) ) then
           overlap_count1 = overlap_count1 + 1
         endif
        ! Part 2
        if((r1(1) <= r2(1) .and. r1(2) >= r2(1)) .or. &
           (r1(1) <= r2(2) .and. r1(2) >= r2(2)) .or. &
           (r2(1) <= r1(1) .and. r2(2) >= r1(1)) .or. &
           (r2(1) <= r1(2) .and. r2(2) >= r1(2))) then
           !print*, trim(c1)
           overlap_count2 = overlap_count2 + 1
         endif
    end do
    20 close(fid)

    print*, '# Overlaps (part 1): ', overlap_count1
    print*, '# Overlaps (part 2): ', overlap_count2
end program
