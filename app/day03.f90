program day03
    implicit none
    integer, parameter :: clen = 1024
    character(len=clen) :: x, comp1, comp2, three_elves(3)
    integer :: fid, i, score_sum, score_sum2

    score_sum = 0
    score_sum2 = 0
    open(newunit=fid, file='data/input_day03.txt', form='formatted', action='read')
    i = 1
    do
        read(fid, *, end=20) x
        ! Part 1
        comp1 = x(1:(len_trim(x)/2))
        comp2 = x((len_trim(x)/2 + 1):len_trim(x))
        score_sum = score_sum + matching_item_score(comp1, comp2)
        ! Part 2
        three_elves(i) = x
        if(i == 3) then
            score_sum2 = score_sum2 + group_score(three_elves(1), three_elves(2), three_elves(3)) 
        end if
        i = merge(1, i+1, i==3)
    end do
    20 close(fid)
    print*, 'Score sum (part 1): ', score_sum
    print*, 'Score sum (part 2): ', score_sum2

    contains
        integer function matching_item_score(c1, c2)
            character(len=clen), intent(in) :: c1, c2
            integer :: j
            character(len=1) :: matching_item

            do j = 1, len_trim(c1)
                if(index(c2, c1(j:j)) > 0) then
                    matching_item = c1(j:j)
                end if
            end do

            ! In the ascii character set, lower case numbers are HIGHER than upper case numbers
            if(ichar(matching_item) >= ichar('a')) then
                ! Lowercase, scores 1-26
                matching_item_score = 1 + ichar(matching_item) - ichar('a')
            else
                ! Uppercase, scores 27 - 52
                matching_item_score = 26 + 1 + ichar(matching_item) - ichar('A')
            end if

        end function
        
        integer function group_score(c1, c2, c3)
            character(len=clen), intent(in) :: c1, c2, c3
            integer :: j
            character(len=1) :: matching_item

            do j = 1, len_trim(c1)
                if(index(c2, c1(j:j)) > 0 .and. index(c3, c1(j:j)) > 0) then
                    matching_item = c1(j:j)
                end if
            end do

            ! In the ascii character set, lower case numbers are HIGHER than upper case numbers
            if(ichar(matching_item) >= ichar('a')) then
                ! Lowercase, scores 1-26
                group_score = 1 + ichar(matching_item) - ichar('a')
            else
                ! Uppercase, scores 27 - 52
                group_score = 26 + 1 + ichar(matching_item) - ichar('A')
            end if

        end function

end program
