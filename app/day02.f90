program day02
    implicit none
    integer :: fid, my_score, i
    character(len=4), allocatable :: x(:)

    ! Try reading the whole file at once, like in R
    open(newunit=fid, file='data/input_day02.txt', form='formatted', action='read')
    allocate(x(1)) 
    i = 0
    do
        i = i+1
        if(i > size(x)) x = [x, x] ! Grow array if needed
        read(fid, '(A)', end=20) x(i)
    end do
    20 close(fid)
    x = x(1:(i-1)) ! Reduce down to the size with real data

    print*, 'My score (part 1) is: ', sum(round_score(x))
    print*, 'My score (part 2) is: ', sum(outcome_score(x))

    contains 
    elemental integer function round_score(x)
        character(len=4), intent(in) :: x
        round_score = 0
        if(x == 'A X') round_score = 3+1
        if(x == 'A Y') round_score = 6+2
        if(x == 'A Z') round_score = 0+3
        if(x == 'B X') round_score = 0+1
        if(x == 'B Y') round_score = 3+2
        if(x == 'B Z') round_score = 6+3
        if(x == 'C X') round_score = 6+1
        if(x == 'C Y') round_score = 0+2
        if(x == 'C Z') round_score = 3+3
    end function

    elemental integer function outcome_score(x)
        character(len=4), intent(in) :: x
        outcome_score = 0
        if(x == 'A X') outcome_score = 0+3
        if(x == 'A Y') outcome_score = 3+1
        if(x == 'A Z') outcome_score = 6+2
        if(x == 'B X') outcome_score = 0+1
        if(x == 'B Y') outcome_score = 3+2
        if(x == 'B Z') outcome_score = 6+3
        if(x == 'C X') outcome_score = 0+2
        if(x == 'C Y') outcome_score = 3+3
        if(x == 'C Z') outcome_score = 6+1
    end function

end program
