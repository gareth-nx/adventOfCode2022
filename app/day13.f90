module qsort_C_mod
    implicit none
    interface

        subroutine qsort_C(array, elem_count, elem_size, compare) bind(C,name="qsort")
          !! Call qsort from C.
          use iso_c_binding, only: c_ptr, c_size_t, c_funptr
          implicit none
          type(c_ptr), value       :: array
              !! When called this should be c_loc(array(1))
          integer(c_size_t), value :: elem_count
              !! When called this is int(size(array), c_size_t)
          integer(c_size_t), value :: elem_size
              !! When called this is int(storage_size(array(1))/8, c_size_t)
          type(c_funptr), value    :: compare
              !! When called this should be c_funloc(comparison_function) where
              !! comparison_function(array(i), array(j)) will return
              !! -1_c_int, 0_c_int, or 1_c_int, if array(i) is less than, equal,
              !! or greater than array(j) respectively.
        end subroutine qsort_C !standard C library qsort

    end interface
end module

module entries
    implicit none

    integer, parameter :: cl = 1024, non_integer_entry = -999
    logical :: verbose  = .FALSE.

    ! List entry is either an integer or another list
    type :: entry
        integer, allocatable :: i(:)
        type(entry), allocatable :: l(:)
        character(len=cl) :: c1 = ""
    end type

    contains

    recursive subroutine parse_list(c1, p1)
        character(len=cl), intent(in) :: c1
        type(entry), intent(inout) :: p1

        character(len=cl) :: c2
        integer :: ierr, entry_count, j, i, n, level, myint, end_index
        integer, allocatable :: levels(:)

        if(verbose) print*, 'PARSING ', trim(c1); flush(6)
        c2 = ""

        ! Count the list entries 
        entry_count = 0
        level = 0
        allocate(levels(len_trim(c1)))
        linescan: do j = 1, len_trim(c1)
            ! Parse one character at a time
            if(c1(j:j) == '[') level = level + 1
            levels(j) = level
            if(c1(j:j) == ']') level = level - 1

            if(level == 2 .and. c1(j:j) == '[') entry_count = entry_count + 1
            if(level == 1 .and. is_integer(c1(j:j))) then
                ! Some numbers take up 2 digits -- need to check for that.
                if(j == 1) then
                    entry_count = entry_count + 1
                else if(.not. is_integer(c1(j-1:j-1))) then
                    entry_count = entry_count + 1
                end if
            end if

            !if(verbose) print*, ' ', levels(j), c1(j:j), entry_count; flush(6)
        end do linescan
        if(verbose) print*, 'ENTRY COUNT: ', entry_count; flush(6)

        call delete_entry(p1)
        p1%c1 = c1
        !if(allocated(p1%i)) deallocate(p1%i)
        !if(allocated(p1%l)) deallocate(p1%l)
        allocate(p1%i(entry_count), p1%l(entry_count))

        if(entry_count > 0) then
        
            p1%i = non_integer_entry
            !stop

            ! Read the entries
            i = 1
            reader: do j = 2, len_trim(c1)-1

                if(levels(j) == 1 .and. is_integer(c1(j:j))) then
                    ! Deal with the fact that 10 has 2 digits

                    ! If the last j was an integer, we are on the 2nd digit of a 2digit number 
                    ! Skip it
                    if(j-1 >= 1) then
                        if(is_integer( c1((j-1):(j-1)) )) cycle reader
                    end if
                    if(len_trim(c1) > j) then
                        ! Read the next integer
                        if(is_integer( c1(j+1:j+1) )) then
                            ! The ten has 2 digits
                            read( c1(j:j+1), *, iostat=ierr) myint
                        else
                            ! Other numbers have 1 digit
                            read(c1(j:j), *, iostat=ierr) myint
                        end if
                    else
                        ! Other numbers have 1 digit
                        read(c1(j:j), *, iostat=ierr) myint
                    end if

                    if(ierr /= 0) stop 'READ ERROR'
                    !if(verbose) print*, 'read integer: ', myint, ' from ', trim(c1(j:))
                    p1%i(i) = myint

                else if(levels(j) == 1 .and. c1(j:j) == ',') then
                    ! Next entry
                    i = i + 1

                else if(levels(j) == 2 .and. c1(j:j) == '[') then
                    ! Next entry is a list that we need to parse
                    find_end: do n = j+1, len_trim(c1)
                        if(levels(n) == 2 .and. c1(n:n) == ']') then
                            end_index = n
                            exit find_end
                        end if
                    end do find_end
                    c2 = c1(j:end_index)
                    call parse_list(c2, p1%l(i))
                end if
            end do reader
        end if

        if(verbose) then
            print*, 'FINAL READ'
            print*, trim(c1)
            print*, ', p1%i: [', p1%i, ']'
            flush(6)
        end if

    end subroutine parse_list

    recursive subroutine delete_entry(p)
        type(entry), intent(inout) :: p
        integer :: i
        if(verbose) print*, 'DELETING p'; flush(6)
        if(allocated(p%i)) deallocate(p%i)
        if(allocated(p%l)) then
            if(size(p%l) > 0) then
                do i = 1, size(p%l)
                    call delete_entry(p%l(i))
                end do
            end if
            deallocate(p%l)
        end if
        p%c1 = ""
        if(verbose) print*, '....DELETED p'; flush(6)
    end subroutine

    logical function is_integer(c)
        character(len=1), intent(in) :: c
        is_integer = (index('0123456789', c) > 0)
    end function

    recursive subroutine order_check(p1, p2, res)
        ! Return -1 if p1 < p2
        ! Return +1 if p1 > p2
        ! Return 0  if p1 == p2 (should not happen in final comparisons)
        type(entry), intent(in) :: p1, p2
        integer, intent(out) :: res

        integer :: j, myorder
        type(entry), allocatable :: pl, pr

        if(verbose) then
            print*, 'ORDERING'
            print*, 'p1%i: ', p1%i
            print*, 'p2%i: ', p2%i
            flush(6)
        end if

        res = 0
        myorder = 0

        ! Entries with one item, used if we need to recurse
        allocate(pl, pr)

        if(size(p1%i) == 0) then
            ! Special case
            if(size(p2%i) > 0) then
                res = -1 ! p1 < p2
            else
                res = 0 ! both of size 0
            end if
        else if(size(p2%i) == 0) then
            ! Special case
            res = 1 ! p1 > p2
        else
            ! Regular case
            mainloop: do j = 1, size(p1%i)
                if(size(p2%i) < j) then
                    if(verbose) print*, 'quick exit'; flush(6)
                    res = 1 ! p1 > p2
                    exit mainloop 
                end if

                if(p1%i(j) /= non_integer_entry .and. p2%i(j) /= non_integer_entry) then

                    ! Both integers
                    ! If results are unequal we can determine the result
                    if(verbose) print*, 'comparing ints: ', p1%i(j), p2%i(j); flush(6)
                    if(p1%i(j) /= p2%i(j)) then
                        res = merge(-1, 1, p1%i(j) < p2%i(j))
                        exit mainloop 
                    end if

                else if(p1%i(j) == non_integer_entry .and. p2%i(j) == non_integer_entry) then
                    if(verbose) print*, 'comparing non-ints: ', p1%i(j), p2%i(j); flush(6)
                    ! Both lists
                    call order_check( p1%l(j), p2%l(j), myorder)
                    if(myorder /= 0) then
                        res = myorder
                        exit mainloop 
                    end if

                else if((p1%i(j) == non_integer_entry .and. p2%i(j) /= non_integer_entry) .or. &
                        (p1%i(j) /= non_integer_entry .and. p2%i(j) == non_integer_entry) ) then
                    if(verbose) print*, 'comparing mixed int / non-int: ', p1%i(j), p2%i(j); flush(6)
                    !print*, trim(p1%c1)
                    !print*, trim(p2%c1)
                    call delete_entry(pl)
                    call delete_entry(pr)
                    if(p1%i(j) == non_integer_entry) then
                        pl%i = p1%l(j)%i
                        pl%l = p1%l(j)%l
                        pr%i = [p2%i(j)]
                        if(size(pl%l) == 0) then
                            ! p1 < p2
                            res = -1
                            exit mainloop 
                        end if
                    else if(p2%i(j) == non_integer_entry) then
                        pl%i = [p1%i(j)]
                        !pl%l(1) = p1%l(j)
                        !pr%i(1) = p2%i(j)
                        !pr%l(1) = p2%l(j)
                        pr%i = p2%l(j)%i
                        pr%l = p2%l(j)%l
                        if(size(pr%l) == 0) then
                            ! p1 > p2
                            res = 1
                            exit mainloop
                        end if
                    end if

                    if(verbose) then
                        print*, 'pl: ', pl%i
                        print*, 'pr: ', pr%i
                    end if

                    call order_check(pl, pr, myorder)
                    if(myorder /= 0) then
                        res = myorder
                        exit mainloop 
                    end if
                end if
            end do mainloop
        end if

        if(res == 0) then
            if((size(p1%i) < size(p2%i))) then
                ! If we got here without finishing, then p1 < p2
                res = -1
            else
                ! Cannot resolve the comparison
                res = 0
            end if
        end if

        if(verbose) print*, 'res: ', res; flush(6)

        call delete_entry(pl)
        call delete_entry(pr)

    end subroutine

end module entries

program day13
    use entries
    use iso_c_binding
    use qsort_C_mod
    implicit none
    character(len=cl) :: c1, c2
    integer :: fid, ierr, counter=0, order, index_sum = 0
    type(entry) :: p1, p2
    type(entry), allocatable :: all_p(:)
    integer, allocatable :: counter_array(:)
    integer :: i, decoder_key, j

    allocate(all_p(0), counter_array(0))

    !open(newunit=fid, file='data/test_input_day13.txt', form='formatted', action='read')
    open(newunit=fid, file='data/input_day13.txt', form='formatted', action='read')

    reader: do
        read(fid, '(a)', iostat=ierr) c1
        read(fid, '(a)', iostat=ierr) c2
        counter = counter+1

        if(verbose) print*, 'INIT: ', trim(c1); flush(6)
        call parse_list(c1, p1)
        if(verbose) print*, 'INIT: ', trim(c2); flush(6)
        call parse_list(c2, p2)

        all_p = [ all_p, p1, p2 ]
        counter_array = [counter_array, 2*counter - 1, 2*counter]

        if(verbose) then
            print*, ' '
            print*, 'COMPARING'
            print*, trim(c1)
            print*, trim(c2)
            flush(6)
        end if
        call order_check(p1, p2, order)
        if(order == -1) index_sum = index_sum + counter
        if(verbose) then
            print*, 'COUNTER, ORDER'
            print*, counter, order; flush(6)
        end if
        read(fid, *, iostat=ierr)
        if(ierr /= 0) exit reader
    end do reader

    print*, 'SUM OF INDICES IN RIGHT ORDER (part 1): ', index_sum !5605

    ! Part 2
    c1 = '[[2]]'
    call parse_list(c1, p1)
    c2 = '[[6]]'
    call parse_list(c2, p2)
    all_p = [all_p, p1, p2]
    counter_array = [counter_array, maxval(counter_array) + 1, maxval(counter_array) + 2]

    call qsort_C(c_loc(counter_array(1)), size(counter_array, kind=c_size_t), &
            c_sizeof(counter_array(1)), &
            c_funloc(order_check_for_c) )

    decoder_key = 1
    do i = 1, size(all_p)
        j = counter_array(i)
        !print*, i, trim(all_p(j)%c1)
        if(all_p(j)%c1 == c1 .or. all_p(j)%c1 == c2) decoder_key = decoder_key * i
    end do
    print*, 'Decoder key: ', decoder_key ! 24969

    contains

    function order_check_for_c(i1ptr, i2ptr) result(sgn) bind(C)
        use iso_c_binding, only: c_ptr, c_int, c_f_pointer
        type(c_ptr), value, intent(in) :: i1ptr, i2ptr
        integer, pointer :: i1, i2
        integer(c_int) :: sgn

        call c_f_pointer(i1ptr, i1)
        call c_f_pointer(i2ptr, i2)

        call order_check(all_p(i1), all_p(i2), sgn)

    end function

end program
