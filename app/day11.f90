module queue_int_mod
    ! Use the queue from flibs to make a queue of int32's
    ! There are some shortcomings in this at present, discussed below
    use iso_fortran_env, only: int32, int64
    implicit none
    integer, parameter :: ip = int64 !int32

#define QUEUE_STRUCT queue_ip_type
#define TYPE_QUEUE_DATA integer(ip)
#include "queues_flibs.inc"

end module queue_int_mod

program day11
    use queue_int_mod
    use stdlib_selection, only: select
    implicit none

    ! Size of the queues in monkey(j)%items
    integer(ip) :: max_items = 200 
    ! Note that the flibs queue has a tendency to run out of space once we have added max_items to it, even if
    ! we simultaneously removed most of those items. To prevent that, below I call queue_destroy/queue_create if
    ! the end index is > max_items/2

    integer(ip), parameter :: cl = 1024, max_rounds(2) = [20, 10000]
    integer(ip) :: fid, ierr, nm, buf(100), i, j, n, rounds, monkey_business, worry_level_mod
    character(len=cl) :: c0, c1, c2, c3, c4, c5
    logical :: success
    integer(ip), allocatable :: inspections(:)

    type :: monkey_type
        type(queue_ip_type), pointer :: items
        integer(ip) :: op(3) = 0 ! new = dot_product(op, old**[2,1,0])
        integer(ip) :: test_divisor
        ! Monkeys they send items to (depending on whether test is true/false)
        integer(ip) :: to_monkey_true
        integer(ip) :: to_monkey_false
        integer(ip) :: inspections = 0
    end type

    type(monkey_type), allocatable :: monkeys(:), monkeys2(:)
    type(monkey_type) :: new_monkey

    print*, '!' 
    print*, '! Part 1'
    print*, '!'

    call initialise_monkeys(monkeys)
    n = total_items_count(monkeys)

    ! Convert worry_level --> mod(worry_level, worry_level_mod)
    ! This prevents overflow without changing the results of the divisibility tests
    worry_level_mod = product(monkeys%test_divisor)

    do rounds = 1, max_rounds(1)
        do j = 1, size(monkeys)
            call inspect_and_throw(monkeys, j, divide_by_three=.TRUE., worry_level_mod=worry_level_mod)
        end do
        !print*, 'After step ', rounds
        if(total_items_count(monkeys) /= n) then
            print*, 'Total items count has changed', total_items_count(monkeys), n
            call print_monkeys(monkeys)
            stop
        end if
        !call print_monkeys(monkeys)
    end do

    inspections = monkeys(:)%inspections
    !print*, inspections
    n = size(inspections)
    call select(inspections, n - 1, j)
    !print*, inspections
    monkey_business = product(inspections(n-1:)) ! 61503
    print*, 'Monkey business (part 1) = ', monkey_business

    print*, '!' 
    print*, '! Part 2'
    print*, '!'

    call initialise_monkeys(monkeys2)
    n = total_items_count(monkeys2)
    !call print_monkeys(monkeys)

    do rounds = 1, max_rounds(2)
        !print*, 'Round: ', rounds
        do j = 1, size(monkeys2)
            call inspect_and_throw(monkeys2, j, divide_by_three=.FALSE., worry_level_mod=worry_level_mod)
        end do
        !print*, 'After step ', rounds
        !print*, monkeys2%inspections
        !if(rounds == 38) call print_monkeys(monkeys2)
        !if(rounds == 39) call print_monkeys(monkeys2)
        if(total_items_count(monkeys2) /= n) then
            print*, 'Total items count has changed'
            stop
        end if
    end do
    !call print_monkeys(monkeys2)

    inspections = monkeys2(:)%inspections
    !print*, inspections
    n = size(inspections)
    call select(inspections, n - 1, j)
    !print*, inspections
    monkey_business = product(inspections(n-1:)) !  14081365540
    print*, 'Monkey business (part 2) = ', monkey_business

    contains 

    subroutine initialise_monkeys(monkeys)
        type(monkey_type), allocatable :: monkeys(:)

        if(allocated(monkeys)) deallocate(monkeys)

        allocate(monkeys(0))
        nm = 0

        open(newunit=fid, file='data/input_day11.txt', form='formatted', action='read')
        !open(newunit=fid, file='data/test_input_day11.txt', form='formatted', action='read')

        scan: do 
            read(fid, '(a)', iostat=ierr) c0
            if(ierr /= 0) exit scan
            if(c0(1:6) == 'Monkey') then
                ! Make space for a monkey
                nm = nm + 1_ip
                monkeys = [monkeys, new_monkey]

            else if(c0(1:6) == "  Star") then
                ! Get the starting items

                buf=0_ip
                read(c0, *, iostat=ierr) c1, c2, buf
                call queue_create(monkeys(nm)%items, int(max_items, int32))
                do j = 1, count(buf>0)
                    call queue_append_data(monkeys(nm)%items, buf(j), success)
                end do

                print*, 'monkey(', nm, ')%items(1) : ', queue_start_data(monkeys(nm)%items)

            else if(c0(1:6) == "  Oper") then
                ! Get the operation

                c1 = c0(24:)
                if(c1(1:2) == '+ ') then
                    ! Addition
                    read(c1, *, iostat=ierr) c2, j
                    monkeys(nm)%op(3) = j
                    monkeys(nm)%op(2) = 1_ip
                else if(c1(1:5) == '* old') then
                    ! Quadratic term
                    monkeys(nm)%op(1) = 1_ip
                else if(c1(1:2) == '* ') then
                    ! Constant multiple
                    read(c1, *, iostat=ierr) c2, j
                    monkeys(nm)%op(2) = j
                else
                    stop 'parsing error'
                end if

                print*, 'monkey(', nm, ')%op : ', monkeys(nm)%op

            else if(c0(1:6) == '  Test') then
                ! Get the divisor
                read(c0, *, iostat=ierr) c1, c2, c3, j
                monkeys(nm)%test_divisor = j

                print*, 'monkey(', nm, ')%test_divisor : ', monkeys(nm)%test_divisor

            else if(c0(1:6) == '    If') then
                ! Get the monkeys to throw to
                read(c0, *, iostat=ierr) c1, c2, c3, c4, c5, j
                if(c2(1:4) == 'true') then
                    monkeys(nm)%to_monkey_true = j
                    print*, 'monkey(', nm, ')%to_monkey_true : ', j
                else if(c2(1:5) == 'false') then
                    monkeys(nm)%to_monkey_false = j
                    print*, 'monkey(', nm, ')%to_monkey_false : ', j
                end if

            end if
        end do scan
    end subroutine

    subroutine inspect_and_throw(monkeys, nm, divide_by_three, worry_level_mod)
        type(monkey_type), intent(inout) :: monkeys(:)
        integer(ip), intent(in) :: nm ! Monkey's array index
        logical, intent(in) :: divide_by_three
        integer(ip), intent(in) :: worry_level_mod

        integer(ip) :: worry_level, tomonkey
        logical :: success

        do while(.not. queue_empty(monkeys(nm)%items))

            monkeys(nm)%inspections = monkeys(nm)%inspections + 1

            worry_level = queue_retrieve_data(monkeys(nm)%items)
            !print*, nm, 'worry level: ', worry_level
            worry_level = dot_product(monkeys(nm)%op , worry_level**[2,1,0])
            !print*, nm, 'worry level op: ', worry_level
            if(divide_by_three) worry_level = worry_level / 3_ip
            !print*, nm, 'worry level / 3: ', worry_level

            ! Control size of worry_level
            worry_level = mod(worry_level, worry_level_mod)

            tomonkey = merge(monkeys(nm)%to_monkey_true + 1, monkeys(nm)%to_monkey_false + 1, &
                mod(worry_level, monkeys(nm)%test_divisor) == 0)
            !print*, nm, 'to monkey: ', tomonkey

            call queue_append_data(monkeys(tomonkey)%items, worry_level, success)

            if(.not. success) then
                print*, 'ERROR IN QUEUE_APPEND_DATA FOR MONKEY(', nm, ')'
                call print_monkeys(monkeys)
                stop
            end if

        end do

        if(.not. queue_empty(monkeys(nm)%items)) then
            print*, 'Monkey(', nm, ') is not empty'
            stop
        end if

        if(monkeys(nm)%items%end > max_items/2) then
            ! Reset the queue to avoid running out of index space. With this queue,
            ! indices are not moved back to the start, even when the queue empties.
            call queue_destroy(monkeys(nm)%items)
            call queue_create(monkeys(nm)%items, int(max_items, int32))
        end if

    end subroutine

    subroutine print_monkeys(monkeys)
        ! For debugging
        type(monkey_type), intent(in) :: monkeys(:)
        integer(ip) :: i, j, k

        do k = 1, size(monkeys)
            i = monkeys(k)%items%start
            j = monkeys(k)%items%end
            if(i <= j) then
                print*, 'Monkeys( ', k, ')%items = ', monkeys(k)%items%data(i:j)
                print*, '    inspections: ', monkeys(k)%inspections
            else
                print*, 'Monkeys( ', k, ')%items = { empty }'
                print*, '    inspections: ', monkeys(k)%inspections
            end if
        end do
    end subroutine

    integer function total_items_count(monkeys)
        ! For debugging
        type(monkey_type), intent(in) :: monkeys(:)
        integer(ip) :: j
           
        total_items_count = 0 
        do j = 1, size(monkeys)
            total_items_count = total_items_count + max(0, monkeys(j)%items%end - monkeys(j)%items%start + 1)     
        end do
        
    end function


end program
