module queue_int_mod_b
    ! Use the queue from flibs to make a queue of ints
    use iso_fortran_env, only: int32, int64
    implicit none
    integer, parameter :: ip = int64 !int32
#define QUEUE_STRUCT queue_ip_type
#define TYPE_QUEUE_DATA integer(ip)
#include "queues_flibs.inc"
end module queue_int_mod_b

program day11
    use queue_int_mod_b
    use stdlib_selection, only: select
    implicit none

    integer(ip) :: max_items = 200 ! Size of the queues in monkey(j)%items
    integer(ip), parameter :: cl = 1024, max_rounds(2) = [20, 10000]
    integer(ip) :: fid, ierr, nm, buf(100), i, j, n, rounds, monkey_business, worry_level_mod
    logical :: success
    integer(ip), allocatable :: inspections(:)

    type :: monkey_type
        type(queue_ip_type), pointer :: items
        integer(ip) :: op(3) = 0 ! new = dot_product(op, old**[2,1,0])
        integer(ip) :: test_divisor
        integer(ip) :: to_monkey_true
        integer(ip) :: to_monkey_false
        integer(ip) :: inspections = 0
    end type

    type(monkey_type), allocatable :: monkeys(:), monkeys2(:)
    type(monkey_type) :: new_monkey

    call initialise_monkeys(monkeys)

    ! Convert worry_level --> mod(worry_level, worry_level_mod)
    worry_level_mod = product(monkeys%test_divisor)

    do rounds = 1, max_rounds(1)
        do j = 1, size(monkeys)
            call inspect_and_throw(monkeys, j, divide_by_three=.TRUE., worry_level_mod=worry_level_mod)
        end do
    end do

    inspections = monkeys(:)%inspections
    n = size(inspections)
    call select(inspections, n - 1, j)
    monkey_business = product(inspections(n-1:)) ! 61503
    print*, 'Monkey business (part 1) = ', monkey_business

    ! Part 2
    call initialise_monkeys(monkeys2)

    do rounds = 1, max_rounds(2)
        do j = 1, size(monkeys2)
            call inspect_and_throw(monkeys2, j, divide_by_three=.FALSE., worry_level_mod=worry_level_mod)
        end do
    end do

    inspections = monkeys2(:)%inspections
    n = size(inspections)
    call select(inspections, n - 1, j)
    monkey_business = product(inspections(n-1:)) !  14081365540
    print*, 'Monkey business (part 2) = ', monkey_business

    contains 

    subroutine initialise_monkeys(monkeys)
        type(monkey_type), allocatable :: monkeys(:)
        character(len=cl) :: c0, c1, c2, c3, c4, c5

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
                end if

            else if(c0(1:6) == '  Test') then
                ! Get the divisor
                read(c0, *, iostat=ierr) c1, c2, c3, j
                monkeys(nm)%test_divisor = j

            else if(c0(1:6) == '    If') then
                ! Get the monkeys to throw to
                read(c0, *, iostat=ierr) c1, c2, c3, c4, c5, j
                if(c2(1:4) == 'true') then
                    monkeys(nm)%to_monkey_true = j
                else if(c2(1:5) == 'false') then
                    monkeys(nm)%to_monkey_false = j
                end if

            end if
        end do scan
    end subroutine

    subroutine inspect_and_throw(monkeys, nm, divide_by_three, worry_level_mod)
        type(monkey_type), intent(inout) :: monkeys(:)
        integer(ip), intent(in) :: nm ! Monkey's array index
        logical, intent(in) :: divide_by_three
        integer(ip), intent(in) :: worry_level_mod ! Control size of worry_level
        integer(ip) :: worry_level, tomonkey
        logical :: success

        do while(.not. queue_empty(monkeys(nm)%items))

            monkeys(nm)%inspections = monkeys(nm)%inspections + 1
            worry_level = queue_retrieve_data(monkeys(nm)%items)
            worry_level = dot_product(monkeys(nm)%op , worry_level**[2,1,0])
            if(divide_by_three) worry_level = worry_level / 3_ip
            worry_level = mod(worry_level, worry_level_mod)
            tomonkey = merge(monkeys(nm)%to_monkey_true + 1, monkeys(nm)%to_monkey_false + 1, &
                mod(worry_level, monkeys(nm)%test_divisor) == 0)
            call queue_append_data(monkeys(tomonkey)%items, worry_level, success)

        end do

        if(monkeys(nm)%items%end > max_items/2) then
            ! Reset the empty queue to avoid running out of index space. FIXME: Could just reset start/end inds
            call queue_destroy(monkeys(nm)%items)
            call queue_create(monkeys(nm)%items, int(max_items, int32))
        end if

    end subroutine

end program
