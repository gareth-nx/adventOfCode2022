! Data type inside a heap
module heap_data
    implicit none
    integer, parameter :: dp = selected_real_kind(12)
    type :: ij_dist
        integer :: i, j
        real(dp) :: dis=-1.0_dp
    end type
    contains
    logical function smaller_distance(p1, p2)
        type(ij_dist), intent(in) :: p1, p2
        smaller_distance = p1%dis < p2%dis
    end function
end module heap_data

! Create the heap
module heap_point_mod
    use heap_data, only: dp, node_data_type=>ij_dist, default_priority_function=>smaller_distance
    implicit none
#include "mheap_template.inc"    
end module heap_point_mod

program day12
    use heap_data, only: dp, ij_dist
    use heap_point_mod, only: heap_type => theap
    implicit none
    integer, parameter :: cl = 2048 
    integer :: fid, nx=0, ny, ierr, i, j, start_ind(2), end_ind(2)
    character(len=cl) :: c1
    character(len=1), allocatable, target :: th(:,:)

    !open(newunit=fid, file='data/test_input_day12.txt', form='formatted', action='read')
    open(newunit=fid, file='data/input_day12.txt', form='formatted', action='read')

    ! Get nx/ny in array
    readonce: do
        read(fid, '(a)', iostat=ierr) c1
        if(ierr /= 0) exit readonce
        ny = ny+1
        nx = len_trim(c1)
    end do readonce
    rewind(fid)

    print*, 'nx: ', nx, ', ny: ', ny
    allocate(th(nx, ny))

    ! Read the data
    readall: do j = 1, ny
        read(fid, '(a)', iostat=ierr) c1
        do i = 1, nx
            read(c1(i:i), '(A1)') th(i,j)
        end do
    end do readall
    close(fid)

    !do j = 1, ny
    !    print*, th(:,j)
    !end do

    start_ind = findloc(th, 'S')
    end_ind = findloc(th, 'E')
    th(start_ind(1), start_ind(2)) = 'a' ! Replace start by 'a'
    th(end_ind(1), end_ind(2)) = 'z' ! Replace end by 'z'

    print*, 'Part 1'
    call shortest_path_on_character_array2d(th, start_ind, target_pt = end_ind, reverse=.FALSE.)

    print*, 'Part 2'
    call shortest_path_on_character_array2d(th, end_ind, stop_at_letter='a', reverse=.TRUE.)

    contains

    subroutine shortest_path_on_character_array2d(th, source_pt, target_pt, stop_at_letter, reverse)
        character(len=1), intent(in) :: th(:,:)
        integer, intent(in) :: source_pt(2)
        integer, intent(in), optional :: target_pt(2)
        character(len=1), intent(in), optional :: stop_at_letter
        logical, optional, intent(in) :: reverse

        type(heap_type) :: heap
        type(ij_dist) :: site
        integer :: i, j, ip, jp, nx, ny, target_ij(2)
        real(dp) :: dis
        logical :: rev
        real(dp), parameter :: bigdist = HUGE(1.0_dp) ! Denote infinite distance
        real(dp), allocatable :: dist(:,:)

        nx = size(th, 1); ny = size(th, 2); target_ij = -1; rev = .false.
        if(present(target_pt)) target_ij = target_pt
        if(present(reverse)) rev = reverse

        allocate(dist(nx, ny))
        dist = bigdist

        ! Start at source point
        call heap%init(1000)
        call heap%insert(ij_dist(source_pt(1), source_pt(2), 0.0_dp))

        ! Main loop
        main: do while(heap%size() > 0)

            call heap%pop(site)
            if(site%dis >= dist(site%i, site%j)) cycle
            dist(site%i, site%j) = site%dis

            ! Search four site neighbours
            do j = -1, 1
                do i = -1, 1
                    ! Skip self and diagonal neighbours
                    if((abs(i) + abs(j) > 1) .or. (i == 0 .and. j == 0)) cycle
                    ip = site%i + i; jp = site%j + j
                    if(ip < 1 .or. ip > nx .or. jp < 1 .or. jp > ny) cycle

                    if(rev) then
                        dis = merge(1.0_dp + site%dis, bigdist, ichar(th(ip, jp)) >= ichar(th(site%i, site%j)) - 1)
                    else
                        dis = merge(1.0_dp + site%dis, bigdist, ichar(th(ip, jp)) <= ichar(th(site%i, site%j)) + 1)
                    end if
                    if(dis == bigdist) cycle
                    if(dis >= dist(ip, jp)) cycle

                    call heap%insert(ij_dist(ip, jp, dis))

                    if(all([ip, jp] == target_ij)) exit main
                    if(present(stop_at_letter)) then
                        if(th(ip, jp) == stop_at_letter) exit main
                    end if
                end do
            end do
        end do main

        print*, '# Steps: ', nint(dis)

    end subroutine

end program
