module shortest_path_on_array_mod
    use kdtree2_precision_module, only: dp => kdkind
    use kdtree2_priority_queue_module, only: pq, pq_create, pq_insert, kdtree2_result, pq_extract_max, pq_delete
    implicit none

    ! A very large distance (in practice we cannot travel paths with this distance)
    real(dp), parameter :: bigdist = 999999.0_dp

    contains
   
    subroutine shortest_path_on_character_array2d(th, source_pt, target_pt, stop_at_letter, reverse)
        character(len=1), intent(in), target, contiguous :: th(:,:)
        integer, intent(in) :: source_pt(2)
        integer, intent(in), optional :: target_pt(2)
        character(len=1), intent(in), optional :: stop_at_letter
        logical, optional, intent(in) :: reverse

        ! To use pq, some procedures expect a pointer queue, some not
        type(pq), target :: priority_queue_raw
        type(pq), pointer :: priority_queue
        type(kdtree2_result), allocatable, target :: elems(:)
        type(kdtree2_result) :: dist_site
        integer :: i, j, idx, nx, ny, ind, target_ind, source_ind, site_ind
        real(dp) :: inv_dis, dis, site_dist
        logical :: rev
        integer, allocatable :: parent_ind_flat(:), path_inds(:)
        logical, allocatable :: searched_flat(:)
        character(len=1), pointer :: th_flat(:) ! Handy to move between linear and 2d indices

        nx = size(th, 1); ny = size(th, 2)
        source_ind = source_pt(1) + nx*(source_pt(2) - 1)
        th_flat(1:size(th)) => th

        target_ind = -1
        if(present(target_pt)) target_ind = target_pt(1) + nx*(target_pt(2) - 1)

        rev = .false.
        if(present(reverse)) rev = reverse

        allocate(elems(size(th)), parent_ind_flat(size(th)), searched_flat(size(th)))
        parent_ind_flat = -1

        priority_queue_raw = pq_create(elems) 
        priority_queue => priority_queue_raw

        searched_flat = .false.
        ! The priority queue returns the maximum element. So priority --> inverse distance
        inv_dis = pq_insert(priority_queue, 1.0_dp/1e-12_dp, source_ind)

        ! Main loop
        main: do while(priority_queue%heap_size > 0)
            ! Find minimum distant point (maximum inverse distance) -- we search around this
            call pq_extract_max(priority_queue, dist_site)
            site_ind = dist_site%idx
            site_dist = 1.0_dp/dist_site%dis
            ! Possible the a site was added to the queue multiple times. But we only check once
            if(searched_flat(site_ind)) cycle 
            searched_flat(site_ind) = .true.

            ! Search the four neighbours of site_ind
            do j = -1, 1
                do i = -1, 1
                    ! Only 4 neighbours
                    if(abs(i) + abs(j) > 1) cycle
                    if(i == 0 .and. j == 0) cycle

                    ind = site_ind + i + j*nx
                    if(ind <= 0 .or. ind > size(th_flat)) cycle

                    ! Avoid updating points we've already searched around -- as we could have got here more cheaply before
                    if(searched_flat(ind)) cycle

                    if(rev) then
                        dis = merge(1.0_dp, bigdist, ichar(th_flat(ind)) >= ichar(th_flat(site_ind)) - 1)
                    else
                        dis = merge(1.0_dp, bigdist, ichar(th_flat(ind)) <= ichar(th_flat(site_ind)) + 1)
                    end if
                    if(dis == bigdist) cycle

                    ! Inverse distances in priority queue
                    inv_dis = pq_insert(priority_queue, 1.0_dp/(site_dist + dis), ind)

                    parent_ind_flat(ind) = site_ind

                    if(ind == target_ind) exit main
                    if(present(stop_at_letter)) then
                        if(th_flat(ind) == stop_at_letter) exit main
                    end if
                end do
            end do
        end do main

        ! Walk back along parent ind
        path_inds = [ind]
        do while(ind /= -1)
            ind = parent_ind_flat(ind)
            path_inds = [path_inds, ind]
        end do
        path_inds = path_inds(size(path_inds):1:-1)
        print*, '# Steps: ', size(path_inds)-2

    end subroutine

end module shortest_path_on_array_mod


program day12
    use shortest_path_on_array_mod
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

end program
