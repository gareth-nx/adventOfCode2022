module shortest_path_on_array_mod
    use kdtree2_precision_module, only: dp => kdkind
    use kdtree2_priority_queue_module, only: pq, pq_create, pq_insert, kdtree2_result, pq_extract_max, pq_delete
    implicit none

    ! A very large distance (in practice we cannot travel paths with this distance)
    real(dp), parameter :: bigdist = 999999.0_dp

    contains

    real(dp) function travel_effort(c1, c2)
        ! Distance from c1 to c2. 
        character(len=1), intent(in) :: c1, c2

        ! Use a very large number to represent 'travel not possible'
        travel_effort = merge(1.0_dp, bigdist, ichar(c2) <= ichar(c1) + 1)

    end function
   
    subroutine shortest_path_on_character_array2d(th, source_pt, target_pt)
        character(len=1), intent(in), target, contiguous :: th(:,:)
        integer, intent(in) :: source_pt(2), target_pt(2)

        ! Handy to move between linear and 2d indices
        character(len=1), pointer :: th_flat(:)

        ! To use pq, some procedures expect a pointer queue, some not
        type(pq), target :: priority_queue_raw
        type(pq), pointer :: priority_queue
        type(kdtree2_result), allocatable, target :: elems(:)
        type(kdtree2_result) :: dist_site

        integer :: i, j, idx, nx, ny, ind, target_ind, source_ind, site_ind
        real(dp) :: dis, inv_dis1, inv_dis2, dis2, site_dist

        integer, allocatable :: parent_ind_flat(:)
        logical, allocatable :: searched_flat(:)
        integer, allocatable :: path_inds(:)

        nx = size(th, 1); ny = size(th, 2)
        th_flat(1:size(th)) => th

        source_ind = source_pt(1) + nx*(source_pt(2) - 1)
        target_ind = target_pt(1) + nx*(target_pt(2) - 1)

        allocate(elems(size(th)), parent_ind_flat(size(th)), searched_flat(size(th)))
        parent_ind_flat = -1

        priority_queue_raw = pq_create(elems) 
        priority_queue => priority_queue_raw

        searched_flat = .false.
        ! The priority queue returns the maximum element. We want minimum distances.
        ! So use inverse distances.
        inv_dis2 = pq_insert(priority_queue, 1.0_dp/1e-12_dp, source_ind)

        !print*, 'pq heap: ', priority_queue%heap_size

        ! Main loop
        main: do while(priority_queue%heap_size > 0)
            !print*, 'indices in queue: ', priority_queue%elems(1:priority_queue%heap_size)%idx
            ! Find minimum distant point (maximum inverse distance) -- we search around this
            call pq_extract_max(priority_queue, dist_site)
            !print*, 'Searching site: ', th_flat(dist_site%idx), dist_site%idx, 1.0_dp/dist_site%dis, priority_queue%heap_size

            ! Possible the a site was added to the queue multiple times. But we only check once
            if(searched_flat(dist_site%idx)) cycle 

            site_ind = dist_site%idx
            site_dist = 1.0_dp/dist_site%dis
            searched_flat(site_ind) = .true.

            !do j = 1, ny
            !    print*, searched_flat(1 + (j-1)*nx :nx + (j-1)*nx)
            !end do

            ! Search the four neighbours of site_ind
            do j = -1, 1
                do i = -1, 1
                    ! Only 4 neighbours
                    if(abs(i) + abs(j) > 1) cycle
                    if(i == 0 .and. j == 0) cycle

                    ind = site_ind + i + j*nx
                    ! Bounds
                    if(ind <= 0 .or. ind > size(th_flat)) cycle

                    ! Avoid updating points we've already searched around -- as we could have got here more cheaply before
                    if(searched_flat(ind)) cycle

                    dis2 = travel_effort(th_flat(site_ind), th_flat(ind))
                    if(dis2 == bigdist) cycle

                    dis = site_dist + dis2

                    ! Inverse distances in priority queue
                    inv_dis2 = pq_insert(priority_queue, 1.0_dp/dis, ind)

                    parent_ind_flat(ind) = site_ind

                    !print*, '    ..', ind, dis, searched_flat(ind)
                    if(ind == target_ind) then
                       !print*, 'Hit target site, ', ind, dis, th_flat(ind)
                       exit main
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

        print*, 'PATH: ', path_inds
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
    character(len=1), pointer :: th_flat(:)

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
    th_flat(1:(nx*ny)) => th

    start_ind = findloc(th, 'S')
    end_ind = findloc(th, 'E')
    !print*, 'Start index: ', start_ind , ', linear index: ', findloc(th_flat, 'S')
    !print*, 'End index: ', end_ind, ', linear index: ', findloc(th_flat, 'E')
    th(start_ind(1), start_ind(2)) = 'a' ! Replace start by 'a'
    th(end_ind(1), end_ind(2)) = 'z' ! Replace end by 'z'

    call shortest_path_on_character_array2d(th, start_ind, end_ind)

end program
