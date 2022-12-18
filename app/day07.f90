module filesystem
    implicit none

    private
    public :: cl, dir

    integer, parameter :: cl=1024
    integer :: i

    type dir
        character(len=cl) :: dname = "" ! Name of the directory (basename)
        integer :: total_dir_size = -1
        character(len=cl) :: long_dname = "" ! Full name of directory

        type(dir), pointer :: parent_dir => NULL()

        character(len=cl), allocatable :: file_names(:) 
        integer, allocatable :: file_sizes(:)

        character(len=cl), allocatable :: subdir_names(:)
        integer, allocatable :: subdir_sizes(:)
        type(dir), allocatable :: subdirs(:)

        contains
        procedure :: print => print_dir
        procedure :: dir_size => get_dir_size
        procedure :: walk => walk_dirs
    end type dir

    interface 
        subroutine recursive_dir_sub(current_dir)
            import dir
            type(dir), intent(inout) :: current_dir
        end subroutine
    end interface

    contains

    subroutine print_dir(current_dir, recurse)
        ! Not needed but handy to understand what we're doing
        class(dir), intent(in) :: current_dir    
        logical, optional, intent(in) :: recurse
        integer :: j

        write(*,*) '###:   ', trim(current_dir%long_dname)
        do j = 1, size(current_dir%subdirs)
            write(*, '(A8,i3,A20)') 'subdir ', j, trim(current_dir%subdir_names(j))
        end do
        do j = 1, size(current_dir%file_names)
            write(*, '(A8,i3,A20)') ' file ', j, trim(current_dir%file_names(j))
        end do

        if(present(recurse)) then
            if(recurse) then
                do j = 1, size(current_dir%subdirs)
                    call current_dir%subdirs(j)%print(recurse=recurse)
                end do
            end if
        end if

    end subroutine

    subroutine get_dir_size(current_dir, recurse)
        ! Could be reduced to a special case of current_dir%walk
        class(dir), intent(inout) :: current_dir    
        logical, optional, intent(in) :: recurse
        integer :: j, dir_size

        dir_size = sum(current_dir%file_sizes)

        if(present(recurse)) then
            if(recurse) then
                do j = 1, size(current_dir%subdirs)
                    call current_dir%subdirs(j)%dir_size(recurse = recurse)
                    current_dir%subdir_sizes(j) = current_dir%subdirs(j)%total_dir_size
                end do

                dir_size = dir_size + sum(current_dir%subdir_sizes)
            end if
        end if

        current_dir%total_dir_size = dir_size

    end subroutine        

    subroutine walk_dirs(current_dir, my_sub)
        ! Walk the directory structure and do something user defined. This is important
        class(dir), intent(inout) :: current_dir
        procedure(recursive_dir_sub) :: my_sub
        integer :: j

        do j = 1, size(current_dir%subdirs)
            call current_dir%subdirs(j)%walk(my_sub)
        end do

        call my_sub(current_dir)
    end subroutine
end module

program day07
    use filesystem
    implicit none
    integer :: fid, ierr, file_size, pind, j
    character(len=cl) :: line, dname, fname

    type(dir) :: empty_dir
    type(dir), target :: base_dir
    type(dir), pointer :: current_dir => NULL(), parent_dir=> NULL()

    integer, allocatable :: dir_size(:)
    character(len=cl), allocatable :: long_dirname(:)
    ! Part 2
    integer, parameter :: total_filesystem_space = 70000000, required_space = 30000000
    integer :: existing_space, extra_space_needed

    open(newunit=fid, file='data/input_day07.txt', form='formatted', action='read')

    ! Base directory holding only "/" as the sub directory
    base_dir%dname = ""
    base_dir%long_dname = ""
    base_dir%parent_dir => base_dir
    allocate(base_dir%file_names(0), base_dir%file_sizes(0))
    base_dir%subdir_names = ['/']
    base_dir%subdir_sizes = [ -1 ]
    allocate(base_dir%subdirs(1))

    current_dir => base_dir
    parent_dir => base_dir

    build_dir_structure: do

        read(fid, '(A)', iostat=ierr) line
        if(ierr /= 0) exit build_dir_structure

        if(line(1:7) == '$ cd ..') then
            ! Move up one directory
            current_dir => current_dir%parent_dir
            parent_dir => current_dir%parent_dir

        else if(line(1:4) == '$ cd') then
            ! Move inside a subdirectory

            parent_dir => current_dir
            dname = adjustl(line(5:))
            pind = findloc(parent_dir%subdir_names, dname, dim=1)

            current_dir => parent_dir%subdirs(pind)

            current_dir%dname = dname
            current_dir%long_dname = trim(parent_dir%long_dname) // '/' // trim(dname)
            current_dir%parent_dir => parent_dir

            allocate(current_dir%file_names(0), current_dir%file_sizes(0), &
                current_dir%subdir_names(0), current_dir%subdir_sizes(0), &
                current_dir%subdirs(0))

        else if(line(1:4) == '$ ls') then
            ! List directory contents

        else if(line(1:3) == 'dir') then
            ! dir subdirectory
            dname = adjustl(line(4:))
            current_dir%subdir_names = [character(len=cl):: current_dir%subdir_names, dname]
            current_dir%subdir_sizes = [current_dir%subdir_sizes, -1]
            current_dir%subdirs = [current_dir%subdirs, empty_dir]

        else
            ! file_size, file_name
            read(line, *) file_size, fname
            current_dir%file_names = [character(len=cl):: current_dir%file_names, fname]
            current_dir%file_sizes = [current_dir%file_sizes, file_size]

        end if

    end do build_dir_structure

    current_dir => base_dir%subdirs(1)
    call current_dir%print(recurse=.TRUE.)
    ! Populate the "total_dir_size" field for each directory, counting the directories beneath it
    call current_dir%dir_size(recurse=.TRUE.)

    print*, 'Total size of directory structure: ', current_dir%total_dir_size

    !
    ! Part 1
    !
    
    print*, ''
    print*, 'Directories with size <= 100000'
    allocate(dir_size(0), long_dirname(0))
    current_dir => base_dir%subdirs(1)
    call current_dir%walk(append_dir_size_if_LE_100000)
    do j = 1, size(long_dirname)
        print*, dir_size(j), trim(long_dirname(j))
    end do
    print*, 'Sum of sizes: ', sum(dir_size)

    !
    ! Part 2
    !
    existing_space = total_filesystem_space - current_dir%total_dir_size
    extra_space_needed = required_space - existing_space
    print*, 'extra_space_needed: ', extra_space_needed
    ! Store all dir_sizes
    deallocate(dir_size, long_dirname)
    allocate(dir_size(0), long_dirname(0))
    current_dir => base_dir%subdirs(1)
    call current_dir%walk(append_dir_size)
    j = minloc(dir_size, mask=dir_size > extra_space_needed, dim=1)
    print*, 'We can delete a directory of size ', dir_size(j), ' with name :', trim(long_dirname(j))


    contains

        subroutine append_dir_size_if_LE_100000(current_dir)
            ! For part 1
            type(dir), intent(inout) :: current_dir
    
            if(current_dir%total_dir_size <= 100000) then
                ! Append to a global list of dirs
                dir_size = [dir_size, current_dir%total_dir_size]
                long_dirname = [character(len=cl):: long_dirname, current_dir%long_dname]
            end if
        end subroutine

        subroutine append_dir_size(current_dir)
            ! For part 2
            type(dir), intent(inout) :: current_dir
    
            ! Append to a global list of dirs
            dir_size = [dir_size, current_dir%total_dir_size]
            long_dirname = [character(len=cl):: long_dirname, current_dir%long_dname]
        end subroutine

end program
