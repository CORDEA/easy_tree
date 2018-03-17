module formatter
    use element
    implicit none
    character(1), parameter :: root = "."
    character(3), parameter :: branch = "├"
    character(3), parameter :: branch_end = "└"
    character(3), parameter :: branch_link = "─"
contains
    function format(lines)
        type(child_element), allocatable, dimension(:) :: lines
        character(:), allocatable :: format, row
        integer :: i, j
        format = root
        do i = 1, size(lines)
            row = branch
            do j = 1, size(lines(i)%child)
                if (len(lines(i)%child(j)%string) == 0) then
                    row = row // branch_link
                    cycle
                end if
                row = row // lines(i)%child(j)%string
            end do
            format = format // new_line('A') // row
        end do
    end function
end module formatter
