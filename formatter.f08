module formatter
    use element
    implicit none
    character(1), parameter :: root = "."
    character(4), parameter :: space = "    "
    character(3), parameter :: branch = "├"
    character(3), parameter :: branch_end = "└"
    character(8), parameter :: branch_pipe = "│   "
    character(3), parameter :: branch_link = "─"
contains
    function format(lines)
        type(child_element), allocatable, dimension(:) :: lines
        character(:), allocatable :: format, row
        integer :: i, j
        format = root
        do i = 1, size(lines)
            if (i == size(lines)) then
                row = branch_end
            else
                row = branch
            end if
            do j = 1, size(lines(i)%child)
                if (len(lines(i)%child(j)%string) == 0) then
                    row = row // branch_link
                    cycle
                end if
                row = row // ' ' // lines(i)%child(j)%string
            end do
            format = format // new_line('A') // row
        end do
    end function
end module formatter
