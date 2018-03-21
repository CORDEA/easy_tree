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
        integer :: indent, prev_indent, next_indent
        format = root
        prev_indent = 0
        do i = 1, size(lines)
            indent = size(lines(i)%child)
            if (size(lines) > i) then
                next_indent = size(lines(i + 1)%child)
            else
                next_indent = 0
            end if
            if ( .not. prev_indent == 0 .and. prev_indent <= indent) then
                row = branch_pipe
            else if (i == size(lines)) then
                row = branch_end
            else
                row = branch // branch_link // branch_link
            end if
            do j = 1, indent
                if ( .not. len(lines(i)%child(j)%string) == 0) then
                    row = row // ' ' // lines(i)%child(j)%string
                    cycle
                end if
                if (prev_indent <= indent) then
                    if (j == indent - 1) then
                        if (indent == next_indent) then
                            row = row // branch // branch_link // branch_link
                        else
                            row = row // branch_end // branch_link // branch_link
                        end if
                    else
                        row = row // space
                    end if
                else
                    row = row // branch_link // branch_link // branch_link
                end if
            end do
            format = format // new_line('A') // row
            prev_indent = indent
        end do
    end function
end module formatter
