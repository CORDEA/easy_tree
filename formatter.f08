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
    function format(lines, max)
        type(child_element), allocatable, dimension(:) :: lines
        integer, intent(in) :: max
        integer, dimension(max) :: contents
        integer, allocatable, dimension(:) :: actual
        integer :: i, j
        character(:), allocatable :: format, row
        integer :: indent, prev_indent, next_indent
        format = root
        prev_indent = 0

        allocate(actual(max))
        contents = find_contents(lines, max)
        actual = 0

        do i = 1, size(lines)
            indent = size(lines(i)%child)
            if (size(lines) > i) then
                next_indent = size(lines(i + 1)%child)
            else
                next_indent = 0
            end if
            if (contents(1) <= actual(1)) then
                row = space
            else if (contents(1) == actual(1) + 1) then
                row = branch_end // branch_link // branch_link
                actual(1) = actual(1) + 1
            else if (indent == 1) then
                row = branch // branch_link // branch_link
            else if ( .not. prev_indent == 0 .and. prev_indent <= indent) then
                row = branch_pipe
            else if (i == size(lines)) then
                row = branch_end
            end if
            do j = 1, indent
                if ( .not. len(lines(i)%child(j)%string) == 0) then
                    row = row // ' ' // lines(i)%child(j)%string
                    actual(j) = actual(j) + 1
                    cycle
                end if
                if (prev_indent <= indent) then
                    if (j == indent - 1) then
                        if (should_show_end(contents, actual, indent, next_indent, j)) then
                            row = row // branch_end // branch_link // branch_link
                        else
                            row = row // branch // branch_link // branch_link
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

    function should_show_end(contents, actual, indent, next_indent, index)
        integer, dimension(:), intent(in) :: contents, actual
        integer :: indent, next_indent, index
        logical(1) :: should_show_end
        should_show_end = .false.
        if ((next_indent > indent .and. contents(index) > actual(index)) &
            & .or. indent > next_indent &
            & .or. contents(index) == (actual(index) + 1)) then
            should_show_end = .true.
        end if
    end function

    function max_indent(lines)
        type(child_element), intent(in) :: lines(:)
        integer :: i, max_indent

        max_indent = 0
        do i = 1, ubound(lines, 1)
            if (max_indent < size(lines(i)%child)) then
                max_indent = size(lines(i)%child)
            end if
        end do
    end function

    function find_contents(lines, max)
        type(child_element), intent(in) :: lines(:)
        integer :: i, j, max
        integer, dimension(max) :: find_contents

        find_contents = 0
        do i = 1, ubound(lines, 1)
            do j = 1, size(lines(i)%child)
                if ( .not. len(lines(i)%child(j)%string) == 0) then
                    find_contents(j) = find_contents(j) + 1
                end if
            end do
        end do
    end function
end module formatter
