module csv_parser
    use element
    implicit none
contains
    function parse_csv(lines)
        type(string_element), allocatable, dimension(:) :: lines
        type(child_element), allocatable, dimension(:) :: parse_csv
        integer :: i
        allocate(parse_csv(size(lines)))
        do i = 1, size(lines)
            parse_csv(i)%child = split(lines(i)%string)
        end do
    end function

    function split(line)
        character(*), intent(in) :: line
        character(:), allocatable :: column
        character(1), parameter :: comma = ','
        type(string_element), allocatable, dimension(:) :: split
        integer :: i, j
        column = ''
        j = 1
        allocate(split(count(line, comma) + 1))
        do i = 1, len(line)
            if (line(i:i) == comma) then
                split(j)%string = column
                column = ''
                j = j + 1
                cycle
            end if
            column = column // line(i:i)
            if (i == len(line)) then
                split(j)%string = column
            end if
        end do
    end function

    function count(str, chr)
        character(*), intent(in) :: str, chr
        integer :: i, j, count
        i = 1
        count = 0
        do
            j = index(str(i:), chr)
            if (j == 0) exit
            count = count + 1
            i = i + j
        end do
    end function
end module csv_parser
