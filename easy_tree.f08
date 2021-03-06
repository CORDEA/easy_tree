program easy_tree
    use csv_parser
    use file_reader
    use formatter
    implicit none
    character(12), parameter :: csv = "test.csv"
    call main(csv)
contains
    subroutine main(csv)
        type(child_element), allocatable, dimension(:) :: parsed
        type(string_element), allocatable, dimension(:) :: lines
        character(*), intent(in) :: csv
        integer :: i, j, max
        lines = read_file(csv)
        parsed = parse_csv(lines)
        max = max_indent(parsed)
        write(*, '(A)') format(parsed, max)
    end subroutine
end program easy_tree
