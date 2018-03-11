program easy_tree
    use csv_parser
    implicit none
    character(5), parameter :: csv = "a,b,c"
    call main(csv)
contains
    subroutine main(csv)
        type(child_element), allocatable, dimension(:) :: parsed
        type(string_element), allocatable, dimension(:) :: child
        character(*), intent(in) :: csv
        integer :: i, j
        parsed = parse_csv(csv)
        print *, parsed(1)%child(2)%string
    end subroutine
end program easy_tree
