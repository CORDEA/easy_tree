module file_reader
    use element
    implicit none
contains
    function read_file(path)
        character(*), intent(in) :: path
        integer, parameter :: number = 10
        type(string_element), allocatable, dimension(:) :: read_file
        character(128), allocatable :: line
        character(:), allocatable :: trimmed
        integer :: count, i
        line = ""
        trimmed = ""
        count = 0
        i = 1

        open(number, file=path, status='old')
        do
            read(number, *, end=100)
            count = count + 1
        end do
100     close(number)

        allocate(read_file(count))
        open(number, file=path, status='old')
        do
            read(number, "(A)", end=200)  line
            trimmed = trim(line)
            read_file(i)%string = trimmed
            i = i + 1
        end do
200     close(number)
    end function
end module
