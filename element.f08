module element
    implicit none
    type string_element
        character(:), allocatable :: string
    end type string_element
    type child_element
        type(string_element), allocatable, dimension(:) :: child
    end type child_element
contains
end module element
