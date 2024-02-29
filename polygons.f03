module polygons
    implicit none

    type point
        real :: x, y
    end type

    type polygon
        character(15) :: name
        type(point), dimension(:), allocatable :: vertices
    end type

    contains
    function create_polygon(name, vertices)
        type(polygon) :: create_polygon
        character(:), allocatable, intent(in) :: name
        type(point), dimension(:), allocatable, intent(in) :: vertices

        integer :: t, b
        t = size(vertices, 1)

        create_polygon%name = name
        allocate(create_polygon%vertices(t))
        create_polygon%vertices = vertices
    end function create_polygon

    subroutine free_polygon(poly)
        type(polygon), intent(inout) :: poly
        
        deallocate(poly%vertices)
    end subroutine
end module