module polygons
    implicit none

    type point
        real :: x, y
    end type

    type polygon
        character(15) :: name
        type(point), dimension(:), allocatable :: vertices
        integer, dimension(:,:), allocatable :: conns
    end type

    contains
    function create_polygon(name, vertices, conns)
        type(polygon) :: create_polygon
        character(:), allocatable, intent(in) :: name
        type(point), dimension(:), allocatable, intent(in) :: vertices
        integer, dimension(:,:), allocatable, intent(in) :: conns

        integer :: t, b
        t = size(vertices, 1)
        b = size(conns, 1)

        create_polygon%name = name

        allocate(create_polygon%vertices(t), create_polygon%conns(b, 1))
        create_polygon%vertices = vertices
        create_polygon%conns = conns
    end function create_polygon

    subroutine free_polygon(poly)
        type(polygon), intent(inout) :: poly
        
        deallocate(poly%vertices, poly%conns)
    end subroutine

end module