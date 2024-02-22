module zoneCalc
    use polygons
    implicit none

    real, dimension(2) :: vertA = (/0.0, 0.0/)
    real, dimension(2) :: vertB = (/5.0, 10.0/)
    real, dimension(2) :: vertC = (/10.0, 0.0/)
    real :: slopeSilt = 2.0, slopeSand = -2.0, slopeClay = 0.0

    ! ----------------------------------------------------------- !
    type(polygon) :: z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12
    type(polygon), dimension(12) :: zoneList
    ! ----------------------------------------------------------- !

    contains
    subroutine assignZones()
        type(point), dimension(:), allocatable :: points
        integer, dimension(:,:), allocatable :: conns
        character(:), allocatable :: name

        name = "sand"
        allocate(points(3))
        allocate(conns(2,3))
        points%x = (/0.0, 0.5, 1.5/)
        points%y = (/0.0, 1.0, 0.0/)
        conns = reshape((/(/1,2/), (/2,3/), (/3,1/)/), shape(conns))
        z1 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "loamy sand"
        allocate(points(4))
        allocate(conns(2,4))
        points%x = (/0.5, 0.75, 1.5, 3.0/)
        points%y = (/1.0, 1.5, 0.0, 0.0/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,3/), (/3,1/)/), shape(conns))
        z2 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "sandy loam"
        allocate(points(7))
        allocate(conns(2,7))
        points%x = (/0.75, 1.0, 3.0, 3.75, 4.375, 5.0, 5.375/)
        points%y = (/1.5, 2.0, 0.0, 2.0, 0.75, 0.0, 0.75/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/), (/7,6/), (/6,3/), (/3,1/)/), shape(conns))
        z3 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        write(*,*) z3%name
        write(*,*) z3%vertices
        write(*,*) z3%conns

        zoneList = (/z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12/)
               
        ! z4%name = "loam"
        ! allocate(z4%points(2,5))
        ! z4%points = reshape((/(/3.75, 2.0/), (/4.125, 2.75/), (/4.375, 0.75/), (/5.375, 0.75/), (/6.5, 3.0/)/), shape(z4%points))

        ! z5%name = "silt loam"
        ! allocate(z5%points(2,6))
        ! z5%points = reshape((/(/5.0, 0.0/), (/6.5, 2.75/), (/8.0, 0.0/), (/8.75, 1.5/), (/8.625, 2.75/), &
        !                     (/9.375, 1.25/)/), shape(z5%points))

        ! z6%name = "silt"
        ! allocate(z6%points(2,4))
        ! z6%points = reshape((/(/8.0, 0.0/), (/8.75, 1.25/), (/9.5, 1.25/), vertC/), shape(z6%points))

        ! z7%name = "sandy clay loam"
        ! allocate(z7%points(2,5))
        ! z7%points = reshape((/(/1.0, 2.0/), (/1.5, 3.0/), (/3.75, 2.0/), (/3.75, 3.5/), (/4.125, 2.75/)/), shape(z7%points))

        ! z8%name = "clay loam"
        ! allocate(z8%points(2,4))
        ! z8%points = reshape((/(/3.5, 4.0/), (/4.125, 2.75/), (/6.0, 4.0/), (/6.75, 2.75/)/), shape(z8%points))

        ! z9%name = "silty clay loam"
        ! allocate(z9%points(2,4))
        ! z9%points = reshape((/(/6.0, 4.0/), (/6.75, 2.75/), (/8.0, 4.0/), (/8.75, 2.75/)/), shape(z9%points))

        ! z10%name = "sandy clay"
        ! allocate(z10%points(2,3))
        ! z10%points = reshape((/(/1.75, 3.5/), (/2.75, 5.5/), (/3.75, 3.5/)/), shape(z10%points))

        ! z11%name = "silty clay"
        ! allocate(z11%points(2,3))
        ! z11%points = reshape((/(/6.0, 4.0/), (/7.0, 6.0/), (/8.0, 4.0/)/), shape(z11%points))

        ! z12%name = "clay"
        ! allocate(z12%points(2,5))
        ! z12%points = reshape((/(/2.75, 5.5/), (/3.5, 4.0/), vertB, (/6.0, 4.0/), (/7.0, 6.0/)/), shape(z12%points))
    end subroutine

    subroutine intercept(clay, sand, interceptPoint)
        real, intent(out) :: clay, sand
        real, dimension(2), intent(out) :: interceptPoint
        real :: clayTransform, sandTransform
        real :: xIntercept, yIntercept


        ! Clay - Line 1 | Sand - Line 2 !
        clayTransform = clay / 10.0
        sandTransform = 2 * (10 - (sand / 10.0))

        ! Clay: 0 = y - height
        ! Sand: 0 = -2x - y + distance
        xIntercept = ((1.0 * sandTransform) - ((-1.0) * (-clayTransform))) / ((slopeClay * (-1.0)) - (slopeSand * 1.0))
        yIntercept = ((slopeSand * (-clayTransform)) - (slopeClay * sandTransform)) / ((slopeClay * (-1.0)) - (slopeSand * 1.0))

        interceptPoint = (/xIntercept, yIntercept/)
    end subroutine

    subroutine getPotentialZones(interceptPoint, potentialZones)
        type(point), intent(in) :: interceptPoint
        integer, dimension(4), intent(inout) :: potentialZones
        integer :: i, j, jMax, zoneCount
        real :: a, b, c, cPrime, cMin, x, y
        cMin = 30

        do i = 1, 3 !12 -> size(zoneList)
            jMax = size(zoneList(i)%vertices)
            do j = 1, jMax
                a = interceptPoint%x * interceptPoint%x
                b = interceptPoint%y * interceptPoint%y
                cPrime = sqrt(a + b)

                a = zoneList(i)%vertices(j)%x * zoneList(i)%vertices(j)%x
                b = zoneList(i)%vertices(j)%y * zoneList(i)%vertices(j)%y
                c = sqrt(a + b)
                cPrime = abs(cPrime - c)

                ! Does not handle if a point is equi-distant
                if (cPrime <= cMin) then
                    cMin = cPrime
                    x = zoneList(i)%vertices(j)%x
                    y = zoneList(i)%vertices(j)%y
                    write(*,*) "Point: ", x, y
                end if
            end do
        end do
        write(*,*) "Cmin: ", + cMin

        zoneCount = 1
        do i = 1, 3
            jMax = size(zoneList(i)%vertices)
            do j = 1, jMax
                if (zoneList(i)%vertices(j)%x == x .and. zoneList(i)%vertices(j)%y == y) then
                    potentialZones(zoneCount) = i
                    zoneCount = zoneCount+1
                    write(*,*) "Test: ", zoneList(i)%vertices(j)
                end if
            end do
        end do
    end subroutine

    integer function cast(interceptPoint)
        type(point), intent(inout) :: interceptPoint
        integer :: i, j, jMax, closestPoint
        integer, dimension(4) :: potentialZones

        ! Get nearest point(s) and each zone with said point(s)
        call getPotentialZones(interceptPoint, potentialZones)

        
        ! Draw line straight out right
        ! Check if intersects with any line in currently iterated zone
        ! If odd num of intersects then inside, if even then outside


        cast = 0
    end function cast

    subroutine cleanZones()
        deallocate(z1%vertices)
        deallocate(z1%conns)

        deallocate(z2%vertices)
        deallocate(z2%conns)

        deallocate(z3%vertices)
        deallocate(z3%conns)
    end subroutine

end module