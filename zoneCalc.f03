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

        name = "loam"
        allocate(points(5))
        allocate(conns(2,5))
        points%x = (/3.75, 4.125, 4.375, 5.375, 6.5/)
        points%y = (/2.0, 2.75, 0.75, 0.75, 3.0/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/), (/7,6/)/), shape(conns)) !UPDATE
        z4 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "silt loam"
        allocate(points(6))
        allocate(conns(2,6))
        points%x = (/5.0, 6.5, 8.0, 8.75, 8.625, 9.375/) !CHECK, 8.625 shouldn't come after 8.75
        points%y = (/0.0, 2.75, 0.0, 1.5, 2.75, 1.25/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/), (/7,6/), (/1,2/)/), shape(conns)) !UPDATE
        z5 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "silt"
        allocate(points(4))
        allocate(conns(2,4))
        points%x = (/8.0, 8.75, 9.5, 10.0/)
        points%y = (/0.0, 1.25, 1.25, 0.0/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/)/), shape(conns)) !UPDATE
        z6 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "sandy clay loam"
        allocate(points(5))
        allocate(conns(2,5))
        points%x = (/1.0, 1.5, 3.75, 3.75, 4.125/) ! CHECK 3.75 and 3.75
        points%y = (/2.0, 3.0, 2.0, 3.5, 2.75/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/), (/7,6/)/), shape(conns)) !UPDATE
        z7 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "clay loam"
        allocate(points(4))
        allocate(conns(2,4))
        points%x = (/3.5, 4.125, 6.0, 6.75/) 
        points%y = (/4.0, 2.75, 4.0, 2.75/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/)/), shape(conns)) !UPDATE
        z8 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "silty clay loam"
        allocate(points(4))
        allocate(conns(2,4))
        points%x = (/6.0, 6.75, 8.0, 8.75/) 
        points%y = (/4.0, 2.75, 4.0, 2.75/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/5,7/)/), shape(conns)) !UPDATE
        z9 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "sandy clay"
        allocate(points(3))
        allocate(conns(2,3))
        points%x = (/1.75, 2.75, 3.75/) 
        points%y = (/3.5, 5.5, 3.5/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/)/), shape(conns)) !UPDATE
        z10 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "silty clay"
        allocate(points(3))
        allocate(conns(2,3))
        points%x = (/6.0, 7.0, 8.0/) 
        points%y = (/4.0, 6.0, 4.0/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/)/), shape(conns)) !UPDATE
        z11 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        name = "clay"
        allocate(points(5))
        allocate(conns(2,5))
        points%x = (/2.75, 3.5, 5.0, 6.0, 7.0/) 
        points%y = (/5.5, 4.0, 10.0, 4.0, 6.0/)
        conns = reshape((/(/1,2/), (/2,4/), (/4,5/), (/1,2/), (/1,2/)/), shape(conns)) !UPDATE
        z12 = create_polygon(name, points, conns)

        deallocate(points)
        deallocate(conns)

        ! Temp !
        write(*,*) z3%name
        write(*,*) z3%vertices
        write(*,*) z3%conns
        ! ---- !

        zoneList = (/z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12/)
    end subroutine

    type(point) function getIntercept(a1, b1, c1, a2, b2, c2)
        real, intent(in) :: a1, b1, c1, a2, b2, c2
        type(point) :: intercept

        intercept%x = ((b1 * c2) - (b2 * c1)) / ((a1 * b2) - (a2 * b1))
        intercept%y = ((a2 * c1) - (a1 * c2)) / ((a1 * b2) - (a2 * b1))

        getIntercept = intercept
    end function getIntercept

    subroutine getPotentialZones(interceptPoint, potentialZones)
        type(point), intent(in) :: interceptPoint
        integer, dimension(4), intent(inout) :: potentialZones
        integer :: i, j, jMax, zoneCount
        real :: a, b, c, cPrime, cMin, x, y
        cMin = 30

        do i = 1, size(zoneList)
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
        do i = 1, size(zoneList)
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
        integer :: i, j, k, jMax, kMax, index, connectingPoint, interceptCount
        real :: y, yPrime
        integer, dimension(4) :: potentialZones = 0
        type(point) :: zoneIntercept
        type(polygon) :: currZone

        ! Get nearest point(s) and each zone with said point(s)
        call getPotentialZones(interceptPoint, potentialZones)
        
        ! If there is a line connection in which one x value is to the right of intercept-x
        ! then the horizontal line will cross over
        ! -> Just count how many x values are to the right and that'll be how many times the 
        ! horiz line crosses (half right)
        ! -> Check that both y-values for the connection are different to confirm lines aren't parallel

        do i = 0, size(potentialZones)
            interceptCount = 0
            if (potentialZones(i) /= 0) then
                currZone = zoneList(potentialZones(i))
                jMax = size(currZone%vertices)
                do j = 0, jMax
                    if (currZone%vertices(j)%x >= interceptPoint%x) then
                        kMax = size(currZone%vertices)
                        do k=0, kMax
                            ! index = 
                        end do
                        ! index = findloc(currZone%vertices, currZone%vertices(j)%x)
                        y = currZone%vertices(index)%y
                        yPrime = currZone%conns(2, index)

                        if (y /= yPrime) then
                            interceptCount = interceptCount+1
                        end if
                    end if
                end do
            end if
            if (mod(interceptCount, 2) /= 0) exit
        end do

        write(*,*) "should be right name-> ", currZone%name

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