program main
    use zoneCalc
    use polygons
    implicit none

    type(point) :: interceptPoint
    integer :: hold
    real :: clay, sand, clayTransform, sandTransform

    write(*,*) "Clay (%) | Sand (%)"
    read(*,*) clay, sand

    call assignZones()

    ! ! Input clay and sand %
    ! ! -> Get coords of intercept
    ! call getIntercept(clay, sand, interceptPoint)
    clayTransform = clay / 10.0
    sandTransform = 2 * (10 - (sand / 10.0))
    interceptPoint = getIntercept(0.0, 1.0, (-clayTransform), (-2.0), (-1.0), sandTransform)
    write(*,*) "Intercept Point: ", interceptPoint

    hold = cast(interceptPoint)

    call cleanZones()
end