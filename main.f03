program main
    use zoneCalc
    use polygons
    implicit none

    type(point) :: interceptPoint
    type(polygon) :: correctZone
    integer :: hold
    real :: clay, sand, clayTransform, sandTransform

    write(*,*) "Clay (%) | Sand (%)"
    read(*,*) clay, sand

    if (clay + sand > 100) call exit(1)
    call assignZones()

    ! Input clay and sand %
    ! -> Get coords of intercept
    clayTransform = clay / 10.0
    sandTransform = 2 * (10 - (sand / 10.0))
    interceptPoint = getIntercept(0.0, 1.0, (-clayTransform), (-2.0), (-1.0), sandTransform)
    write(*,*) "Intercept Point: ", interceptPoint

    correctZone = cast(interceptPoint)
    write(*,*) "Soil texture: ", correctZone%name

    call cleanZones()
end