program main
    use zoneCalc
    use polygons
    implicit none

    real :: clay, sand
    real, dimension(2) :: interceptPoint

    ! write(*,*) zone1%name
    ! write(*,*) zone1%vertices
    ! write(*,*) zone1%conns
    ! write(*,*) "Clay (%) | Sand (%)"
    ! read(*,*) clay, sand

    call assignZones()

    ! ! Input clay and sand %
    ! ! -> Get coords of intercept
    ! call intercept(clay, sand, interceptPoint)
    ! write(*,*) interceptPoint

    call cleanZones()
end