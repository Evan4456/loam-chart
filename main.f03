program main
    use zoneCalc
    implicit none

    real :: clay, sand
    real, dimension(2) :: interceptPoint


    write(*,*) "Clay (%) | Sand (%)"
    read(*,*) clay, sand

    call assignZones()

    ! Input clay and sand %
    ! -> Get coords of intercept
    call intercept(clay, sand, interceptPoint)
    write(*,*) interceptPoint

    ! -- Need working hull calc algo
    ! --> Test on existing zones set up in zoneCalc
    ! --> Run some tests confirming that it works

    ! Find nearest points to intercept point
    ! -> Run hull calc on zones containing those points to see if each of said zones
    !    contains the intercept point
    ! -> Return zone that intercept point is inside   
end