module zoneCalc
    implicit none
    !       B
    !    >     >
    ! A     <     C 

    real, dimension(2) :: vertA = (/0.0, 0.0/)
    real, dimension(2) :: vertB = (/5.0, (10.0*sqrt(3.0)/2.0)/)
    real, dimension(2) :: vertC = (/10.0, 0.0/)
    real :: slopeAB = (10.0*sqrt(3.0)/2.0) / 5, slopeBC = -(10.0*sqrt(3.0)/2.0) / 5, slopeCA = 0.0

    type zone
        !            col, row
        real, dimension(:,:), allocatable :: points
        character(15) :: name
    end type

    ! ----------------------------------------------------------- !
    type(zone) :: z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12
    ! ----------------------------------------------------------- !

    contains
    subroutine assignZones()
        implicit none
        z1%name = "sand"
        allocate(z1%points(2,3))
        z1%points = reshape((/vertA, (/0.5, 0.866/), (/1.5,0.0/)/), shape(z1%points))

        z2%name = "loamy sand"
        allocate(z2%points(2,4))
        z2%points = reshape((/(/0.5, 0.866/), (/0.75, 1.299/), (/1.5, 0.0/), (/3.0, 0.0/)/), shape(z2%points))

  
        z3%name = "sandy loam"
        allocate(z3%points(2,7))
        z3%points = reshape((/(/0.75, 1.299/), (/1.0, 1.732/), (/3.0, 0.0/), (/3.75, 2.0/), (/4.875, 0.75/), (/5.0, 0.0/), (/5.75, 0.75/)/), shape(z3%points))


        !eg.
        write(*,*) z2%name
        write(*,*) z2%points
    end subroutine
    real function quickhull()

        quickhull = 0.0                          
    end function quickhull

end module