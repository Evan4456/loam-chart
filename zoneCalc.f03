module zoneCalc
    implicit none
    !       B
    !    >     >
    ! A     <     C 

    real, dimension(2) :: vertA = (/0.0, 0.0/)
    real, dimension(2) :: vertB = (/5.0, 10.0/)
    real, dimension(2) :: vertC = (/10.0, 0.0/)
    real :: slopeAB = 2.0, slopeBC = -2.0, slopeCA = 0.0

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
        z1%points = reshape((/vertA, (/0.5, 1.0/), (/1.5,0.0/)/), shape(z1%points))

        z2%name = "loamy sand"
        allocate(z2%points(2,4))
        z2%points = reshape((/(/0.5, 1.0/), (/0.75, 1.5/), (/1.5, 0.0/), (/3.0, 0.0/)/), shape(z2%points))
  
        z3%name = "sandy loam"
        allocate(z3%points(2,7))
        z3%points = reshape((/(/0.75, 1.5/), (/1.0, 2.0/), (/3.0, 0.0/), (/3.75, 2.0/), (/4.375, 0.75/), (/5.0, 0.0/), (/5.375, 0.75/)/), shape(z3%points))

        z4%name = "loam"
        allocate(z4%points(2,5))
        z4%points = reshape((/(/3.75, 2.0/), (/4.125, 2.75/), (/4.375, 0.75/), (/5.375, 0.75/), (/6.5, 3.0/)/), shape(z4%points))
        
        z5%name = "silt loam"
        allocate(z4%points(2,6))
        z5%points = reshape((/(/5.0, 0.0/), (/6.5, 3.0/), (/8.0, 0.0/), (/8.625, 2.75/), (/9.375, 1.0/)/), shape(z5%points))

        z6%name = "silt"
        allocate(z6%points(2,4))
        z6%points = reshape((/(/8.0, 0.0/), (/8.75, 1.25/), (/9.5, 1.25/), vertC/), shape(z6%points))

        z7%name = "sandy clay loam"
        allocate(z7%points(2,5))
        z7%points = reshape((/(/1.0, 2.0/), (/1.5, 3.0/), (/3.75, 2.0/), (/3.75, 3.5/), (/4.125, 2.75/)/), shape(z7%points))

        z8%name = "clay loam"
        allocate(z8%points(2,4))
        z8%points = reshape((/(/3.5, 4.0/), (/4.125, 2.75/), (/6.0, 4.0/), (/6.75, 2.75/)/), shape(z8%points))

        z9%name = "silty clay loam"
        allocate(z9%points(2,4))
        z9%points = reshape((/(/6.0, 4.0/), (/6.75, 2.75/), (/8.0, 4.0/), (/8.75, 2.75/)/), shape(z9%points))

        z10%name = "sandy clay"
        allocate(z10%points(2,3))
        z10%points = reshape((/(/1.75, 3.5/), (/2.75, 5.5/), (/3.75, 3.5/)/), shape(z10%points))

        z11%name = "silty clay"
        allocate(z11%points(2,3))
        z11%points = reshape((/(/6.0, 4.0/), (/7.0, 6.0/), (/8.0, 4.0/)/), shape(z11%points))

        z12%name = "clay"
        allocate(z12%points(2,5))
        z12%points = reshape((/(/2.75, 5.5/), (/3.5, 4.0/), vertB, (/6.0, 4.0/), (/7.0, 6.0/)/), shape(z12%points))
    end subroutine
    real function quickhull()

        quickhull = 0.0                          
    end function quickhull

end module