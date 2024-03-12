program main

    implicit none

    real :: clay, sand, silt

    write(*,*) "Clay (%) | Sand (%) | Silt(%)"
    read(*,*) clay, sand, silt

    if (clay + sand + silt > 100) then
        call exit(1)
    end if

    ! if statement instead of good code

    if ((clay >= 40).and.(sand <= 45).and.(silt < 40)) then 
        write(*,*) "Clay"
    else if ((clay >= 35).and.(sand > 45)) then 
        write(*,*) "Sandy Clay"
    else if ((clay >= 40).and.(silt >= 40)) then 
        write(*,*) "Silty Clay"
    else if(((clay >= 27).and.(clay < 40)).and.((sand > 20).and.(sand <= 45))) then
        write(*,*) "Clay Loam"
    else if(((clay >= 20).and.(clay < 35)).and.(silt < 28).and.(sand > 45)) then
        write(*,*) "Sandy Clay Loam"
    else if(((clay >= 27).and.(clay < 40)).and.(sand  <= 20)) then
        write(*,*) "Silty Clay Loam"
    else if (((silt >= 28).and.(silt < 50)).and.((clay >= 7).and.(clay < 27)).and.(sand <=52)) then 
        write(*,*) "Loam"
    else if (( ( (silt >= 50).and.(silt < 80) ) .and. (clay < 12)).or.( ( (clay >= 12).and.(clay < 27) ) .and. ( silt >= 50) )) then
        write(*,*) "Silt Loam"
    else if (((sand < 90).and.(sand >= 70)).and.(clay < 15).and.(silt < 30)) then 
        write(*,*) "Loamy Sand"
    else if ((sand >= 90).and.(clay <= 10)) then
        write(*,*) "Sand"
    else if ((silt >= 80).and.(clay < 12)) then 
        write(*,*) "Silt"
    else
        write(*,*) "Sandy Loam"
    end if

end