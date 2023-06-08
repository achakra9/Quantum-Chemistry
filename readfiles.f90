module readfiles
    contains
        subroutine read_inp(io,inp,fileName,system,method,calculation)
            use, intrinsic :: iso_fortran_env, only: dp => real64
            implicit none
            character(len=50), intent(in) :: fileName
            integer, intent(inout) :: system(7)
            character(len=10), intent(inout) :: method
            integer, intent(inout):: calculation(4)
            integer :: ierr
            integer, intent(in) :: io, inp
            integer :: nelec, ncore, nvfrz, nocca, noccb, mult, norb
            integer :: iconv, maxcc, maxccl, maxeom
            character(len=10):: names
            integer :: i


            open(unit=inp,file=fileName,status='old',iostat=ierr)
            if (ierr .ne. 0) then
                write(io,*) "Input file not found"
                stop
            end if
            read(inp,*)names, method
            do i = 1, 7
                read(inp,*)names, system(i)
            end do
            do i = 1,4
                read(inp, *)names, calculation(i)
            end do
            close(unit=inp)
            end subroutine read_inp
end module readfiles
