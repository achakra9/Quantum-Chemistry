module integrals
    contains
        subroutine load_1body(io,norb,e1int)
            ! reads onebody molecular integrals
            use, intrinsic :: iso_fortran_env, only: dp => real64
            implicit none
            integer, intent(in) :: norb
            integer :: ierr
            integer, intent(in) :: io
            integer :: i, j, one_indx, indx
            real(dp) :: e_onebody
            real(dp), allocatable, intent(out) :: e1int(:,:)


            one_indx = 231
            open(unit=one_indx,file='onebody.inp',status='old',iostat=ierr)
            if (ierr .ne. 0) then
                write(io,*) "onebody file not found"
                stop
            end if

            allocate(e1int(norb,norb))

            do i = 1, norb
                do j=1,i
                    read(one_indx,*)e_onebody, indx
                    e1int(i,j) = e_onebody
                    e1int(j,i) = e_onebody
                end do
            end do

            write(io,*)'onebody integrals read successfully.'

            close(unit=one_indx)
        end subroutine load_1body

        subroutine load_2body(io,norb,e2int,e_repul)
            ! reads twobody molecular integrals
            use, intrinsic :: iso_fortran_env, only: dp => real64
            implicit none
            integer, intent(in) :: norb, io
            real(dp) :: e_twobody
            real(dp), allocatable, intent(out) :: e2int(:,:,:,:)
            real(dp), intent(out) :: e_repul
            integer :: i,j,two_indx, indx, ierr,a,b
            

            two_indx = 241
            open(unit=two_indx,file='twobody.inp',status='old',iostat=ierr)
            if (ierr .ne. 0) then
                write(io,*) "Twobody file not found."
                stop
            end if

            allocate(e2int(norb,norb,norb,norb))

            do
                read(two_indx,*)i,a,j,b,e_twobody

                if (i+a+j+b == 0) then
                    e_repul = e_twobody
                else
                    e2int(i,j,a,b) = e_twobody
                end if
            end do

            write(io,*)'twobody integrals read successfully'
            close(two_indx)
        end subroutine load_2body

        subroutine symmetry(io,norb,irrep,pg)
            !reads the symmetry information
            use, intrinsic :: iso_fortran_env, only: dp => real64
            implicit none
            integer, intent(in) :: io, norb
            integer, allocatable, intent(out) :: irrep(:)
            character(len=3), intent(out) :: pg
            integer :: i,j,ierr, indx
            character(len=3) :: label

            
            indx = 241
            open(unit=indx,file='symm_gamess.out',status='old',iostat=ierr)
            if (ierr .ne. 0) then
                write(io,*) "symmetry file not found."
                stop
            end if

            allocate(irrep(norb))
            read(indx,*)pg
            do i=1,norb
                read(indx,*)irrep(i), label
            end do

            write(io,*)'symmetry file read successfully.'
            write(io,*)'The symmetry used:', pg
            close(indx)
        end subroutine symmetry

end module integrals
