!    This is the main file that reads the input file and does the
!    calculations asked

      program main
          use, intrinsic :: iso_fortran_env, only: dp => real64
          ! integer, parameter :: dp = selected_real_kind(15)
          use readfiles
          implicit none
          integer :: dt(8)
          real(dp) :: a,b
          integer :: num_args, idx, ierr
          character(len=50), dimension(:), allocatable :: args
          integer, parameter :: inp = 211 ! open input file
          integer, parameter :: io = 221  ! open the output file
          character(len=50) :: f_input, f_output
          integer :: system(7), calculation(4)
          character(len=10) :: method


          call date_and_time(values=dt)


          ! Read the command line arguments
          num_args = command_argument_count()
          allocate(args(num_args))

          do idx=1, num_args
              call get_command_argument(idx,args(idx))
          end do

          if (args(1) .eq. '-h') then
              write(*,*)" oscc inputFile outputFile"
              stop
          else
              write(*,*)"Number of args:", num_args
              f_input = args(1)
              f_output = args(2)
          end if

          system = 0
          calculation = 0
          ! open the input file
          call read_inp(io,inp,f_input,system,method,calculation)

          ! open output file for writing
          open(unit=io,file=f_output,status='new',iostat=ierr)
          if (ierr .ne. 0) then
              write(*,*)"Error in creating output file"
              stop
          end if
          write(io,'(A24,2x,I2,A1,I2,A1,I4,1x,A2,1x,I2,A1,I2,A1,I2)')"OSCC program started on:",dt(3),'-',dt(2),'-', &
                     dt(1),' at ',dt(5), ':', dt(6), ':',dt(7)
          write(io,'(A21)')"====================="
          write(io,'(A12,2x,A7)')'Calculation:', method
          write(io,'(A21)')"====================="
          write(io,*)
          write(io,'(A17)')"System Parameters"
          write(io,'(A17)')"-----------------"
          write(io,'(A5,2x,I6)')'Nelec',system(1)
          write(io,'(A5,2x,I6)')'Ncore',system(2)
          write(io,'(A5,2x,I6)')'Nvfrz',system(3)
          write(io,'(A5,2x,I6)')'nocca',system(4)
          write(io,'(A5,2x,I6)')'noccb',system(5)
          write(io,'(A5,2x,I6)')'norb ',system(6)
          write(io,'(A5,2x,I6)')'mult ',system(7)
          write(io,*)
          write(io,'(A22)')"Calculation Parameters"
          write(io,'(A22)')"----------------------"
          write(io,'(A6,2x,I6)')'iconv ',calculation(1)
          write(io,'(A6,2x,I6)')'maxcc ',calculation(1)
          write(io,'(A6,2x,I6)')'maxccl',calculation(1)
          write(io,'(A6,2x,I6)')'maxeom',calculation(1)
          write(io,*)

          call date_and_time(values=dt)
          write(io,'(A26,2x,I2,A1,I2,A1,I4,1x,A2,1x,I2,A1,I2,A1,I2)')"OSCC program completed on:",dt(3),'-',dt(2),'-', &
                     dt(1),' at ',dt(5), ':', dt(6), ':',dt(7)
          close(io)
      end program main
