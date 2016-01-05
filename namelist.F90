Program my_nml_prog

  implicit none

  logical, parameter :: io_on = .true.
  !logical, parameter :: io_on, parameter = .false.
  !logical, parameter :: debug_on = .true.
  logical, parameter :: debug_on = .false.
  integer, parameter :: max_lines = 255, char_len=255

  integer :: a, b, c, ierr, nml_in, i
  character(len=char_len) :: nl_string, msg
  character(len=char_len), dimension(max_lines) :: nl_buffer

  namelist /my_nml1/ a, b, c
  namelist /my_nml2/ a, b, c

  nl_string = ''
  nl_buffer(:) = ''
  open(unit=nml_in, file="namelist.nml", action='read', access='stream',      &
       form='unformatted', position='rewind', iostat=ierr, iomsg=msg)
  read(nml_in, iostat=ierr, iomsg=msg) nl_string
  close(nml_in)
  call string_to_buffer(nl_string, nl_buffer)
  if (debug_on) then
    print*, "string version of namelist"
    print*, "---"
    print*, nl_string
    print*, ""
    print*, "no carriage returns, array of strings"
    print*, "---"
    do i=1,max_lines
      if (len_trim(nl_buffer(i)).gt.0) print*, trim(nl_buffer(i))
    end do
    print*, ""
  end if

  ! Read namelist from string containing many namelists
  a = 0
  b = 0
  c = 0
  read(nl_string, nml=my_nml1, iostat=ierr, iomsg=msg)
  call print_err(ierr, msg, header="Read my_nml1")
  write(*,my_nml1)

  ! Read namelist from array of strings, each containing single namelist
  a = 0
  b = 0
  c = 0
  ierr = 1
  i = 1
  do while ((ierr.ne.0).and.(i.le.max_lines))
    read(nl_buffer(i), nml=my_nml2, iostat=ierr, iomsg=msg)
    i=i+1
  end do
  call print_err(ierr, msg, header="Read my_nml2")
  write(*,my_nml2)

contains

  subroutine print_err(ierr, msg, header)

    integer,          intent(in) :: ierr
    character(len=*), intent(in) :: msg
    character(len=*), optional, intent(in) :: header

    if (io_on.and.(ierr.ne.0)) then
      if (present(header)) then
        print*, trim(header)
        print*, "---"
      end if
      print*, "status code: ", ierr
      print*, "err message: ", trim(msg)
    end if

  end subroutine print_err

  subroutine string_to_buffer(str_in, buf_out)

    character(len=char_len), intent(in) :: str_in
    character(len=char_len), dimension(max_lines), intent(inout) :: buf_out

    character(len=char_len) :: str_tmp
    integer :: old_pos, nl_cnt, i, j

    ! each namelist in it's own element of buf_tmp
    old_pos = 1
    nl_cnt = 1
    do i=1,len_trim(str_in)-1
      if (str_in(i:i+1).eq.'/' // achar(10)) then
        buf_out(nl_cnt) = str_in(old_pos:i)
        nl_cnt = nl_cnt+1
        old_pos=i+2
      end if
    end do

    ! Replace carriage returns with ' '
    do j=1,nl_cnt-1
      str_tmp = buf_out(j)
      do i=1,len_trim(str_tmp)
        if (str_tmp(i:i).eq.achar(10)) then
            str_tmp(i:i) = ' '
        end if
      end do
      buf_out(j) = str_tmp
    end do

  end subroutine string_to_buffer

End Program my_nml_prog
