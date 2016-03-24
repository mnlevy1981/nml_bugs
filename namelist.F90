Program my_nml_prog

  implicit none

  !logical, parameter :: io_msg_on = .true.
  logical, parameter :: io_msg_on = .false.
  !logical, parameter :: debug_on = .true.
  logical, parameter :: debug_on = .false.
  !logical, parameter :: verbose = .true.
  logical, parameter :: verbose = .false.
  integer, parameter :: max_lines = 255, char_len=255

  integer :: a, b, c, ierr, nml_in, i, cnt, max_len
  character(len=15), dimension(2) :: d
  character(len=char_len) :: nl_string, msg, tmp_str
  character(len=char_len), dimension(max_lines) :: nl_buffer

  namelist /my_nml1/ a, b, c, d
  namelist /my_nml2/ a, b, c, d

  ! Initialize string and array to be empty
  nl_string = ''
  nl_buffer(:) = ''

  ! Read string from file
  open(unit=nml_in, file="namelist.nml", action='read', access='stream',      &
       form='unformatted', position='rewind', iostat=ierr, iomsg=msg)
  read(nml_in, iostat=ierr, iomsg=msg) nl_string
  close(nml_in)

  ! Populate array (one namelist per element; strip carriage returns)
  call string_to_buffer(nl_string, nl_buffer)

  ! Debug output: print contents of string and array
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

  ! Read first namelist from string containing many namelists
  a = 0
  b = 0
  c = 0
  d = (/'Hello', 'World'/)
  read(nl_string, nml=my_nml1, iostat=ierr, iomsg=msg)
  call print_err(ierr, msg, header="Read my_nml1")
  if (verbose.or.debug_on) &
    write(*,my_nml1)
  if (any((/a,b,c/).eq.0)) then
    write(*,"(A)") "Read from string: At least one namelist variable is still 0"
    write(*,"(17X,A)") "FAIL!"
  else
    write(*,"(A)") "Read from string: Successful namelist read!"
    write(*,"(17X,A)") "PASS!"
  end if

  ! Read second namelist from array, each element containing single namelist
  a = 0
  b = 0
  c = 0
  ierr = 1
  i = 1
  do while ((ierr.ne.0).and.(i.le.max_lines))
    tmp_str = nl_buffer(i)
    if (tmp_str(2:8).eq."my_nml2") &
      read(nl_buffer(i), nml=my_nml2, iostat=ierr, iomsg=msg)
    i=i+1
  end do
  call print_err(ierr, msg, header="Read my_nml2")
  if (verbose.or.debug_on) &
    write(*,my_nml2)
  if (any((/a,b,c/).eq.0)) then
    write(*,"(A)") "Read from array: At least one namelist variable is still 0"
    write(*,"(17X,A)") "FAIL!"
  else
    write(*,"(A)") "Read from array: Successful namelist read!"
    write(*,"(17X,A)") "PASS!"
  end if

  ! Write namelist to screen
  write(*,"(A)") "Namelist contents directly to screen"
  write(*,nml=my_nml1)

  ! Write namelist to string
  write(*,"(A)") "Namelist contents to string"
  nl_buffer(:) = ''
  write(nl_buffer,nml=my_nml1)
  cnt = 0
  max_len = 0
  do i=1,max_lines
    if (len(trim(nl_buffer(i))).ne.0) then
      max_len = max(max_len, len(trim(nl_buffer(i))))
      cnt = cnt+1
      write(*,"(A)") trim(nl_buffer(i))
    end if
  end do
  write(*, "(A,I0,A)") "Populated ", cnt, " elements of nl_buffer"
  write(*, "(A,I0,A)") "Length of longest line is ", max_len, " characters"

contains

  subroutine print_err(ierr, msg, header)

    integer,          intent(in) :: ierr
    character(len=*), intent(in) :: msg
    character(len=*), optional, intent(in) :: header

    if (io_msg_on.and.(ierr.ne.0)) then
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
