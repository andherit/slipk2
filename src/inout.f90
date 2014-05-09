module inout
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################
! The parser module parser is located here :
! https://github.com/andherit/forparse
use forparse
implicit none

contains

!*********************************************************
function fparsearrf(nit,keyword,vvalue,nv,sep)
implicit none

      integer :: nv,nit
      real*4, dimension(nv) :: vvalue
      character*(*) :: keyword
      character*1 :: sep
      logical :: fparsearrf

      integer :: k,i,cp
      character*300 :: line,linecrt
      logical :: dummy

      fparsearrf=.false.
      rewind(nit)
      do while (.not.fparsearrf)
         read(nit,'(a)',end=10) line
         if (linematchkeyword(line,keyword,k)) then
            linecrt=line(k:lnblnk(line))
            line=linecrt
            i=0
            cp=scan(line,sep)
            do while (.not.cp.eq.0)
               i=i+1
               dummy=string2real(line(1:cp-1),vvalue(i))
               linecrt=line(cp+1:lnblnk(line))
               line=linecrt
               cp=scan(line,sep)
            enddo
            i=i+1
            dummy=string2real(line(1:lnblnk(line)),vvalue(i))
            fparsearrf=.true.
         endif
      enddo
10    return
end function fparsearrf
!*********************************************************
subroutine parsefile(m,n,dx,sd,ng,gauss,mu,na,rmin,rmax,seed,m0,sct,dvd)
  implicit none

  integer m,n,ng,na,seed,sct
  real*4 dx,sd,mu,rmin,rmax,m0
  real*4, dimension(:,:), allocatable :: gauss
  character*1 dvd

  integer ::i,j
  real*4 :: val4(4)
  character*4 :: s4
  character*50 :: fileparam

  integer :: nit

  if (parse_arg('input',fileparam) /= PARSE_OK) stop 'fault file name missing'
  nit=20
  open(nit,file=fileparam)
  if (parse_arg('nx',m,nit) /= PARSE_OK) stop 'nx missing/syntax error'
  if (parse_arg('ny',n,nit) /= PARSE_OK) stop 'ny missing/syntax error'
  if (parse_arg('dx',dx,nit) /= PARSE_OK) stop 'dx missing/syntax error'
  if (parse_arg('stress drop',sd,nit) == PARSE_UNKNOWN_KEYWORD) then
     if (parse_arg('moment',m0,nit) == PARSE_UNKNOWN_KEYWORD) then
        stop 'scaling parameters are missing/syntax error'
     else
        if (parse_arg('moment',m0,nit) == PARSE_OK) then
           sct=0
        else
           stop 'moment syntax error'
        endif
     endif
  else
     if (parse_arg('moment',m0,nit) == PARSE_UNKNOWN_KEYWORD) then
        if (parse_arg('stress drop',sd,nit) == PARSE_OK) then
           sct=1
        else
           stop 'stress drop syntax error'
        endif
     else
        stop 'both scaling parameters are exclusive'
     endif
  endif
  if (parse_arg('rigidity',mu,nit) /= PARSE_OK) stop 'rigidity missing/syntax error'
  if (parse_arg('nb cracks',na,nit) /= PARSE_OK) stop 'crak number missing/syntax error'
  if (parse_arg('rmin',rmin,nit) /= PARSE_OK) stop 'minimum radius missing/syntax error'
  if (parse_arg('rmax',rmax,nit) /= PARSE_OK) stop 'maximum radius missing/syntax error'
  if (parse_arg('seed',seed,nit) /= PARSE_OK) stop 'seed number missing/syntax error'
  if (parse_arg('gaussian number',ng,nit) /= PARSE_OK) stop 'ng missing/syntax error'
  if (ng == 0) then
     close(nit)
     return
  endif
  allocate(gauss(ng,4))
  do i=1,ng
! safe up to 99. need to be improved with a log calculation
     if (i < 10) then
        s4='[ ]'
        write(s4(2:2),'(i1.1)') i
     else
        s4='[  ]'
        write(s4(2:3),'(i2.2)') i
     endif
     if (.not.fparsearrf(nit,s4,val4,4,dvd)) then
        write(0,*) 'missing info for gaussian number#',i
        close(nit)
        stop 
     else
        do j=1,4
           gauss(i,j)=val4(j)
        enddo
     endif
  enddo
  close(nit)
  return
end subroutine parsefile
!############################################################
end module inout
