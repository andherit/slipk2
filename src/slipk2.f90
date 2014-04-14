! This program generates a k2 dislocation distribution based
! on a summation of asperities, with a fractal distribution
! of the radius (Zeng et al, bssa, 1994).
! An input file is needed. The order of the parameter inside
! this file is not important. See the exemple slip.inp
! (Messina like source ...)
! Usage :
! slipk2 input=filename
! The ouput is created in a tmp directory (you need to create it).
! The p.tmp file is the probability fonction on the fault and
! the d.tmp is the dislocation. The normalization may be done
! on a stress drop basis or on a total moment basis.
! If the gaussian number in the input file is null, The code
! will expect an existing p.tmp file.
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################
! 14/04/07 Version 1.0 (Fortran90 version)
! 14/04/14 Version 1.1 The parser module is removed from
!                      this archive to its own one (forparse)
!                      Signature and CC added.
!######################################################
program slipk2
use inout
use makepdf
use pdf2slip
  implicit none

  integer :: m,n,ng,seed,i,na,sct
  real*4, dimension(:,:), allocatable :: gauss
  real*4, dimension(:), allocatable :: fp
  real*4 :: dx,sd,moment,mu,rmin,rmax,sd1
      character*1 dvd

! If you want to change to divider character (for example ' ')
! for the gaussian values, change the following variable
  dvd=';'
  call parsefile(m,n,dx,sd,ng,gauss,mu,na,rmin,rmax,seed,moment,sct,dvd)
  allocate(fp(m*n))
  if (ng /= 0) then
     write(0,'(a,$)') 'computing pdf ...'
     call faultpdf(gauss,ng,fp,m,n,dx)
     open(10,file='tmp/p.tmp',form='unformatted',access='direct',recl=4*m*n)
     write(10,rec=1) (fp(i),i=1,m*n)
  else
     write(0,'(a,$)') 'loading pdf ...'
     open(10,file='tmp/p.tmp',form='unformatted',access='direct',recl=4*m*n,status='old')
     read(10,rec=1) (fp(i),i=1,m*n)
  endif
  close(10)
  write(0,*) ' done!'
  write(0,'(a,$)') 'computing slip ...'
  call pdfslipfract(fp,m,n,dx,seed,moment,sd,mu,na,rmin,rmax,1,sct)
  write(0,*) ' done!'
  write(0,*) 'writing ...'
  open(10,file='tmp/d.tmp',form='unformatted',access='direct',recl=4*m*n)
  write(10,rec=1) (fp(i),i=1,m*n)
  close(10)
end program slipk2
