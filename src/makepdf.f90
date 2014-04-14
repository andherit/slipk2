module makepdf
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################

contains

!***********************************************************
subroutine faultpdf(gauss,ng,fp,m,n,dx)
  implicit none
      
!  read a list of ng 2D gaussian defined by their
!  center (xi,yi), their sigma (sigi) iand their weight (wgti)
!  to construct on the fault plane, a pdf compatible with
!  the subroutine pdfslipfract.
!  The fault plane is defined by its nodes through an array
!  fp(m,n) of dimension (m-1)dx*(n-1)dx

   integer :: m,n,ng
   real*4, dimension(ng,4) :: gauss
   real*4, dimension(m*n) :: fp
   real*4 :: dx

   real*4, parameter :: pi=acos(-1.)
   integer :: i,j,k,idx
   real*4 :: xl,yl,dst2,unormgauss,fpint

   fp=0.
   fpint=0.
   do i=1,m
      xl=(i-1)*dx
      do j=1,n
         yl=(j-1)*dx
         idx=j+(i-1)*n
         do k=1,ng
            dst2=(yl-gauss(k,2))**2.+(xl-gauss(k,1))**2.
            unormgauss=exp(-dst2/2./gauss(k,3)**2.)
            fp(idx)=fp(idx)+unormgauss*gauss(k,4)
         enddo
         fpint=fpint+fp(idx)
      enddo
   enddo
   do i=1,m*n
      fp(i)=fp(i)/fpint
   enddo  
   return
end subroutine faultpdf
end module makepdf
